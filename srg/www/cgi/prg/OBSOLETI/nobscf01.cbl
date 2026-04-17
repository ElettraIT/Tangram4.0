       Identification Division.
       Program-Id.                                 nobscf01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    nobscf              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 20/05/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inserimento fatture per SDI - NOBILI        *
      *                                                                *
      *                    ___ DA IMPLEMENTARE ___                     *
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
      *    * Area di comunicazione per modulo                 "mhtml0" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/h"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *-----------------------------------------------------------*
       01  w-cgi-str.
      *        *-------------------------------------------------------*
      *        * Variabile POST da trattare                            *
      *        *-------------------------------------------------------*
           05  w-cgi-str-var.
               10  filler    occurs 1800  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Delimitatore                                          *
      *        *-------------------------------------------------------*
           05  w-cgi-str-del              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore delle stringhe da estrarre                     *
      *        *-------------------------------------------------------*
           05  w-cgi-str-fld  occurs 20   pic  x(90)                  .
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-cgi-str-ctr              pic  9(02)                  .
           05  w-cgi-str-num              pic  9(02)                  .
           05  w-cgi-str-max              pic  9(02) value 20         .
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-cgi-tip-ope              pic  x(02)                  .

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
      *        * Flag di visualizzazione testata                       *
      *        *-------------------------------------------------------*
           05  w-exe-flg-tst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-alf-pro              pic  x(14)                  .
           05  w-exe-num-pro              pic  9(07)                  .
           05  w-exe-inx-ubi              pic  9(01)                  .
           05  w-exe-inx-ubc              pic  x(01)                  .
           05  w-exe-rig-ubi              pic  x(07)                  .
           05  w-exe-prm-ub1              pic  x(07)                  .
           05  w-exe-prm-ub2              pic  x(07)                  .
           05  w-exe-prm-ub3              pic  x(07)                  .
           05  w-exe-prm-ub4              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati                       *
      *        *-------------------------------------------------------*
           05  w-exe-cop-sfn              pic  x(14)                  .
           05  w-exe-prm-ubi              pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-err-msg              pic  x(80)                  .
           05  w-exe-cla-msg              pic  x(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione codice numerico prodotto           *
      *        *-------------------------------------------------------*
           05  w-det-num-pro.
      *            *---------------------------------------------------*
      *            * Codice alfanumerico prodotto                      *
      *            *---------------------------------------------------*
               10  w-det-num-pro-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico prodotto                          *
      *            *---------------------------------------------------*
               10  w-det-num-pro-num      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella responsabili               *
      *        *-------------------------------------------------------*
           05  w-det-tbl-rsm.
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-cod-ele  occurs  12.
                   15  w-det-tbl-cod-rsp  pic  9(05)                  .
                   15  w-det-tbl-des-rsp  pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-rsm-max      pic  9(03) value 12         .
               10  w-det-tbl-rsm-ctr      pic  9(03)                  .
               10  w-det-tbl-rsm-inx      pic  9(03)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione ubicazione       *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dtl"                   .

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
           move      spaces               to   w-exe-alf-pro          .
           move      zero                 to   w-exe-num-pro          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
           move      o-pst                to   w-cgi-str-var          .
           perform   cgi-str-ext-000      thru cgi-str-ext-999        .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           move      w-cgi-str-num        to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           perform   ext-prm-pro-000      thru ext-prm-pro-999        .
      *                  *---------------------------------------------*
      *                  * Codice numerico responsabile                *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      03                   to   v-car                  .
           move      w-exe-cod-rsm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exe-cnv-rsm          .
       ext-prm-700.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione tabella responsabili           *
      *                  *---------------------------------------------*
           perform   det-tbl-rsm-000      thru det-tbl-rsm-999        .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di eventuale regolarizzazione codice prodotto  *
      *    *-----------------------------------------------------------*
       ext-prm-pro-000.
      *              *-------------------------------------------------*
      *              * Test su valore del campo in input               *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to ext-prm-pro-900.
      *              *-------------------------------------------------*
      *              * Uppercase                                       *
      *              *-------------------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
       ext-prm-pro-200.
      *              *-------------------------------------------------*
      *              * Test se presente il carattere '%'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-chr-000      thru all-str-chr-999        .
      *                  *---------------------------------------------*
      *                  * Se non trovato: ad uscita                   *
      *                  *---------------------------------------------*
           if        w-all-str-num        =    zero
                     go to ext-prm-pro-900.
      *                  *---------------------------------------------*
      *                  * Scomposizione                               *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                  *---------------------------------------------*
      *                  * Test sui componenti                         *
      *                  *---------------------------------------------*
           if        w-all-str-cat (2)
                    (01 : 02)             not  = "2F"
                     go to ext-prm-pro-900.
           move      w-all-str-cat (1)    to   w-exe-prm-vx1          .
           move      w-all-str-cat (2)
                    (03 : 12)             to   w-exe-prm-vx2          .
      *                  *---------------------------------------------*
      *                  * Riassemblaggio                              *
      *                  *---------------------------------------------*
           move      14                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-exe-prm-vx1        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      w-exe-prm-vx2        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
       ext-prm-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-pro-999.
       ext-prm-pro-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
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
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione ubicazioni        *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-opn-000  thru det-prm-ubi-opn-999    .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
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
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione ubicazioni       *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-cls-000  thru det-prm-ubi-cls-999    .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Ricerca del codice numerico prodotto            *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to exe-cph-100.
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
           move      w-det-num-pro-num    to   w-exe-num-pro          .
           move      w-det-num-pro-alf    to   w-exe-alf-pro          .
       exe-cph-100.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Raccolta dati supplementari                     *
      *              *-------------------------------------------------*
           perform   exe-rds-000          thru exe-rds-999            .
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Emissione testata                               *
      *              *-------------------------------------------------*
           perform   emi-tes-000          thru emi-tes-999            .
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Emissione corpo documento                       *
      *              *-------------------------------------------------*
           perform   emi-cor-000          thru emi-cor-999            .
      *              *-------------------------------------------------*
      *              * Emissione piede documento                       *
      *              *-------------------------------------------------*
           perform   emi-pie-000          thru emi-pie-999            .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Raccolta dati supplementari                               *
      *    *-----------------------------------------------------------*
       exe-rds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione prodotto                        *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-exe-num-pro        to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione                       *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-prm-ubi-tip-ope      .
           move      01                   to   d-prm-ubi-cod-dpz      .
           move      01                   to   d-prm-ubi-tip-mag      .
           move      rf-dcp-num-pro       to   d-prm-ubi-num-mag      .
           move      spaces               to   d-prm-ubi-var-mag      .
           perform   det-prm-ubi-cll-000  thru det-prm-ubi-cll-999    .
           move      d-prm-ubi-ubi-lit    to   w-exe-prm-ubi          .
       exe-rds-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-rds-999.
       exe-rds-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-tes-000.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           move      "HH"                 to   h-ope                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Commento di apertura                            *
      *              *-------------------------------------------------*
           move      "EC"                 to   h-ope                  .
           move      "* wip - Tutti i diritti riservati *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Commento di aggiornamento                       *
      *              *-------------------------------------------------*
           move      "EC"                 to   h-ope                  .
           move      "* .....Aggiornamento : 20/05/2023 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "[***[NOBILI]***] Fatture Fornitori"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI CSS                                   *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css/"            to   h-prm                  .
           move      "jquery-ui.css"      to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Css per il documento                            *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css/"            to   h-prm                  .
           move      "ele.css"            to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery min                                      *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery.min.js"      to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI min                                   *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery-ui.min.js"   to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Javascript per il documento                     *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "eleubi01.js"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Head - fine                                     *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "head"               to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "body"               to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='ubi_001' id='ubi_001' method='post' ac
      -              "tion='/cgi-bin/eleubi01'>"                      .
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
      *              *-------------------------------------------------*
      *              * Tabella di input codice prodotto                *
      *              *-------------------------------------------------*
           display   "<table width=30% border=0 cellspacing=0 cellpaddin
      -              "g=0 bgcolor='#FFFFFF' align='center'>"          .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Responsabile                                    *
      *              *-------------------------------------------------*
           perform   emi-tes-rsm-000      thru emi-tes-rsm-999        .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   emi-tes-pro-000      thru emi-tes-pro-999        .
      *              *-------------------------------------------------*
      *              * Codice numerico prodotto 'hidden'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-dcp-num-pro       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "num_pro"            to   h-nam                  .
           move      v-edt                to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-tes-600.
      *              *-------------------------------------------------*
      *              * Test se prodotto trovato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-det-num-pro-num    not  = zero
                     go to emi-tes-800.
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           display   "<hr>"                                           .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "CODICE PRODOTTO '"  to   w-all-str-cat (1)      .
           move      w-exe-alf-pro        to   w-all-str-cat (2)      .
           move      "' NON TROVATO !"    to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      w-all-str-alf        to   w-exe-err-msg          .
           move      "msg_red"            to   w-exe-cla-msg          .
           perform   err-msg-htm-000      thru err-msg-htm-999        .
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           display   "<hr>"                                           .
       emi-tes-800.
      *              *-------------------------------------------------*
      *              * Inizio corpo                                    *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
       emi-tes-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *                                                           *
      *    * Subroutine per accettazione Responsabile (Operatore)      *
      *    *-----------------------------------------------------------*
       emi-tes-rsm-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ute' name= 'cel_ute' align=center>" .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Operatore </h1>"                           .
      *              *-------------------------------------------------*
      *              * Apertura campo Select                           *
      *              *-------------------------------------------------*
           display   "<select name='rsp_doc' id='rsp_doc'>"           .
      *              *-------------------------------------------------*
      *              * Elemento iniziale vuoto                         *
      *              *-------------------------------------------------*
           if        w-exe-cnv-rsm        not  = zero
                     go to emi-tes-rsm-100.
           display   "<option value=0 selected> (non selezionato)"    .
           display   "</option>"                                      .
       emi-tes-rsm-100.
      *              *-------------------------------------------------*
      *              * Costruzione tabella responsabili                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-rsm-inx      .
       emi-tes-rsm-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-rsm-inx      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-inx    >    w-det-tbl-rsm-ctr
                     go to emi-tes-rsm-500.
           if        w-det-tbl-rsm-inx    >    w-det-tbl-rsm-max
                     go to emi-tes-rsm-500.
      *                  *---------------------------------------------*
      *                  * Editing codice responsabile                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-det-tbl-cod-rsp
                    (w-det-tbl-rsm-inx)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Test se elemento selezionato                *
      *                  *---------------------------------------------*
           if        w-det-tbl-cod-rsp
                    (w-det-tbl-rsm-inx)   =    w-exe-cnv-rsm
                     move  "selected"     to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Emissione elemento                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<option "
                                delimited by   size
                     w-all-str-cat (1)
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-all-str-cat (2)
                                delimited by   spaces
                     " value='"
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     "'>"
                                delimited by   size
                     w-det-tbl-des-rsp
                    (w-det-tbl-rsm-inx)
                                delimited by   spaces
                     "</option>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to emi-tes-rsm-200.
       emi-tes-rsm-500.
      *              *-------------------------------------------------*
      *              * Chiusura accettazione                           *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
       emi-tes-rsm-300.
      *              *-------------------------------------------------*
      *              * Chiusura accettazione                           *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-tes-rsm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-rsm-999.
       emi-tes-rsm-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *                                                           *
      *    * Subroutine per Codice prodotto                            *
      *    *-----------------------------------------------------------*
       emi-tes-pro-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_pro' name='cel_pro' align=center>"  .
       emi-tes-pro-200.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Prodotto </h1>"                            .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='alf_pro' name='alf_pro' "
                                delimited by   size
                     "value='"
                                delimited by   size
                     w-exe-alf-pro
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='18'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-tes-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-pro-999.
       emi-tes-pro-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
           display   "<table width=80% border=0 cellspacing=5 cellpaddin
      -              "g=5 bgcolor='#FFFFFF' align='center'>"          .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Trattamento codice e descrizione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt codice                               *
      *                  *---------------------------------------------*
           display   "<td> Codice </td>"                              .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           display   "<td colspan=2><b style='color:blue'>"           .
           display   w-exe-alf-pro                                    .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           display   "<td colspan=2><b style='color:blue'>"           .
           display   rf-dcp-des-pro                                   .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-150.
      *              *-------------------------------------------------*
      *              * Fincatura per le ubicazioni                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           display   "<td  colspan=5 bgcolor='#BBBBBB'>&nbsp;</td>"   .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-200.
      *              *-------------------------------------------------*
      *              * Estrazione codici Ubicazione                    *
      *              *-------------------------------------------------*
           move      w-exe-prm-ubi        to   w-all-str-alf          .
           move      " "                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (1)    to   w-exe-prm-ub1          .
           move      w-all-str-cat (2)    to   w-exe-prm-ub2          .
           move      w-all-str-cat (3)    to   w-exe-prm-ub3          .
           move      w-all-str-cat (4)    to   w-exe-prm-ub4          .
       emi-cor-300.
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione 1                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           display   "<td width='25%'> Ubicazione principale</td>"    .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           display   "<td colspan='2'><b>"                            .
           display   w-exe-prm-ub1                                    .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Accettazione Ubicazione - 1                 *
      *                  *---------------------------------------------*
           move      1                    to   w-exe-inx-ubi          .
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-400.
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione 2                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           display   "<td width='25%'> Ubicazione di riserva 1</td>"  .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           display   "<td colspan='2'><b>"                            .
           display   w-exe-prm-ub2                                    .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Accettazione Ubicazione - 2                 *
      *                  *---------------------------------------------*
           move      2                    to   w-exe-inx-ubi          .
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-500.
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione 3                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           display   "<td width='25%'> Ubicazione di riserva 2</td>"  .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           display   "<td colspan='2'><b>"                            .
           display   w-exe-prm-ub3                                    .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Accettazione Ubicazione - 3                 *
      *                  *---------------------------------------------*
           move      3                    to   w-exe-inx-ubi          .
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-600.
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione 4                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           display   "<td width='25%'> Ubicazione di riserva 3</td>"  .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           display   "<td colspan='2'><b>"                            .
           display   w-exe-prm-ub4                                    .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * Accettazione Ubicazione - 4                 *
      *                  *---------------------------------------------*
           move      4                    to   w-exe-inx-ubi          .
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Fine tabella                                    *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura Form                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "form"               to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine di accettazione generica ubicazione            *
      *    *-----------------------------------------------------------*
       emi-cor-ubi-000.
      *              *-------------------------------------------------*
      *              * Valore da accettare in funzione dell'indice     *
      *              *-------------------------------------------------*
           if        w-exe-inx-ubi        =    1
                     move  w-exe-prm-ub1  to   w-exe-rig-ubi
           else if   w-exe-inx-ubi        =    2
                     move  w-exe-prm-ub2  to   w-exe-rig-ubi
           else if   w-exe-inx-ubi        =    3
                     move  w-exe-prm-ub3  to   w-exe-rig-ubi
           else if   w-exe-inx-ubi        =    4
                     move  w-exe-prm-ub4  to   w-exe-rig-ubi          .
      *              *-------------------------------------------------*
      *              * Editing indice ubicazione                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-inx-ubi        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-inx-ubc          .
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "<td id='cel_ub"     to   w-all-str-cat (1)      .
           move      w-exe-inx-ubc        to   w-all-str-cat (2)      .
           move      "' name='cel_ub"     to   w-all-str-cat (3)      .
           move      w-exe-inx-ubc        to   w-all-str-cat (4)      .
           move      "'>"                 to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
           if        w-det-num-pro-num    =    zero
                     move  "disabled"     to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='acc_ub"
                                delimited by   size
                     w-exe-inx-ubc
                                delimited by   size
                     "' name='acc_ub"
                                delimited by   size
                     w-exe-inx-ubc
                                delimited by   size
                     "' class ='acc_ubi' value='"
                                delimited by   size
                     w-exe-rig-ubi
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='07' "
                                delimited by   size
                                
                     w-all-str-cat (1)
                                delimited by   spaces
                     "/>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-ubi-800.
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-ubi-999.
       emi-cor-ubi-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Bottone di ritorno alla home                    *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
           display   "<div id='back' name='back'>"                    .
           display   "<a href='../ele.html' id='next-blue'>"          .
           display   "<img src='../icons/next-blue-1.png' width='50'>".
           display   "</a>"                                           .
           display   "</div>"                                         .
      *              *-------------------------------------------------*
      *              * Copyright                                       *
      *              *-------------------------------------------------*
           display   "<div id='copy' name='copy'>"                    .
           display   "<address>"                                      .
           display   "<font size=3>&copy wip - TANGRAM"               .
           display   "</address>"                                     .
           display   "</div>"                                         .
           display   "</center>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura HTML                                   *
      *              *-------------------------------------------------*
           display   "</body>"                                        .
           display   "</html>"                                        .
       emi-pie-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-pie-999.
       emi-pie-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
       det-num-pro-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [dcp] per codice alfanumerico *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "ALFPRO    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-num-pro-alf    to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: a ricerca per barcode      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-600.
       det-num-pro-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [dcp]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-600.
       det-num-pro-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [dcp]                      *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-600.
       det-num-pro-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazioni                                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
       det-num-pro-500.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     det-num-pro-900.
       det-num-pro-600.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-klb-000  thru det-num-pro-klb-999    .
      *                  *---------------------------------------------*
      *                  * Test se codice prodotto determinato         *
      *                  *---------------------------------------------*
           if        w-det-num-pro-num    not  = zero
                     go to det-num-pro-900.
       det-num-pro-700.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode confezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-cfz-000  thru det-num-pro-cfz-999    .
      *                  *---------------------------------------------*
      *                  * Test se codice prodotto determinato         *
      *                  *---------------------------------------------*
           if        w-det-num-pro-num    not  = zero
                     go to det-num-pro-900.
       det-num-pro-800.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode imballo             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-imb-000  thru det-num-pro-imb-999    .
       det-num-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-999.
       det-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *                                                           *
      *    * Subroutine per barcode [dcp]                              *
      *    *-----------------------------------------------------------*
       det-num-pro-klb-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [dcp] per barcode             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "KLBPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-dcp-klb-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-klb-900.
       det-num-pro-klb-200.
      *              *-------------------------------------------------*
      *              * Next su [dcp]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Next errata : ad uscita                  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-klb-900.
       det-num-pro-klb-300.
      *              *-------------------------------------------------*
      *              * Max su [dcp]                                    *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-klb-900.
       det-num-pro-klb-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazioni                                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
           move      rf-dcp-alf-pro       to   w-det-num-pro-alf      .
       det-num-pro-klb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-klb-999.
       det-num-pro-klb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *                                                           *
      *    * Subroutine per barcode della confezione                   *
      *    *-----------------------------------------------------------*
       det-num-pro-cfz-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [pdk] per barcode confezione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-pdk-alf-pro         .
           move      "B"                  to   rf-pdk-tip-rec         .
           move      zero                 to   rf-pdk-num-pro         .
           move      zero                 to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se errata : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-300.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-cfz-900.
           if        rf-pdk-tip-rec       not  = "B"
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-400.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-pdk-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Bufferizzazioni                                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
           move      rf-dcp-alf-pro       to   w-det-num-pro-alf      .
       det-num-pro-cfz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-cfz-999.
       det-num-pro-cfz-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *                                                           *
      *    * Subroutine per barcode dell'imballo                       *
      *    *-----------------------------------------------------------*
       det-num-pro-imb-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [pdk] per barcode confezione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-pdk-alf-pro         .
           move      "D"                  to   rf-pdk-tip-rec         .
           move      zero                 to   rf-pdk-num-pro         .
           move      zero                 to   rf-pdk-cod-arc         .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se errata : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-imb-900.
       det-num-pro-imb-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [pdk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-imb-900.
       det-num-pro-imb-300.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-imb-900.
           if        rf-pdk-tip-rec       not  = "D"
                     go to det-num-pro-imb-900.
       det-num-pro-imb-400.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-pdk-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Bufferizzazioni                                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
           move      rf-dcp-alf-pro       to   w-det-num-pro-alf      .
       det-num-pro-imb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-imb-999.
       det-num-pro-imb-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'estrazione di max 20 coppie campi/valori da *
      *    * variabile POST letta                                      *
      *    *---------------------------------------------------------- *
      *    *                                                           *
      *    * Input  : w-cgi-str-var     = Variabile POST letta         *
      *    *                                                           *
      *    *          w-cgi-str-del     = Delimitatore (&)             *
      *    *                                                           *
      *    * Output : w-cgi-str-fld (i) = coppie estratte              *
      *    *                                                           *
      *    *          w-cgi-str-num     = Numero coppie estratte       *
      *    *-----------------------------------------------------------*
       cgi-str-ext-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "&"                  to   w-cgi-str-del          .
           move      zero                 to   w-cgi-str-num          .
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-200.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-300.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
           go to     cgi-str-ext-200.
       cgi-str-ext-300.
      *              *-------------------------------------------------*
      *              * Test preliminare se il campo e' vuoto           *
      *              *-------------------------------------------------*
           if        w-cgi-str-var        =    spaces
                     go to cgi-str-ext-900.
       cgi-str-ext-400.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           unstring  w-cgi-str-var
                                delimited by   w-cgi-str-del
                                          into w-cgi-str-fld (01)
                                               w-cgi-str-fld (02)
                                               w-cgi-str-fld (03)
                                               w-cgi-str-fld (04)
                                               w-cgi-str-fld (05)
                                               w-cgi-str-fld (06)
                                               w-cgi-str-fld (07)
                                               w-cgi-str-fld (08)
                                               w-cgi-str-fld (09)
                                               w-cgi-str-fld (10)
                                               w-cgi-str-fld (11)
                                               w-cgi-str-fld (12)
                                               w-cgi-str-fld (13)
                                               w-cgi-str-fld (14)
                                               w-cgi-str-fld (15)
                                               w-cgi-str-fld (16)
                                               w-cgi-str-fld (17)
                                               w-cgi-str-fld (18)
                                               w-cgi-str-fld (19)
                                               w-cgi-str-fld (20)     .
       cgi-str-ext-500.
      *              *-------------------------------------------------*
      *              * Ciclo di verifica numero stringhe estratte      *
      *              *-------------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       cgi-str-ext-600.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to cgi-str-ext-900.
           if        w-cgi-str-fld
                    (w-cgi-str-ctr)       =    spaces
                     go to cgi-str-ext-900.
           add       1                    to   w-cgi-str-num          .
       cgi-str-ext-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-600.
       cgi-str-ext-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cgi-str-ext-999.
       cgi-str-ext-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tabella responsabili                       *
      *    *-----------------------------------------------------------*
       det-tbl-rsm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tbl-rsm-ctr      .
       det-tbl-rsm-050.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
       det-tbl-rsm-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [zrm]                         *
      *              *-------------------------------------------------*
           move      "ST"                 to   f-ope                  .
           move      "CODRSP    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: a close                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-rsm-900.
       det-tbl-rsm-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [zrm]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-rsm-900.
       det-tbl-rsm-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [zrm]                      *
      *              *-------------------------------------------------*
       det-tbl-rsm-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su Codice ambito di responsabilita'    *
      *                  *---------------------------------------------*
______*    if        rf-zrm-cod-adr       not  = 02
______*              go to det-tbl-rsm-200.
       det-tbl-rsm-500.
      *              *-------------------------------------------------*
      *              * Incremento tabella                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento tabella                          *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-rsm-ctr      .
           move      rf-zrm-cod-rsp       to   w-det-tbl-cod-rsp
                                              (w-det-tbl-rsm-ctr)     .
           move      rf-zrm-des-rsp       to   w-det-tbl-des-rsp
                                              (w-det-tbl-rsm-ctr)     .
       det-tbl-rsm-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-tbl-rsm-200.
       det-tbl-rsm-900.
      *              *-------------------------------------------------*
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tbl-rsm-999.
       det-tbl-rsm-999.
           exit.

      *    *===========================================================*
      *    * Operazioni su parametri in input                          *
      *    *-----------------------------------------------------------*
       ope-prm-inp-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cgi-tip-ope        =    "NO"
                     go to ope-prm-inp-100
           else if   w-cgi-tip-ope        =    "EX"
                     go to ope-prm-inp-500.
       ope-prm-inp-100.
      *              *=================================================*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di normalizzazione                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-120.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
           move      spaces               to   w-cgi-str-fld
                                              (w-cgi-str-ctr)         .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-120.
       ope-prm-inp-500.
      *              *=================================================*
      *              * Estrazione coppie                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di estrazione                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-cgi-str-ctr          .
       ope-prm-inp-520.
           add       1                    to   w-cgi-str-ctr          .
           if        w-cgi-str-ctr        >    w-cgi-str-num
                     go to ope-prm-inp-900.
           if        w-cgi-str-ctr        >    w-cgi-str-max
                     go to ope-prm-inp-900.
      *                  *---------------------------------------------*
      *                  * Estrazione                                  *
      *                  *---------------------------------------------*
           move      w-cgi-str-fld
                    (w-cgi-str-ctr)       to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           perform   ext-prm-ass-000      thru ext-prm-ass-999        .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     ope-prm-inp-520.
       ope-prm-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ope-prm-inp-999.
       ope-prm-inp-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "rsp_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-rsm
           else if   w-all-str-cat (1)    =    "alf_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-alf-pro          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggi di errore                              *
      *    *-----------------------------------------------------------*
       err-msg-htm-000.
      *              *-------------------------------------------------*
      *              * Emissione messaggio                             *
      *              *-------------------------------------------------*
           display   "<div id='msg' class='"                          .
           display   w-exe-cla-msg                                    .
           display   "'><h1> "                                        .
           display   w-exe-err-msg                                    .
           display   " </h1>"                                         .
           display   "</div>"                                         .
       err-msg-htm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione ubicazione di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
