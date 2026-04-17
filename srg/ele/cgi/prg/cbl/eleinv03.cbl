       Identification Division.
       Program-Id.                                 eleinv03           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 23/11/23    *
      *                       Ultima revisione:    NdK del 23/12/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Inventario per Prodotto - ELETTRA           *
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
      *        * [miu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmiu"                          .
      *        *-------------------------------------------------------*
      *        * [mim]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmim"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cpw"                   .

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
           05  w-exe-ctr-rig              pic  9(05)                  .
           05  w-exe-tot-rig              pic s9(13)                  .
           05  w-exe-ctr-ria              pic  x(05)                  .
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  9(07)                  .
           05  w-exe-cod-ubi              pic  x(07)                  .
           05  w-exe-ann-ncf              pic  x(80)                  .
           05  w-exe-flg-spn              pic  x(01)                  .
           05  w-exe-rig-qtv              pic s9(11)                  .
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
           05  w-exe-str-chd              pic  x(08)                  .
           05  w-exe-inp-val              pic  x(20)                  .
           05  w-exe-inp-stl              pic  x(80)                  .
           05  w-exe-ubi-pri              pic  x(07)                  .
           05  w-exe-sta-tus              pic  x(40)                  .
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
               10  w-det-num-pro-alf      pic  x(24)                  .
               10  w-det-num-pro-ele      pic  x(20)                  .
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
               10  w-det-tbl-cod-ele  occurs  48.
                   15  w-det-tbl-cod-rsp  pic  9(05)                  .
                   15  w-det-tbl-des-rsp  pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-rsm-max      pic  9(03) value 48         .
               10  w-det-tbl-rsm-ctr      pic  9(03)                  .
               10  w-det-tbl-rsm-inx      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione giacenza prodotto                  *
      *        *-------------------------------------------------------*
           05  w-det-gia-pro.
      *            *---------------------------------------------------*
      *            * Codice numerico prodotto                          *
      *            *---------------------------------------------------*
               10  w-det-gia-pro-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Quantita' determinata                             *
      *            *---------------------------------------------------*
               10  w-det-gia-pro-qta      pic s9(10)v9(03)            .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zrm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zrm.
               10  w-let-arc-zrm-flg      pic  x(01)                  .
               10  w-let-arc-zrm-cod      pic  9(05)                  .
               10  w-let-arc-zrm-des      pic  x(40)                  .
               10  w-let-arc-zrm-fds      pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    * per ubicazione                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dtl"                   .

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
           move      spaces               to   w-exe-cod-ubi          .
           move      spaces               to   w-exe-ann-ncf          .
           move      spaces               to   w-exe-flg-spn          .
           move      zero                 to   w-exe-rig-qtv          .
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
      *                  *                                             *
      *                  * Ricodifica unicode                          *
      *                  *---------------------------------------------*
           move      "DU"                 to   h-ope                  .
           move      w-exe-alf-pro        to   h-alf                  .
           move      "upp"                to   h-tip                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
           move      h-alf                to   w-exe-alf-pro          .
      *                  *---------------------------------------------*
      *                  * Codice numerico responsabile                *
      *                  *                                             *
      *                  * Conversione                                 *
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
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [miu]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * [mim]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magaz-   *
      *              * zino per ubicazione                             *
      *              *-------------------------------------------------*
           perform   det-sld-ubi-opn-000  thru det-sld-ubi-opn-999    .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [miu]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * [mim]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magaz-  *
      *              * zino per ubicazione                             *
      *              *-------------------------------------------------*
           perform   det-sld-ubi-cls-000  thru det-sld-ubi-cls-999    .
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
      *              * Normalizzazione [aaq]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Lettura codice per il Produttore                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      rf-dcp-num-pro       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
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
           perform   emi-tes-hea-000      thru emi-tes-hea-999        .
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='inv_003' id='inv_003' method='post' ac
      -              "tion=''>"                                       .
      *              *-------------------------------------------------*
      *              * Interlinae                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Tabella di input codice prodotto                *
      *              *-------------------------------------------------*
           display   "<table class='tes'>"                            .
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
       emi-tes-500.
      *              *-------------------------------------------------*
      *              * Bottone di reset                                *
      *              *-------------------------------------------------*
           display   "<td class='td_nb' align='center'>"              .
           display   "<button type='button' "                         .
           display   "id='but_res' name='but_res' "                   .
           display   "/>"                                             .
           display   "<img src='../../icons/gomma.png' width='40'>"   .
           display   "</button>"                                      .
           display   "</td>"                                          .
       emi-tes-550.
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
      *    * Subroutine per intestazione pagina                        *
      *    *-----------------------------------------------------------*
       emi-tes-hea-000.
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
           move      "INVENTARIO PER PRODOTTO {ELETTRA}"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI CSS                                   *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../../ele/css/"     to   h-prm                  .
           move      "jquery-ui.min.css"  to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Css per il documento                            *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../../ele/css/"     to   h-prm                  .
           move      "ele.css"            to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery min                                      *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "jquery.min.js"      to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI min                                   *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "jquery-ui.min.js"   to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Javascript per il documento                     *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "eleinv03.js"        to   h-alf                  .
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
       emi-tes-hea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-hea-999.
       emi-tes-hea-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *                                                           *
      *    * Subroutine per accettazione Responsabile (Operatore)      *
      *    *-----------------------------------------------------------*
       emi-tes-rsm-000.
      *              *-------------------------------------------------*
      *              * Test iniziale se codice responsabile o codice   *
      *              * prodotto non ancora inseriti                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-cnv-rsm        =    zero
                     go to emi-tes-rsm-050.
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td id='cel_ute' name= 'cel_ute' class='td_nb' ali
      -              "gn=center>"                                     .
      *                  *---------------------------------------------*
      *                  * Lettura descrizione responsabile            *
      *                  *---------------------------------------------*
           move      w-exe-cnv-rsm        to   w-let-arc-zrm-cod      .
           perform   let-arc-zrm-000      thru let-arc-zrm-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione responsabile    *
      *                  *---------------------------------------------*
           display   "<b>"                                            .
           display   w-let-arc-zrm-des                                .
           display   "</b>"                                           .
      *                  *---------------------------------------------*
      *                  * Emissione codice responsabile 'hidden'      *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "rsp_doc"            to   h-nam                  .
           move      w-exe-cod-rsm        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Emissione descrizione responsabile 'hidden' *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "rsp_des"            to   h-nam                  .
           move      w-let-arc-zrm-des    to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * A chiusura cella                            *
      *                  *---------------------------------------------*
           go to     emi-tes-rsm-800.
       emi-tes-rsm-050.
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
                                delimited by   size
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
       emi-tes-rsm-800.
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
           display   "<table id='pre' name='pre' class='bordotab' align=
      -              "'center' width=80%>"                            .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Determinazione giacenza (presunta)              *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-gia-pro-num      .
           perform   det-gia-pro-000      thru det-gia-pro-999        .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           display   "<tr style='background-color: #EEEEEE;'>"        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice produttore               *
      *              *-------------------------------------------------*
           perform   emi-cor-prd-000      thru emi-cor-prd-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione prodotto            *
      *              *-------------------------------------------------*
           perform   emi-cor-des-000      thru emi-cor-des-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione barcode prodotto                *
      *              *-------------------------------------------------*
           perform   emi-cor-bcd-000      thru emi-cor-bcd-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione status prodotto                 *
      *              *-------------------------------------------------*
           perform   emi-cor-sts-000      thru emi-cor-sts-999        .
      *              *-------------------------------------------------*
      *              * Accettazione nuova ubicazione                   *
      *              *-------------------------------------------------*
           perform   emi-cor-nub-000      thru emi-cor-nub-999        .
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
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Apertura tabella ubicazioni prodotto            *
      *              *-------------------------------------------------*
           display   "<table id='rig' name='rig' class='bordotab' align=
      -              "'center' width=80%>"                            .
       emi-cor-150.
      *              *-------------------------------------------------*
      *              * Fincatura per le ubicazioni                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to emi-cor-200.
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Progressivo riga                *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "NR"                 to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Ubicazione                      *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Ubicazione"         to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita' presunta              *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Presunta"           to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita' rilevata              *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Rilevata"           to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Delta                           *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Delta"              to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Spunta                          *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "(V)"                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Annotazioni                     *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Note"               to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
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
       emi-cor-300.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione ubicazioni rilevate e non    *
      *              *-------------------------------------------------*
           perform   emi-cor-miu-000      thru emi-cor-miu-999        .
       emi-cor-500.
      *              *-------------------------------------------------*
      *              * Totali di riepilogo                             *
      *              *-------------------------------------------------*
           perform   emi-cor-tot-000      thru emi-cor-tot-999        .
       emi-cor-800.
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
      *              *-------------------------------------------------*
      *              * Box di dialogo per conferma spunta generale     *
      *              *-------------------------------------------------*
           display   "<div id='dialog-confirm' name='dialog-confirm' tit
      -              "le=''>"                                         .                                 .
           display   "</div>"                                         .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-999.
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione elementi con ubicazione        *
      *    *-----------------------------------------------------------*
       emi-cor-miu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-rig          .
           move      zero                 to   w-exe-tot-rig          .
       emi-cor-miu-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [miu]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-num-pro        to   rf-miu-num-mag         .
           move      spaces               to   rf-miu-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to emi-cor-miu-900.
       emi-cor-miu-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [miu]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to emi-cor-miu-900.
       emi-cor-miu-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-miu-cod-dpz       not  = 01
                     go to  emi-cor-miu-900.
      *                  *---------------------------------------------*
      *                  * Test su codice numerico prodotto            *
      *                  *---------------------------------------------*
           if        rf-miu-num-mag       not  = w-exe-num-pro
                     go to  emi-cor-miu-900.
       emi-cor-miu-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [miu]                              *
      *              *-------------------------------------------------*
       emi-cor-miu-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-ctr-rig          .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           perform   emi-cor-inr-000      thru emi-cor-inr-999        .
       emi-cor-miu-520.
      *              *-------------------------------------------------*
      *              * Progressivo riga                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-exe-ctr-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-miu-550.
      *              *-------------------------------------------------*
      *              * Visualizzazione Ubicazione                      *
      *              *-------------------------------------------------*
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
       emi-cor-miu-570.
      *              *-------------------------------------------------*
      *              * Visualizzazione Quantita' presunta              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-miu-qta-prs       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-miu-600.
      *              *-------------------------------------------------*
      *              * Accettazione Quantita' rilevata                 *
      *              *-------------------------------------------------*
           perform   emi-cor-qtv-000      thru emi-cor-qtv-999        .
      *              *-------------------------------------------------*
      *              * Quantita' delta                                 *
      *              *-------------------------------------------------*
           perform   emi-cor-qtd-000  thru emi-cor-qtd-999    .
      *              *-------------------------------------------------*
      *              * Spunta                                          *
      *              *-------------------------------------------------*
           perform   emi-cor-spn-000      thru emi-cor-spn-999        .
      *              *-------------------------------------------------*
      *              * Non conformita' (annotazioni)                   *
      *              *-------------------------------------------------*
           perform   emi-cor-ncf-000      thru emi-cor-ncf-999        .
      *              *-------------------------------------------------*
      *              * Bottone Trash                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da mostrare                         *
      *                  *---------------------------------------------*
           if        rf-miu-qta-prs       not  = zero or
                     rf-miu-qta-rlv       not  = zero
                     go to emi-cor-miu-650.
           if        rf-miu-cod-ubi       =    w-exe-ubi-pri
                     go to emi-cor-miu-650.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<td id='cel_del' name='cel_del' class='td_nb'>" .
           display   "<button type='button' "                         .
           display   "id='del' name='del' class='del_btn'>"           .
           display   "<img src='../../icons/trash.png' width='40'>"   .
           display   "</button>"                                      .
           display   "</td>"                                          .
       emi-cor-miu-650.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-miu-700.
      *              *-------------------------------------------------*
      *              * Riga di espansione per la Non conformita'       *
      *              *-------------------------------------------------*
           perform   emi-cor-rnc-000      thru emi-cor-rnc-999        .
      *              *-------------------------------------------------*
      *              * Accumulo quantita' rilevata                     *
      *              *-------------------------------------------------*
           add       rf-miu-qta-rlv       to   w-exe-tot-rig          .
       emi-cor-miu-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     emi-cor-miu-200.
       emi-cor-miu-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-miu-999.
       emi-cor-miu-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di visualizzazione delta quantita'             *
      *    *-----------------------------------------------------------*
       emi-cor-qtd-000.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<D"                 to   v-edm                  .
           move      rf-miu-qta-rlv       to   v-num                  .
           subtract  rf-miu-qta-prs       from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
      *
           if        v-num                <    zero
                     move  "r"            to   h-stl
           else      move  "B"            to   h-stl                  .
      *
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-qtd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-qtd-999.
       emi-cor-qtd-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per apertura riga                              *
      *    *-----------------------------------------------------------*
       emi-cor-inr-000.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing del progressivo interno riga        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-ctr-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-exe-ctr-ria          .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<tr id='row_"       to   w-all-str-cat (1)      .
           move      w-exe-ctr-ria        to   w-all-str-cat (2)      .
           move      "' "                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Apertura Form                                   *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<form id='frm_"     to   w-all-str-cat (1)      .
           move      w-exe-ctr-ria        to   w-all-str-cat (2)      .
           move      "' name='frm_"       to   w-all-str-cat (3)      .
           move      w-exe-ctr-ria        to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           move      02                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "action='' method='post'>"
                                          to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Campi 'hidden' per la riga                      *
      *              *-------------------------------------------------*
           perform   emi-cor-hid-000      thru emi-cor-hid-999        .
       emi-cor-inr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-inr-999.
       emi-cor-inr-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per codice produttore                          *
      *    *-----------------------------------------------------------*
       emi-cor-prd-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rf-aaq-cdp-pdt       =    spaces
                     go to emi-cor-prd-900.
       emi-cor-prd-200.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
______*    display   "<td class='td_bi' align='left'>"                .
           display   "<td align='left'>"                              .
           display   "<b style='color:green'>"                        .
           display   rf-aaq-cdp-pdt                                   .
           display   "</b>"                                           .
           display   "</td>"                                          .
       emi-cor-prd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-prd-999.
       emi-cor-prd-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per descrizione prodotto                       *
      *    *-----------------------------------------------------------*
       emi-cor-des-000.
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
______*    display   "<td class='td_bi' align='left'>"                .
           display   "<td align='left'>"                              .
           display   "<b style='color:blue'>"                         .
           display   rf-dcp-des-pro                                   .
           display   "</b>"                                           .
           display   "</td>"                                          .
       emi-cor-des-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-des-999.
       emi-cor-des-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per barcode prodotto                           *
      *    *-----------------------------------------------------------*
       emi-cor-bcd-000.
      *              *-------------------------------------------------*
      *              * Trattamento barcode                             *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       =    spaces and
                     w-exe-alf-pro        =    spaces
                     move  "<td class='td_nb'>&nbsp;</td>"
                                          to   w-exe-str-dsp
                     go to emi-cor-bcd-400.
           if        rf-dcp-klb-pro       =    spaces
                     move  "<td class='td_nb'>(senza barcode)</td>"
                                          to   w-exe-str-dsp
                     go to emi-cor-bcd-400.
       emi-cor-bcd-200.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<td colspan='1' align='center' class='barcode'>"
                                delimited by size
                     "<img border=0 src='../elebcd01?BAR=ean&COD="
                                delimited by size
                     rf-dcp-klb-pro
                                delimited by spaces
                     "'></td>"
                                delimited by size
                                          into w-exe-str-dsp          .
       emi-cor-bcd-400.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           display   w-exe-str-dsp                                    .
       emi-cor-bcd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-bcd-999.
       emi-cor-bcd-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per status prodotto                            *
      *    *-----------------------------------------------------------*
       emi-cor-sts-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        rf-dcp-sta-tus       =    01
                     go to emi-cor-sts-900.
      *              *-------------------------------------------------*
      *              * Determinazione literal status                   *
      *              *                                                 *
      *              *  - 01 : Normale                                 *
      *              *  - 11 : Ad esaurimento                          *
      *              *  - 21 : Sostituito da ns. nuovo prodotto        *
      *              *  - 51 : Cessata commercializzazione             *
      *              *  - 52 : Cessata commercializzazione, ma         *
      *              *         sostituito da ns. nuovo prodotto        *
      *              *  - 71 : Obsoleto                                *
      *              *  - 72 : Obsoleto, ma sostituito da ns.          *
      *              *         nuovo prodotto                          *
      *              *-------------------------------------------------*
           if        rf-dcp-sta-tus       =    11
                     move  "Ad esaurimento"
                                          to   w-exe-sta-tus
           else if   rf-dcp-sta-tus       =    21
                     move  "Sostituito da nuovo prodotto"
                                          to   w-exe-sta-tus
           else if   rf-dcp-sta-tus       =    51 or
                     rf-dcp-sta-tus       =    52
                     move  "Cessata commercializzazione"
                                          to   w-exe-sta-tus
           else if   rf-dcp-sta-tus       =    71 or
                     rf-dcp-sta-tus       =    72
                     move  "Obsoleto"     to   w-exe-sta-tus
           else      go to emi-cor-sts-900.
       emi-cor-sts-200.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           display   "<td class='td_nb' align='center'>"              .
           display   "<b style='color:red'>"                          .
           display   w-exe-sta-tus                                    .
           display   "</b>"                                           .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-sts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-sts-999.
       emi-cor-sts-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per nuova ubicazione                           *
      *    *-----------------------------------------------------------*
       emi-cor-nub-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_new' name='cel_new'>"               .
       emi-cor-nub-100.
      *              *-------------------------------------------------*
      *              * Accettazione nuovo codice ubicazione            *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
           string    "<input type='text' name='cod_ubi' id='cod_ubi'"
                                delimited by   size
                     " style='text-transform: uppercase;' "
                                delimited by   size
                     " maxlength='07' "
                                delimited by   size
                     " value='"
                                delimited by   size
                     w-exe-cod-ubi
                                delimited by   spaces
                     "'> NUOVA"
                                delimited by   size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * A capo                                          *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Bottone nuovo record                            *
      *              *-------------------------------------------------*
           display   "<button type='button' id='new' name='new' "     .
           display   " class='new_rec' "                              .
           display   "/>"                                             .
           display   "<img src='../../icons/new.gif' width='40'>"     .
           display   "</button>"                                      .
           display   "</td>"                                          .
       emi-cor-nub-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-nub-999.
       emi-cor-nub-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine di accettazione generica ubicazione            *
      *    *-----------------------------------------------------------*
       emi-cor-ubi-000.
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione principale            *
      *              *-------------------------------------------------*
           move      "UP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-miu-num-mag       to   d-sld-ubi-num-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
           move      d-sld-ubi-cod-ubi    to   w-exe-ubi-pri          .
      *              *-------------------------------------------------*
      *              * Determinazione tipo ubicazione                  *
      *              *-------------------------------------------------*
           move      "TU"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      rf-miu-cod-ubi       to   d-sld-ubi-cod-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      rf-miu-cod-ubi       to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
      *
           if        rf-miu-cod-ubi       =    w-exe-ubi-pri
                     move  "r"            to   h-stl
           else if   d-sld-ubi-exi-sts    =    "T"
                     move  "g"            to   h-stl
           else      move  "B"            to   h-stl                  .
      *
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
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione quantita' rilevata             *
      *    *-----------------------------------------------------------*
       emi-cor-qtv-000.
      *              *-------------------------------------------------*
      *              * Valori determinati da riga ordine di spedizione *
      *              *-------------------------------------------------*
           if        rf-miu-qta-rlv       not  = 0
                     move  "S"            to   w-exe-flg-spn          .
       emi-cor-qtv-100.
      *              *-------------------------------------------------*
      *              * Apertura cella in funzione della quantita'      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella normale                      *
      *                  *---------------------------------------------*
           move      "<td id='cel_qtv' name ='cel_qtv' align='right'>"
                                          to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Apertura cella con quantita' superiore      *
      *                  *---------------------------------------------*
           move      "<td id='cel_qtv' name ='cel_qtv' class='sel' align
      -              "='right'>"          to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Apertura cella con quantita' inferiore      *
      *                  *---------------------------------------------*
           move      "<td id='cel_qtv' name ='cel_qtv' class='msg_red' a
      -              "lign='right'>"      to   w-all-str-cat (3)      .
      *                  *---------------------------------------------*
      *                  * Apertura cella riga chiusa                  *
      *                  *---------------------------------------------*
           move      "<td id='cel_qtv' name ='cel_qtv' class='td_ds' ali
      -              "gn='right'>"        to   w-all-str-cat (4)      .
      *                  *---------------------------------------------*
      *                  * Test sulle quantita'                        *
      *                  *---------------------------------------------*
           if        rf-miu-qta-rlv       =    zero
                     move  w-all-str-cat (1)
                                          to   w-all-str-alf
           else if   rf-miu-qta-rlv       =    rf-miu-qta-prs
                     move  w-all-str-cat (1)
                                          to   w-all-str-alf
           else if   rf-miu-qta-rlv       >    rf-miu-qta-prs
                     move  w-all-str-cat (2)
                                          to   w-all-str-alf
           else      move  w-all-str-cat (3)
                                          to   w-all-str-alf          .
      *                  *---------------------------------------------*
      *                  * Test su stato riga                          *
      *                  *---------------------------------------------*
           if        rf-miu-flg-rlv       not  = spaces
                     move  w-all-str-cat (4)
                                          to   w-all-str-alf          .
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   w-all-str-alf                                    .
       emi-cor-qtv-200.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-miu-qta-rlv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           if        rf-miu-qta-rlv       =    zero
                     move  "0"            to   w-exe-inp-val
           else      move  v-edt          to   w-exe-inp-val          .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           display   "<input type='number' "                          .
           display   "id='qtv' name ='qtv' "                          .
           display   "class='numbersOnly' "                           .
      *              *-------------------------------------------------*
      *              * Stile                                           *
      *              *-------------------------------------------------*
           move      "style='text-align:right;font-weight:bold;' "
                                          to   w-exe-inp-stl          .
      *              *-------------------------------------------------*
      *              * Eventuale colore e stile                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "value='"  delimited by   size
                     w-exe-inp-val
                                delimited by   spaces
                     "' "       delimited by   size
                     "min='0' max='999999' "
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-inp-stl                                    .
           display   w-exe-str-dsp                                    .
       emi-cor-qtv-600.
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
______*    if        rf-osr-snx-3qt       =    0 and
______*              rf-miu-flg-rlv       =    spaces
______*              go to emi-cor-qtv-700.
______*    display   "disabled "                                      .
       emi-cor-qtv-700.
      *              *-------------------------------------------------*
      *              * Chiusura input                                  *
      *              *-------------------------------------------------*
           display   "/>"                                             .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-qtv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-qtv-999.
       emi-cor-qtv-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione spunta                         *
      *    *-----------------------------------------------------------*
       emi-cor-spn-000.
      *              *-------------------------------------------------*
      *              * Editing del progressivo interno riga            *
      *              *-------------------------------------------------*
       emi-cor-spn-200.
      *              *-------------------------------------------------*
      *              * Spunta                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td id='cel_spn' name='cel_spn' class='td_nb'>" .
      *                  *---------------------------------------------*
      *                  * Test se attiva o no                         *
      *                  *---------------------------------------------*
           if        rf-miu-flg-rlv       not  = spaces
                     move  "checked"      to   w-exe-str-chd
           else      move  spaces         to   w-exe-str-chd          .
      *                  *---------------------------------------------*
      *                  * Test se riga chiusa                         *
      *                  *---------------------------------------------*
______*    if        rf-miu-flg-rlv       not  = spaces
______*              move  "disabled"     to   w-exe-str-dsb
______*    else      move  spaces         to   w-exe-str-dsb          .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='checkbox' id='flg_spn'"
                                delimited by   size
                     " name='flg_spn' "
                                delimited by   size
                     w-exe-str-chd
                                delimited by   size
                     ">"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-spn-700.
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-spn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-spn-999.
       emi-cor-spn-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di gestione Non conformita' (annotazioni)      *
      *    *-----------------------------------------------------------*
       emi-cor-ncf-000.
      *              *-------------------------------------------------*
      *              * Bottone                                         *
      *              *-------------------------------------------------*
           display   "<td id='cel_ncf' name='cel_ncf' class='td_nb'>" .
           display   "<button type='button' "                         .
           display   " class='ncf' "                                  .
           display   "/>"                                             .
           display   "<img src='../../icons/new.png' width='40'>"     .
           display   "</button>"                                      .
           display   "</td>"                                          .
       emi-cor-ncf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-ncf-999.
       emi-cor-ncf-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di emissione della riga 2 per la Non conformi- *
      *    * ta' (annotazione)                                         *
      *    *-----------------------------------------------------------*
       emi-cor-rnc-000.
      *              *-------------------------------------------------*
      *              * Recupero del campo                              *
      *              *-------------------------------------------------*
           move      rf-miu-ann-rlv       to   w-exe-ann-ncf          .
       emi-cor-rnc-100.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           if        w-exe-ann-ncf        =    spaces
                     move  "<tr class='exp_ncf' id='rw2_"
                                          to   w-all-str-cat (1)
           else      move  "<tr id='rw2_" to   w-all-str-cat (1)      .
      *
           move      w-exe-ctr-ria        to   w-all-str-cat (2)      .
           move      "' "                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
           display   "/>"                                             .
      *              *-------------------------------------------------*
      *              * Campi 'hidden' per la riga                      *
      *              *-------------------------------------------------*
           perform   emi-cor-hid-000      thru emi-cor-hid-999        .
      *              *-------------------------------------------------*
      *              * Apertura colonna                                *
      *              *-------------------------------------------------*
           display   "<td colspan='5'>"                               .
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
______*    if        w-exe-flg-spn        =    "S"
______*              move  "disabled"     to   w-all-str-cat (1)
______*    else      move  spaces         to   w-all-str-cat (1)      .
      *              *-------------------------------------------------*
      *              * Accettazione campo testo                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='nct' name='nct' "
                                delimited by   size
                     "class='inp_ncf' "
                                delimited by   size
                     "value="""
                                delimited by   size
                     w-exe-ann-ncf
                                delimited by   size
                     """ maxlength='80' "
                                delimited by   size
                     " >"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura colonna                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
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
       emi-cor-rnc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-rnc-999.
       emi-cor-rnc-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di emissione campi 'hidden' per la riga        *
      *    *-----------------------------------------------------------*
       emi-cor-hid-000.
      *              *-------------------------------------------------*
      *              * Responsabile 'hidden'                           *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "rsm"                to   h-nam                  .
           move      w-exe-cod-rsm        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-100.
      *              *-------------------------------------------------*
      *              * 'hidden' - Codice ubicazione                    *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "ubi"                to   h-nam                  .
           move      rf-miu-cod-ubi       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-150.
      *              *-------------------------------------------------*
      *              * Progressivo riga interno 'hidden'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-exe-ctr-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "pri"                to   h-nam                  .
           move      v-edt                to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-200.
      *              *-------------------------------------------------*
      *              * Progressivo collo 'hidden'                      *
      *              *-------------------------------------------------*
       emi-cor-hid-300.
      *              *-------------------------------------------------*
      *              * Si/no riga spuntata 'hidden'                    *
      *              *-------------------------------------------------*
       emi-cor-hid-400.
      *              *-------------------------------------------------*
      *              * 'hidden' -  Quantita' rilevata                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-miu-qta-rlv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "qta"                to   h-nam                  .
           move      v-edt                to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-400.
      *              *-------------------------------------------------*
      *              * 'hidden' -  Quantita' presunta                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-miu-qta-prs       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "qtp"                to   h-nam                  .
           move      v-edt                to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-500.
      *              *-------------------------------------------------*
      *              * 'hidden' - Codice ubicazione                    *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "ubi"                to   h-nam                  .
           move      rf-miu-cod-ubi       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-600.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico prodotto 'hidden'           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-miu-num-mag       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "alf"                to   h-nam                  .
           move      rf-dcp-alf-pro       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-700.
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
           move      rf-miu-num-mag       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "pro"                to   h-nam                  .
           move      v-edt                to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-700.
      *              *-------------------------------------------------*
      *              * Status riga 'hidden'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "sts"                to   h-nam                  .
           move      rf-miu-flg-rlv       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-hid-750.
      *              *-------------------------------------------------*
      *              * Si/no prodotto con barcode                      *
      *              *-------------------------------------------------*
       emi-cor-hid-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-hid-999.
       emi-cor-hid-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di emissione totali di riepilogo               *
      *    *-----------------------------------------------------------*
       emi-cor-tot-000.
      *              *-------------------------------------------------*
      *              * Test se totali da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-exe-ctr-rig        not  > 1
                     go to emi-cor-tot-900.
       emi-cor-tot-100.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Cella vuota                                     *
      *              *-------------------------------------------------*
           display   "<td class='td_nb'> &nbsp; </td>"                .
      *              *-------------------------------------------------*
      *              * Cella vuota                                     *
      *              *-------------------------------------------------*
           display   "<td class='td_nb'> &nbsp; </td>"                .
       emi-cor-tot-200.
      *              *-------------------------------------------------*
      *              * Totale quantita' rilevata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing quantita' presunta                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-det-gia-pro-qta    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tot-400.
      *              *-------------------------------------------------*
      *              * Totale quantita' presunta                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing quantita' rilevata                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-exe-tot-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tot-600.
      *              *-------------------------------------------------*
      *              * Totale delta                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      02                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<D"                 to   v-edm                  .
           move      w-exe-tot-rig        to   v-num                  .
           subtract  w-det-gia-pro-qta    from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
      *
           if        v-num                <    zero
                     move  "r"            to   h-stl
           else      move  "B"            to   h-stl                  .
      *
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tot-800.
      *              *-------------------------------------------------*
      *              * Cella vuota                                     *
      *              *-------------------------------------------------*
           display   "<td class='td_nb' colspan='3'>"                 .
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tot-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-tot-999.
       emi-cor-tot-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
      *              *-------------------------------------------------*
      *              * Box per bottone di ritorno a menu               *
      *              *-------------------------------------------------*
           perform   emi-htm-bck-000      thru emi-htm-bck-999        .
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
      *    * Emissione rientro a menu'                                 *
      *    *-----------------------------------------------------------*
       emi-htm-bck-000.
      *              *-------------------------------------------------*
      *              * Box per bottone di ritorno a menu               *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "div"                to   h-tag                  .
           move      "I"                  to   h-sub                  .
           move      "back"               to   h-idx                  .
           move      "back"               to   h-nam                  .
           move      spaces               to   h-cla                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Tag 'a' per link                                *
      *              *-------------------------------------------------*
           move      "EA"                 to   h-ope                  .
           move      "../../ele_inv.html" to   h-hrf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Immagine                                        *
      *              *-------------------------------------------------*
           move      "IM"                 to   h-ope                  .
           move      "../../icons/next-blue-1.png"
                                          to   h-src                  .
           move      "50"                 to   h-wdt                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura 'a'                                    *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "a"                  to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura Box per rientro a menu                 *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "div"                to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-htm-bck-999.
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
      *    * Routine di lettura archivio [zrm]                         *
      *    *-----------------------------------------------------------*
       let-arc-zrm-000.
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
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zrm-cod    =    zero
                     go to let-arc-zrm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODRSP"             to   f-key                  .
           move      w-let-arc-zrm-cod    to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zrm-400.
       let-arc-zrm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zrm-des-rsp       to   w-let-arc-zrm-des      .
           move      rf-zrm-flg-spn       to   w-let-arc-zrm-fds      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zrm-999.
       let-arc-zrm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zrm-flg      .
           move      all   "."            to   w-let-arc-zrm-des      .
           move      "?"                  to   w-let-arc-zrm-fds      .
           go to     let-arc-zrm-600.
       let-arc-zrm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-des      .
           move      spaces               to   w-let-arc-zrm-fds      .
       let-arc-zrm-600.
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
       let-arc-zrm-999.
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
      *    * Determinazione giacenza prodotto                          *
      *    *-----------------------------------------------------------*
       det-gia-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gia-pro-qta      .
       det-gia-pro-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione [mim]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGTGC"             to   f-key                  .
           move      01                   to   rf-mim-cod-dpz         .
           move      01                   to   rf-mim-tip-mag         .
           move      w-det-gia-pro-num    to   rf-mim-num-mag         .
           move      spaces               to   rf-mim-var-mag         .
           move      01                   to   rf-mim-tip-gia         .
           move      spaces               to   rf-mim-tip-arc         .
           move      zero                 to   rf-mim-cod-arc         .
           move      spaces               to   rf-mim-dpz-arc         .
           move      spaces               to   rf-mim-cod-mic         .
           move      "pgm/mag/fls/ioc/obj/iofmim"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mim                 .
           if        f-sts                not  = e-not-err
                     go to det-gia-pro-900.
       det-gia-pro-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione quantita' letta                 *
      *              *-------------------------------------------------*
           move      rf-mim-qta-prs       to   w-det-gia-pro-qta      .
       det-gia-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-gia-pro-999.
       det-gia-pro-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino per     *
      *    * ubicazione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines di trattamento variabile POST                 *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
