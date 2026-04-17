       Identification Division.
       Program-Id.                                 eleubi01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    eleubi              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 18/06/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione cambio ubicazione                  *
      *                                                                *
      *                    ELETTRA (VERSIONE NUOVA)                    *
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
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-des-rsm              pic  x(40)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  9(07)                  .
           05  w-exe-ubi-qta              pic  x(10)                  .
           05  w-exe-cnv-qta              pic s9(11)                  .
           05  w-exe-acc-ub1              pic  x(07)                  .
           05  w-exe-acc-ub2              pic  x(07)                  .
           05  w-exe-pri-ubi              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-rig              pic  9(05)                  .
           05  w-exe-tot-rig              pic s9(13)                  .
           05  w-exe-ctr-ria              pic  x(05)                  .
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati                       *
      *        *-------------------------------------------------------*
           05  w-exe-str-chd              pic  x(08)                  .
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
               10  w-det-num-pro-alf      pic  x(24)                  .
               10  w-det-num-pro-ele      pic  x(20)                  .
               10  w-det-num-pro-tip      pic  x(01)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Det su giacenze per ubicazione               *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-ctr      pic  9(02)                  .
               10  w-det-gia-ubi-ucc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-qta      pic s9(10)v9(03)            .

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
           move      spaces               to   w-exe-cod-rsm          .
           move      zero                 to   w-exe-cnv-rsm          .
           move      spaces               to   w-exe-des-rsm          .
      *
           move      spaces               to   w-exe-alf-pro          .
           move      zero                 to   w-exe-num-pro          .
      *
           move      spaces               to   w-exe-ubi-qta          .
           move      zero                 to   w-exe-cnv-qta          .
      *
           move      spaces               to   w-exe-acc-ub1          .
           move      spaces               to   w-exe-acc-ub2          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      07                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
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
      *                  * Quantita'                                   *
      *                  *                                             *
      *                  * Conversione                                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-exe-ubi-qta        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exe-cnv-qta          .
      *                  *---------------------------------------------*
      *                  * Codice ubicazione di origine                *
      *                  *                                             *
      *                  * Uppercase                                   *
      *                  *---------------------------------------------*
           move      w-exe-acc-ub1        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-acc-ub1          .
      *                  *---------------------------------------------*
      *                  * Codice ubicazione di destinazione           *
      *                  *                                             *
      *                  * Uppercase                                   *
      *                  *---------------------------------------------*
           move      w-exe-acc-ub2        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-acc-ub2          .
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
       exe-rds-200.
      *              *-------------------------------------------------*
      *              * Test se confermata registrazione                *
      *              *-------------------------------------------------*
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
      *              * Emissione icona                                 *
      *              *-------------------------------------------------*
           move      "HF"                 to   h-ope                  .
           move      "../icons/"          to   h-prm                  .
           move      "favicon.ico"        to   h-alf                  .
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
           move      "* .....Aggiornamento : 15/05/2025 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "GESTIONE UBICAZIONE {ELETTRA}"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Jquery UI CSS                                   *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css_ele/"        to   h-prm                  .
           move      "jquery-ui.min.css"  to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Css per il documento                            *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../css_ele/"        to   h-prm                  .
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
      *              * Javascript accessoria per il documento          *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "eleipx01.js"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Javascript accessoria per il documento          *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "eleubiJ0.js"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura 'head'                                 *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "head"               to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Apertura 'body'                                 *
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
      -              "tion=''>"                                       .
      *              *-------------------------------------------------*
      *              * Campi 'hidden'                                  *
      *              *-------------------------------------------------*
           perform   emi-tes-hid-000      thru emi-tes-hid-999        .
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
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
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione principale            *
      *              *-------------------------------------------------*
           move      "UP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-dcp-num-pro       to   d-sld-ubi-num-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "ubi_prh"            to   h-nam                  .
           move      d-sld-ubi-cod-ubi    to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Tasto di reset                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_res' name='cel_res'>"               .
           display   "<button type='button' id='res' name='res'>"     .
           display   "<img src='../icons/gomma.png' width='70'>"      .
           display   "</button>"                                      .
           display   "</td>"                                          .
      *              *-------------------------------------------------*
      *              * Bottone di refresh                              *
      *              *-------------------------------------------------*
           display   "<td id='cel_ref' name='cel_ref'>"               .
           display   "<button type='button' "                         .
           display   "id='but_ref' name='but_ref' "                   .
           display   "/>"                                             .
           display   "<img src='../icons/refresh.png' width='40'>"    .
           display   "</button>"                                      .
           display   "</td>"                                          .
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
      *                  * Normalizzazione campi di input              *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-alf-pro          .
           move      zero                 to   w-exe-num-pro          .
           move      spaces               to   w-exe-ubi-qta          .
           move      zero                 to   w-exe-cnv-qta          .
           move      spaces               to   w-exe-acc-ub1          .
           move      spaces               to   w-exe-acc-ub2          .
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
      *              * Test iniziale se codice responsabile o codice   *
      *              * prodotto non ancora inseriti                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-cnv-rsm        =    zero
                     go to emi-tes-rsm-050.
______*    if        w-exe-cnv-rsm        =    zero or
______*              w-exe-alf-pro        =    spaces
______*              go to emi-tes-rsm-050.
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td id='cel_ute' name= 'cel_ute' align=center>" .
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
      *    * Subroutine per campi 'hidden'                             *
      *    *-----------------------------------------------------------*
       emi-tes-hid-000.
       emi-tes-hid-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-hid-999.
       emi-tes-hid-999.
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
           display   "<table class='bordotab' align='center' width=80% >
      -              ""                                               .
       emi-cor-100.
      *              *-------------------------------------------------*
      *              * Trattamento codice produttore, descrizione e    *
      *              * barcode                                         *
      *              *-------------------------------------------------*
           perform   emi-cor-cdb-000      thru emi-cor-cdb-999        .
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
      *              *-------------------------------------------------*
      *              * Test su codice prodotto                         *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to emi-cor-120.
      *              *-------------------------------------------------*
      *              * Tabella giacenze per ubicazione                 *
      *              *-------------------------------------------------*
           perform   emi-cor-tgu-000      thru emi-cor-tgu-999        .
       emi-cor-120.
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
           display   "<table class='bordotab' align='center' width=80% >
      -              ""                                               .
       emi-cor-150.
      *              *-------------------------------------------------*
      *              * Fincatura per la registrazione                  *
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
      *                  * Literal per Quantita'                       *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Quantita"           to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per di prelievo                     *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Ubicazione di prelievo"
                                          to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per di destinazione                 *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Ubicazione di destinazione"
                                          to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Conferma registrazione          *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Conferma"           to   h-alf                  .
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
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Accettazione Quantita' rilevata                 *
      *              *-------------------------------------------------*
           perform   emi-cor-qta-000      thru emi-cor-qta-999        .
      *              *-------------------------------------------------*
      *              * Accettazione Ubicazione di origine              *
      *              *-------------------------------------------------*
           perform   emi-cor-ub1-000      thru emi-cor-ub1-999        .
      *              *-------------------------------------------------*
      *              * Accettazione Ubicazione di destinazione         *
      *              *-------------------------------------------------*
           perform   emi-cor-ub2-000      thru emi-cor-ub2-999        .
      *              *-------------------------------------------------*
      *              * Bottone di conferma                             *
      *              *-------------------------------------------------*
           perform   emi-cor-cnf-000      thru emi-cor-cnf-999        .
       emi-cor-700.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
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
      *              * Box di dialogo per conferma                     *
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
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per codice produttore, descrizione e barcode   *
      *    *-----------------------------------------------------------*
       emi-cor-cdb-000.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           display   "<tr style='background-color: #EEEEEE;'>"        .
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
           display   "<td colspan=2 class='td_nb' align='center'>"    .
           display   "<b style='color:blue'>"                         .
           display   rf-aaq-cdp-pdt                                   .
           display   "</b>"                                           .
           display   "</td>"                                          .
      *              *-------------------------------------------------*
      *              * Descrizione                                     *
      *              *-------------------------------------------------*
           display   "<td colspan=2 class='td_nb' align='center'>"    .
           display   "<b style='color:blue'>"                         .
           display   rf-dcp-des-pro                                   .
           display   "</b>"                                           .
           display   "</td>"                                          .
       emi-cor-cdb-200.
      *              *-------------------------------------------------*
      *              * Trattamento barcode                             *
      *              *-------------------------------------------------*
           if        rf-dcp-klb-pro       =    spaces and
                     w-exe-alf-pro        =    spaces
                     move "<td colspan='1'  class='barcode'>&nbsp;</td>"
                                          to   w-exe-str-dsp
                     go to emi-cor-cdb-250.
           if        rf-dcp-klb-pro       =    spaces
                     move "<td colspan='1' class='barcode'>(senza barcod
      -              "e)</td>"
                                          to   w-exe-str-dsp
                     go to emi-cor-cdb-250.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<td colspan='1' align='center' class='barcode'>"
                                delimited by size
                     "<img border=0 src='./elebcd01?BAR=ean&COD="
                                delimited by size
                     rf-dcp-klb-pro
                                delimited by spaces
                     "'></td>"
                                delimited by size
                                          into w-exe-str-dsp          .
       emi-cor-cdb-250.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
           display   w-exe-str-dsp                                    .
       emi-cor-cdb-400.
      *              *-------------------------------------------------*
      *              * 'hidden' - Barcode prodotto                     *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "bcd_pro"            to   h-nam                  .
           move      rf-dcp-klb-pro       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * 'hidden' - Descrizione prodotto                 *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "des_pro"            to   h-nam                  .
           move      rf-dcp-des-pro       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-cdb-600.
      *              *-------------------------------------------------*
      *              * Ultimo movimento di cambio ubicazione           *
      *              *-------------------------------------------------*
______*    perform   emi-cor-umm-000      thru emi-cor-umm-999        .
       emi-cor-cdb-800.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-cdb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-cdb-999.
       emi-cor-cdb-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per tabella giacenze per ubicazione            *
      *    *-----------------------------------------------------------*
       emi-cor-tgu-000.
      *              *-------------------------------------------------*
      *              * Determinazione buffer giacenze per ubicazione   *
      *              *-------------------------------------------------*
           move      "PU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-dcp-num-pro       to   d-sld-ubi-num-mag      .
           move      spaces               to   d-sld-ubi-cod-ubi      .
           move      spaces               to   d-sld-ubi-lit-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione principale            *
      *              *-------------------------------------------------*
           move      "UP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-dcp-num-pro       to   d-sld-ubi-num-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Ubicazione principale                           *
      *              *-------------------------------------------------*
           move      d-sld-ubi-cod-ubi    to   w-exe-pri-ubi          .
       emi-cor-tgu-100.
      *              *-------------------------------------------------*
      *              * Apertura tabella per le righe                   *
      *              *-------------------------------------------------*
           display   "<table class='bordotab' id='ubi' name='ubi' width=
      -              "30% align='center'>"                            .
       emi-cor-tgu-150.
      *              *-------------------------------------------------*
      *              * Ciclo di stampa in base al numero di elementi   *
      *              * determinati                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio ciclo                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-gia-ubi-qta      .
           move      zero                 to   w-det-gia-ubi-ucc      .
           move      zero                 to   w-det-gia-ubi-ctr      .
       emi-cor-tgu-200.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    d-sld-ubi-num-ele
                     go to emi-cor-tgu-700.
      *                  *---------------------------------------------*
      *                  * Test se ubicazione a spazi                  *
      *                  *---------------------------------------------*
           if        d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   =    spaces
                     go to emi-cor-tgu-200.
       emi-cor-tgu-300.
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione tipo ubicazione          *
      *                      *-----------------------------------------*
           move      "TU"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   to   d-sld-ubi-cod-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
      *
           if        d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   =    w-exe-pri-ubi
                     move  "r"            to   h-stl
           else if   d-sld-ubi-exi-sts    not  = "N"
                     move  "g"            to   h-stl
           else      move  "B"            to   h-stl                  .
      *
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tgu-400.
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accumulo quantita'                          *
      *                  *---------------------------------------------*
           add       d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   to   w-det-gia-ubi-qta      .
      *                  *---------------------------------------------*
      *                  * Accumulo ubicazioni con giacenza            *
      *                  *---------------------------------------------*
           if        d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   not  = zero
           add       1                    to   w-det-gia-ubi-ucc      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      v-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "b"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tgu-420.
      *              *-------------------------------------------------*
      *              * Bottone Trash                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo ubicazione, giacenza o ubica-  *
      *                  * zione principale                            *
      *                  *---------------------------------------------*
           if        d-sld-ubi-exi-sts    not  = "N"
                     go to emi-cor-tgu-460.
           if        d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   not  = zero
                     go to emi-cor-tgu-460.
           if        d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   =    w-exe-pri-ubi
                     go to emi-cor-tgu-460.
       emi-cor-tgu-450.
      *                  *---------------------------------------------*
      *                  * Emissione bottone di cancellazione          *
      *                  *---------------------------------------------*
           display   "<td id='cel_del' name='cel_del' class='td_nb'>" .
           display   "<button type='button' id='del' name='del' class='d
      -              "el_btn'>"                                       .
           display   "<img src='../../icons/trash.png' width='30'>"   .
           display   "</button>"                                      .
           display   "</td>"                                          .
      *                  *---------------------------------------------*
      *                  * A chiusura riga                             *
      *                  *---------------------------------------------*
           go to     emi-cor-tgu-580.
       emi-cor-tgu-460.
      *                  *---------------------------------------------*
      *                  * Emissione cella vuota                       *
      *                  *---------------------------------------------*
           display   "<td class='td_nb'>&nbsp;</td>"                  .
      *                  *---------------------------------------------*
      *                  * A chiusura riga                             *
      *                  *---------------------------------------------*
           go to     emi-cor-tgu-580.
       emi-cor-tgu-580.
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tgu-600.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-cor-tgu-200.
       emi-cor-tgu-700.
      *              *-------------------------------------------------*
      *              * Totale quantita'                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero elementi                     *
      *                  *---------------------------------------------*
           if        d-sld-ubi-num-ele    <    2
                     go to emi-cor-tgu-800.
      *                  *---------------------------------------------*
      *                  * Test su numero ubicazioni con giacenza      *
      *                  *---------------------------------------------*
           if        w-det-gia-ubi-ucc    <    2
                     go to emi-cor-tgu-800.
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
           display   "<td colspan=1 class='td_nb' align='right'>"     .
           display   "<b style='color:green'>Totale</b></td>"         .
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      w-det-gia-ubi-qta    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<td colspan=1 class='td_nb' align='right'>"     .
           display   "<b style='color:green'>"                        .
           display   v-edt                                            .
           display   "</b></td>"                                      .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tgu-800.
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-tgu-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-tgu-999.
       emi-cor-tgu-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione quantita' verificata           *
      *    *-----------------------------------------------------------*
       emi-cor-qta-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_qta' name='cel_qta'>"               .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           display   "<input type='number' "                          .
           display   "id='ubi_qta' name ='ubi_qta' "                  .
           display   "class='numbersOnly' "                           .
      *              *-------------------------------------------------*
      *              * Stile                                           *
      *              *-------------------------------------------------*
           move      "style='text-align:right;font-weight:bold;' "
                                          to   w-all-str-cat (1)      .
           move      "style='text-align:right;font-weight:bold;;color : 
      -              "red;' "             to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Eventuale colore e stile                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "value='"  delimited by   size
                     w-exe-ubi-qta
                                delimited by   spaces
                     "' "       delimited by   size
                     "min='0' max='999999'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-qta-600.
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
       emi-cor-qta-700.
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-qta-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-qta-999.
       emi-cor-qta-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine di accettazione ubicazione di origine          *
      *    *-----------------------------------------------------------*
       emi-cor-ub1-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ub1' name='cel_ub1'>"               .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='acc_ub1' "
                                delimited by   size
                     "name='acc_ub1' "
                                delimited by   size
                     " class ='acc_ubi' value='"
                                delimited by   size
                     w-exe-acc-ub1
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='07' "
                                delimited by   size
                     "/>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-ub1-800.
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-ub1-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-ub1-999.
       emi-cor-ub1-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine di accettazione ubicazione di destinazione     *
      *    *-----------------------------------------------------------*
       emi-cor-ub2-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ub2' name='cel_ub2'>"               .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='acc_ub2' "
                                delimited by   size
                     "name='acc_ub2' "
                                delimited by   size
                     " class ='acc_ubi' value='"
                                delimited by   size
                     w-exe-acc-ub2
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='07' "
                                delimited by   size
                     "/>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       emi-cor-ub2-800.
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-ub2-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-ub2-999.
       emi-cor-ub2-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di gestione Non conformita' (annotazioni)      *
      *    *                                                           *
      *    * ___ CONFERMA ___                                          *
      *    *-----------------------------------------------------------*
       emi-cor-cnf-000.
      *              *-------------------------------------------------*
      *              * Cella per tasto conferma                        *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "I"                  to   h-sub                  .
           move      "cel_cnf"            to   h-idx                  .
           move      "cel_cnf"            to   h-nam                  .
           move      "td_nb"              to   h-cla                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Bottone                                         *
      *              *-------------------------------------------------*
           display   "<button type='button' id='but_cnf' name='but_cnf' 
      -              ">"                                              .
      *              *-------------------------------------------------*
      *              * Immagine bottone                                *
      *              *-------------------------------------------------*
           move      "IM"                 to   h-ope                  .
           move      "../icons/check.png" to   h-src                  .
           move      "40"                 to   h-wdt                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura bottone                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "button"             to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-cnf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-cnf-999.
       emi-cor-cnf-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *-----------------------------------------------------------*
       emi-pie-000.
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
           move      "../ele.html"        to   h-hrf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Immagine                                        *
      *              *-------------------------------------------------*
           move      "IM"                 to   h-ope                  .
           move      "../icons/next-blue-1.png"
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
       det-num-pro-650.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode confezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           move      "B"                  to   w-det-num-pro-tip      .
           perform   det-num-pro-pdk-000  thru det-num-pro-pdk-999    .
      *                  *---------------------------------------------*
      *                  * Test se codice prodotto determinato         *
      *                  *---------------------------------------------*
           if        w-det-num-pro-num    not  = zero
                     go to det-num-pro-900.
       det-num-pro-700.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode imballo             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           move      "D"                  to   w-det-num-pro-tip      .
           perform   det-num-pro-pdk-000  thru det-num-pro-pdk-999    .
           if        w-det-num-pro-num    not  = zero
                     go to det-num-pro-900.
       det-num-pro-800.
      *              *-------------------------------------------------*
      *              * Eventuale scansione codice per il produttore    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-aaq-000  thru det-num-pro-aaq-999    .
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
      *    * Subroutine per ricerca su file di chiavi                  *
      *    *-----------------------------------------------------------*
       det-num-pro-pdk-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [pdk]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-pdk-alf-pro         .
           move      w-det-num-pro-tip    to   rf-pdk-tip-rec         .
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
                     go to det-num-pro-pdk-900.
       det-num-pro-pdk-200.
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
                     go to det-num-pro-pdk-900.
       det-num-pro-pdk-300.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-pdk-900.
           if        rf-pdk-tip-rec       not  = w-det-num-pro-tip
                     go to det-num-pro-pdk-900.
       det-num-pro-pdk-400.
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
       det-num-pro-pdk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-pdk-999.
       det-num-pro-pdk-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *                                                           *
      *    * Subroutine per ricerca su codice del produttore           *
      *    *-----------------------------------------------------------*
       det-num-pro-aaq-000.
      *              *-------------------------------------------------*
      *              * Start su file [aaq]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CDPPDT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-num-pro-alf    to   rf-aaq-cdp-pdt         .
           move      01                   to   rf-aaq-tip-mag         .
           move      zero                 to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se errata : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-aaq-900.
       det-num-pro-aaq-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [aaq]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-aaq-900.
       det-num-pro-aaq-300.
      *              *-------------------------------------------------*
      *              * Test max su [aaq]                               *
      *              *-------------------------------------------------*
           if        rf-aaq-cdp-pdt       not  = w-det-num-pro-alf
                     go to det-num-pro-aaq-900.
           if        rf-aaq-tip-mag       not  = 01
                     go to det-num-pro-aaq-900.
       det-num-pro-aaq-400.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico individuato                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-aaq-num-pro       to   rf-dcp-num-pro         .
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
       det-num-pro-aaq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-aaq-999.
       det-num-pro-aaq-999.
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
      *              * Codice responsabile                             *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "rsp_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-rsm
      *              *-------------------------------------------------*
      *              * Descrizione responsabile                        *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "rsp_des"
                     move  w-all-str-cat (2)
                                          to   w-exe-des-rsm
      *              *-------------------------------------------------*
      *              * Codice alfanumerico prodotto                    *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "alf_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-alf-pro
      *              *-------------------------------------------------*
      *              * Quantita'                                       *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "ubi_qta"
                     move  w-all-str-cat (2)
                                          to   w-exe-ubi-qta
      *              *-------------------------------------------------*
      *              * Ubicazione di origine                           *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "acc_ub1"
                     move  w-all-str-cat (2)
                                          to   w-exe-acc-ub1
      *              *-------------------------------------------------*
      *              * Ubicazione di destinazione                      *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "acc_ub2"
                     move  w-all-str-cat (2)
                                          to   w-exe-acc-ub2          .
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
