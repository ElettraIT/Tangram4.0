       Identification Division.
       Program-Id.                                 eleubi02           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    eleubi              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 23/05/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Situazione ubicazione - ELETTRA             *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-cod-ubi              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati                       *
      *        *-------------------------------------------------------*
           05  w-exe-pri-ubi              pic  x(07)                  .
           05  w-exe-qta-ubi              pic  x(13)                  .
           05  w-exe-ele-ubi              pic  9(03)                  .
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
      *        * Work per Det su giacenze per ubicazione               *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-ctr      pic  9(04)                  .
               10  w-det-gia-ubi-ctx      pic  9(04)                  .
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
           move      spaces               to   w-exe-cod-ubi          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      02                   to   w-cgi-str-num          .
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
      *                  *---------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      03                   to   v-car                  .
           move      w-exe-cod-rsm        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exe-cnv-rsm          .
      *                  *---------------------------------------------*
      *                  * Codice ubicazione                           *
      *                  *---------------------------------------------*
           move      w-exe-cod-ubi        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-cod-ubi          .
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
           move      "* .....Aggiornamento : 08/02/2024 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "{ELETTRA} Ubicazione"
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
      *              * Jquery redirect                                 *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "jquery.redirect.js" to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Javascript per il documento                     *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../jsc_ele/"        to   h-prm                  .
           move      "eleubi02.js"        to   h-alf                  .
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
           display   "<form name='ubi_002' id='ubi_002' method='post' ac
      -              "tion=''>"                                       .
      *              *-------------------------------------------------*
      *              * Separatore                                      *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "hr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
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
      *              * Eventuale responsabile                          *
      *              *-------------------------------------------------*
           perform   emi-rsp-mag-000      thru emi-rsp-mag-999        .
      *              *-------------------------------------------------*
      *              * Codice ubicazione                               *
      *              *-------------------------------------------------*
           perform   emi-tes-ubi-000      thru emi-tes-ubi-999        .
      *              *-------------------------------------------------*
      *              * Bottone di refresh                              *
      *              *-------------------------------------------------*
           display   "<td class='td_nb' id='cel_ref' name='cel_ref'>" .                  .
           display   "<button type='button' "                         .
           display   "id='but_ref' name='but_ref' "                   .
           display   "/>"                                             .
           display   "<img src='../icons/refresh.png' width='40'>"    .
           display   "</button>"                                      .
           display   "</td>"                                          .
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td class ='td_nb'>"                            .
      *              *-------------------------------------------------*
      *              * Bottone di ritorno a menu                       *
      *              *-------------------------------------------------*
           perform   emi-htm-bck-000      thru emi-htm-bck-999        .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
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
      *              * Test su codice ubicazione                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        not  = spaces
                     go to emi-tes-800.
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           display   "<hr>"                                           .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "INSERIRE UNA UBICAZIONE !"
                                          to   w-exe-err-msg          .
           move      "msg_red"            to   w-exe-cla-msg          .
           perform   err-msg-htm-000      thru err-msg-htm-999        .
       emi-tes-800.
      *              *-------------------------------------------------*
      *              * 'hidden' - Emissione codice responsabile        *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "rsp_doc"            to   h-nam                  .
           move      w-exe-cod-rsm        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Inizio corpo                                    *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
       emi-tes-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *                                                           *
      *    * Subroutine per Responsabile                               *
      *    *-----------------------------------------------------------*
       emi-rsp-mag-000.
      *              *-------------------------------------------------*
      *              * Test su codice responsabile                     *
      *              *-------------------------------------------------*
           if        w-exe-cnv-rsm        not  = zero
                     go to emi-rsp-mag-200.
      *              *-------------------------------------------------*
      *              * Responsabile non identificato                   *
      *              *-------------------------------------------------*
           move      "(Operatore)"        to   w-let-arc-zrm-des      .
      *              *-------------------------------------------------*
      *              * A visualizzazione                               *
      *              *-------------------------------------------------*
           go to     emi-rsp-mag-400.
       emi-rsp-mag-200.
      *              *-------------------------------------------------*
      *              * Lettura descrizione responsabile                *
      *              *-------------------------------------------------*
           move      w-exe-cnv-rsm        to   w-let-arc-zrm-cod      .
           perform   let-arc-zrm-000      thru let-arc-zrm-999        .
       emi-rsp-mag-400.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ute' name='cel_ute' align=center class
      -              "='td_nb'>"                                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione responsabile        *
      *              *-------------------------------------------------*
           display   "<b>"                                            .
           display   w-let-arc-zrm-des                                .
           display   "</b>"                                           .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-rsp-mag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-rsp-mag-999.
       emi-rsp-mag-999.
           exit.

      *    *===========================================================*
      *    * Emissione testata documento                               *
      *    *                                                           *
      *    * Subroutine per Codice ubicazione                          *
      *    *-----------------------------------------------------------*
       emi-tes-ubi-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ubi' name='cel_ubi' align=center>"  .
       emi-tes-ubi-200.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Ubicazione </h1>"                          .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='cod_ubi' name='cod_ubi' "
                                delimited by   size
                     "value='"
                                delimited by   size
                     w-exe-cod-ubi
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='10'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-tes-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-ubi-999.
       emi-tes-ubi-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-exe-cod-ubi        =    spaces
                     go to emi-cor-999.
      *              *-------------------------------------------------*
      *              * Apertura tabella                                *
      *              *-------------------------------------------------*
           display   "<table class='bordotab' align='center' width=80% i
      -              "d='ubi' name='ubi'>"                            .
      *              *-------------------------------------------------*
      *              * Determinazione buffer prodotti in ubicazione    *
      *              *-------------------------------------------------*
           move      "GU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      w-exe-cod-ubi        to   d-sld-ubi-cod-ubi      .
           move      01                   to   d-sld-ubi-tip-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione                          *
      *              *-------------------------------------------------*
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
       emi-cor-800.
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Box di dialogo per lista ordini                 *
      *              *-------------------------------------------------*
           display   "<div id='dialog' name='dialog' title=''>"       .                                 .
           display   "</div>"                                         .
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per ubicazione                                 *
      *    *-----------------------------------------------------------*
       emi-cor-ubi-000.
      *              *-------------------------------------------------*
      *              * Ciclo di stampa in base al numero di elementi   *
      *              * determinati                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio ciclo                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-gia-ubi-ctr      .
           move      zero                 to   w-det-gia-ubi-ctx      .
       emi-cor-ubi-200.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    d-sld-ubi-num-pro
                     go to emi-cor-ubi-900.
           if        w-det-gia-ubi-ctr    >    d-sld-ubi-max-pro
                     go to emi-cor-ubi-900.
      *                  *---------------------------------------------*
      *                  * Determinazione giacenza                     *
      *                  *---------------------------------------------*
           move      "PU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      d-sld-ubi-pro-ubi
                    (w-det-gia-ubi-ctr)   to   d-sld-ubi-num-mag      .
           move      w-exe-cod-ubi        to   d-sld-ubi-cod-ubi      .
           move      spaces               to   d-sld-ubi-lit-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                  *---------------------------------------------*
      *                  * Test se giacenza prodotto a zero            *
      *                  *---------------------------------------------*
           if        d-sld-ubi-qta-gia (01)
                                          =    zero
                     go to  emi-cor-ubi-200.
       emi-cor-ubi-300.
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-gia-ubi-ctx      .
      *                  *---------------------------------------------*
      *                  * Editing contatore                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      w-det-gia-ubi-ctx    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           go to     emi-cor-ubi-350.
      *
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      p-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "g"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-ubi-350.
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcp]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcp]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      d-sld-ubi-pro-ubi
                    (w-det-gia-ubi-ctr)   to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Prodotto                                    *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      rf-dcp-alf-pro       to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      rf-dcp-des-pro       to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "g"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto 'hidden'       *
      *                  *---------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "alf"                to   h-nam                  .
           move      rf-dcp-alf-pro       to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-ubi-400.
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      d-sld-ubi-qta-gia
                    (01)                  to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      p-edt                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "b"                  to   h-stl                  .
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
       emi-cor-ubi-600.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-cor-ubi-200.
       emi-cor-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-cor-ubi-999.
       emi-cor-ubi-999.
           exit.

      *    *===========================================================*
      *    * Emissione piede documento                                 *
      *    *-----------------------------------------------------------*
       emi-pie-000.
      *              *-------------------------------------------------*
      *              * Linea vuota                                     *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "br"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Allineamento centrato                           *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "center"             to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Box per bottone di ritorno a menu               *
      *              *-------------------------------------------------*
           perform   emi-htm-bck-000      thru emi-htm-bck-999        .
       emi-pie-800.
      *              *-------------------------------------------------*
      *              * Copyright                                       *
      *              *-------------------------------------------------*
           move      "CY"                 to   h-ope                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura allineamento centrato                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "center"             to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura 'body'                                 *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "body"               to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura 'html'                                 *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "html"               to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
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
      *              * Codice ubicazione                               *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ubi          .
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
