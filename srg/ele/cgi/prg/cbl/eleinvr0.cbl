       Identification Division.
       Program-Id.                                 eleinvr0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/11/24    *
      *                       Ultima revisione:    NdK del 23/11/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione Inventario                         *
      *                                                                *
      *                    Cambio colori responsabili per grafici      *
      *                                                                *
      *                    VERSIONE con Color Picker ___ NON IN USO __ *
      *                                                                *
      *                    ELETTRA                                     *
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
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Stringhe di comodo per display                        *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
           05  w-exe-str-cls              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella responsabili               *
      *        *-------------------------------------------------------*
           05  w-det-tbl-rsm.
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-cod-ele  occurs  32.
                   15  w-det-tbl-cod-rsp  pic  9(05)                  .
                   15  w-det-tbl-des-rsp  pic  x(40)                  .
                   15  w-det-tbl-col-rsp  pic  x(12)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-rsm-max      pic  9(03) value 32         .
               10  w-det-tbl-rsm-ctr      pic  9(03)                  .
               10  w-det-tbl-rsm-inx      pic  9(03)                  .
               10  w-det-tbl-rsm-in3      pic  9(01)                  .

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
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      1                    to   w-cgi-str-num          .
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
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni parametri estratti             *
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
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
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
           move      "* .....Aggiornamento : 22/11/2024 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "Colori responsabili" 
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * CSS - Jquery UI                                 *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../../ele/css/"     to   h-prm                  .
           move      "jquery-ui.min.css"  to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * CSS - documento                                 *
      *              *-------------------------------------------------*
           move      "HC"                 to   h-ope                  .
           move      "../../ele/css/"     to   h-prm                  .
           move      "ele.css"            to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *=================================================*
      *              * JSC - Jquery min                                *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "jquery.min.js"      to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * JSC - Jquery UI min                             *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "jquery-ui.min.js"   to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * JSC - documento                                 *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "eleinvr0.js"        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Head - fine                                     *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "head"               to   h-tag                  .
           move      "C"                  to   h-sub                  .
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
      *              * Form                                            *
      *              *-------------------------------------------------*
           display   "<form name='col_rsp' id='col_rsp' method='post' ac
      -              "tion=''>"                                       .
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Tabella responsabili (3 colonne)                *
      *              *-------------------------------------------------*
           perform   exe-cph-rsm-000      thru exe-cph-rsm-999        .
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Color picker                                    *
      *              *-------------------------------------------------*
           perform   exe-cph-col-000      thru exe-cph-col-999        .
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Chiusura form                                   *
      *              *-------------------------------------------------*
           display   "</form>"                                        .
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
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine per accettazione Responsabile (Operatore)      *
      *    *-----------------------------------------------------------*
       exe-cph-rsm-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella responsabili           *
      *              *-------------------------------------------------*
           display   "<table id='rsp' name='rsp' class='bordotab' width=
      -              "90%>"                                           .
       exe-cph-rsm-100.
      *              *-------------------------------------------------*
      *              * Costruzione tabella responsabili                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatori                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-rsm-inx      .
           move      zero                 to   w-det-tbl-rsm-in3      .
       exe-cph-rsm-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-rsm-inx      .
           add       1                    to   w-det-tbl-rsm-in3      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-in3    >    3
                     move  1              to   w-det-tbl-rsm-in3      .
           if        w-det-tbl-rsm-inx    >    w-det-tbl-rsm-ctr
                     go to exe-cph-rsm-900.
           if        w-det-tbl-rsm-inx    >    w-det-tbl-rsm-max
                     go to exe-cph-rsm-900.
       exe-cph-rsm-300.
      *              *-------------------------------------------------*
      *              * Test se nuova riga                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-in3    not  = 1
                     go to exe-cph-rsm-320.
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-rsm-320.
      *                  *---------------------------------------------*
      *                  * Editing codice responsabile                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-det-tbl-cod-rsp
                    (w-det-tbl-rsm-inx)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Trattamento parametro 'colspan'             *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-inx    =    w-det-tbl-rsm-ctr and
                     w-det-tbl-rsm-in3    =    1
                     move  "3"            to   w-exe-str-cls
           else if   w-det-tbl-rsm-inx    =    w-det-tbl-rsm-ctr and
                     w-det-tbl-rsm-in3    =    2
                     move  "2"            to   w-exe-str-cls
           else      move  "1"            to   w-exe-str-cls          .
      *                  *---------------------------------------------*
      *                  * Emissione elemento                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<td id='"
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     "' name='"
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     "' class='nom_rsp' colspan='"
                                delimited by   size
                     w-exe-str-cls
                                delimited by   size
                     "' align='center' style='background-color: "
                                delimited by   size
                     w-det-tbl-col-rsp
                    (w-det-tbl-rsm-inx)
                                delimited by   spaces
                     ";' height='80'><h5>"
                                delimited by   size
                     w-det-tbl-des-rsp
                    (w-det-tbl-rsm-inx)
                                delimited by   size
                     "</h5></td>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-rsm-500.
      *              *-------------------------------------------------*
      *              * Test se chiusura riga                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-rsm-in3    not  = 3
                     go to exe-cph-rsm-800.
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-rsm-800.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-rsm-200.
       exe-cph-rsm-900.
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-rsm-999.
       exe-cph-rsm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine per Color Picker                               *
      *    *-----------------------------------------------------------*
       exe-cph-col-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella                        *
      *              *-------------------------------------------------*
           display   "<table id='col' name='col' class='bordotab' width=
      -              "90%>"                                           .
       exe-cph-col-100.
      *              *-------------------------------------------------*
      *              * Costruzione Color Picker                        *
      *              *                                                 *
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Regolatori colore                               *
      *              *-------------------------------------------------*
           display   "<div id='red' name='red'></div>"                .
           display   "<div id='green' name='green'></div>"            .
           display   "<div id='blue' name='blue'></div>"              .
      *              *-------------------------------------------------*
      *              * Tavolozza colore                                *
      *              *-------------------------------------------------*
           display   "<div id='swatch' name='red'class='ui-widget-conten
      -              "t ui-corner-all'></div>"                        .
      *              *-------------------------------------------------*
      *              * Colore selezionato                              *
      *              *-------------------------------------------------*
           display   "<div id='cod_col' name='cod_col'></div>"        .
       exe-cph-col-200.
      *              *-------------------------------------------------*
      *              * Accettazione campo testo                        *
      *              *-------------------------------------------------*
      
______*    ATTUALMENTE NON IMPLEMENTATO ___
      
           go to     exe-cph-col-300.
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='cod_col' name='cod_col' "
                                delimited by   size
                     "class='inp_ncf' "
                                delimited by   size
                     "value=''"
                                delimited by   size
                     """ maxlength='7'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
           
       exe-cph-col-300.
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-col-400.
      *              *-------------------------------------------------*
      *              * Box per codice responsabile                     *
      *              *-------------------------------------------------*
           display   "<div id='cod_rsp' name='cod_rsp'></div>"        .
      *              *-------------------------------------------------*
      *              * Box per decrizione responsabile                 *
      *              *-------------------------------------------------*
           display   "<div id='snx_sel' name='snx_sel'></div>"        .
           display   "<div id='des_rsp' name='des_rsp'></div>"        .
       exe-cph-col-500.
      *              *-------------------------------------------------*
      *              * Bottone di conferma                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td class='td_nb'>"                             .
      *                  *---------------------------------------------*
      *                  * Bottone                                     *
      *                  *---------------------------------------------*
           display   "<button type='button' id='but_fip' name='but_fip'>
      -              ""                                               .
      *                  *---------------------------------------------*
      *                  * Bottone - immagine                          *
      *                  *---------------------------------------------*
           move      "IM"                 to   h-ope                  .
           move      "../../icons/check.png"
                                          to   h-src                  .
           move      "40"                 to   h-wdt                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
           display   "</button>"                                      .
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "td"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-col-600.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-col-900.
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-col-999.
       exe-cph-col-999.
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
                                          to   w-exe-cod-rsm          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
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
           move      rf-zrm-alx-exp       to   w-det-tbl-col-rsp
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
