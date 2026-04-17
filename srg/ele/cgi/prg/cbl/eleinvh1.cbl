       Identification Division.
       Program-Id.                                 eleinvh1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 19/11/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione Inventario                         *
      *                                                                *
      *                    Preparazione HTML per grafici               *
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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
           05  w-exe-tip-grf              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-ele              pic  9(05)                  .
           05  w-exe-ctr-elw              pic  x(05)                  .
           05  w-exe-ctr-elp              pic  x(11)                  .
           05  w-exe-ctr-elc              pic  x(07)                  .
           05  w-exe-ctr-spn              pic  9(05)                  .
           05  w-exe-ctr-spw              pic  x(05)                  .
           05  w-exe-ctr-spp              pic  x(08)                  .
           05  w-exe-ctr-spc              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .

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
           move      spaces               to   w-exe-tip-grf          .
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
           move      "* .....Aggiornamento : 19/11/2024 *"
                                          to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Titolo documento                                *
      *              *-------------------------------------------------*
           move      "HT"                 to   h-ope                  .
           move      "Grafico inventario" 
                                          to   h-alf                  .
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
      *              * JSC - Chart                                     *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "chart.js"           to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * JSC - documento                                 *
      *              *-------------------------------------------------*
           move      "HJ"                 to   h-ope                  .
           move      "../../ele/jsc/"     to   h-prm                  .
           move      "eleinvh0.js"        to   h-alf                  .
______*    move      "eleinvh0_SAV.js"    to   h-alf                  .
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
           display   "<form method='post' action=''>"                 .
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Prompt per tipo di grafico                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo grafico     *
      *                  *---------------------------------------------*
           if        w-exe-tip-grf        =    "grf_not"
                     move  "Prodotti con note"
                                          to   w-all-str-cat (2)
           else if   w-exe-tip-grf        =    "grf_spn"
                     move  "Prodotti con visto del magazzino"
                                          to   w-all-str-cat (2)
           else if   w-exe-tip-grf        =    "grf_vst"
                     move  "Prodotti con spunta supervisione"
                                          to   w-all-str-cat (2)
           else if   w-exe-tip-grf        =    "grf_rsp"
                     move  "Visto magazzinieri"
                                          to   w-all-str-cat (2)
           else if   w-exe-tip-grf        =    "grf_rsq"
                     move  "Visto magazzinieri - quantita'"
                                          to   w-all-str-cat (2)
           else if   w-exe-tip-grf        =    "grf_ute"
                     move  "Spunte utenti supervisori"
                                          to   w-all-str-cat (2)
           else      move  "Grafico XXXXXXXX"
                                          to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<h5>"               to   w-all-str-cat (1)      .
           move      "</h5>"              to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   w-all-str-alf                                    .
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Linea di separazione                            *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
      *              *-------------------------------------------------*
      *              * Interlinee                                      *
      *              *-------------------------------------------------*
           display   "<br>"                                           .
      *              *-------------------------------------------------*
      *              * Campo 'hidden'                                  *
      *              *-------------------------------------------------*
           move      "IH"                 to   h-ope                  .
           move      "tip_grf"            to   h-nam                  .
           move      w-exe-tip-grf        to   h-alf                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *              *-------------------------------------------------*
      *              * Chiusura form                                   *
      *              *-------------------------------------------------*
           display   "</form>"                                        .
      *              *-------------------------------------------------*
      *              * Grafico                                         *
      *              *-------------------------------------------------*
           display   "<canvas id='grf_pie' width='500' height='500'>" .
      *              *-------------------------------------------------*
      *              * Chiusura Grafico                                *
      *              *-------------------------------------------------*
           display   "</canvas>"                                      .
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
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "tip_grf"
                     move  w-all-str-cat (2)
                                          to   w-exe-tip-grf          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
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
