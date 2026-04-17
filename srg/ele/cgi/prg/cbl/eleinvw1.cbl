       Identification Division.
       Program-Id.                                 eleinvw1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/06/23    *
      *                       Ultima revisione:    NdK del 23/11/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni di scrittura per inventario      *
      *                                                                *
      *                    Modifiche anagrafica responsabili           *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .

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
           05  w-exe-tip-ope              pic  x(02)                  .
           05  w-exe-cod-rsm              pic  x(03)                  .
           05  w-exe-cnv-rsm              pic  9(03)                  .
           05  w-exe-cod-col              pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
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

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zrm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zrm.
               10  w-let-arc-zrm-flg      pic  x(01)                  .
               10  w-let-arc-zrm-dpz      pic  9(02)                  .
               10  w-let-arc-zrm-cod      pic  x(07)                  .
               10  w-let-arc-zrm-des      pic  x(30)                  .
               10  w-let-arc-zrm-inx      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area routine di trattamento variabile POST           *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cpw"                   .

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
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      03                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-cod-rsm          .
           move      spaces               to   w-exe-cod-col          .
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
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Responsabile documento                      *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      03                   to   p-car                  .
           move      w-exe-cod-rsm        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-cnv-rsm          .
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
      *              * [zrm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
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
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-exe-tip-ope        =    "CR"
                     perform exe-cph-wcr-000
                                          thru exe-cph-wcr-999
           else      perform exe-cph-err-000
                                          thru exe-cph-err-999        .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di aggiornamento colore responsabile           *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice responsabile                 *
      *                  *---------------------------------------------*
           if        w-exe-cnv-rsm        not  = zero
                     go to exe-cph-wcr-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-900.
       exe-cph-wcr-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [zrm]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *              *-------------------------------------------------*
      *              * Ottenimento record [zrm]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "CODRSP    "         to   f-key                  .
           move      w-exe-cnv-rsm        to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad uscita            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wcr-600.
       exe-cph-wcr-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "#"                  to   w-all-str-cat (1)      .
           move      w-exe-cod-col        to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   rf-zrm-alx-exp         .
       exe-cph-wcr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [zrm]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-zrm-000      thru upd-rec-zrm-999        .
       exe-cph-wcr-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-900.
       exe-cph-wcr-600.
      *              *-------------------------------------------------*
      *              * Se record [zrm] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-900.
       exe-cph-wcr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-999.
       exe-cph-wcr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per messaggio di aggiornamento                 *
      *    *-----------------------------------------------------------*
       exe-cph-agg-000.
      *              *-------------------------------------------------*
      *              * ATTUALMENTE INIBITO                             *
      *              *-------------------------------------------------*
           go to     exe-cph-agg-900.
       exe-cph-agg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-agg-999.
       exe-cph-agg-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di uscita per errore                           *
      *    *-----------------------------------------------------------*
       exe-cph-err-000.
      *              *-------------------------------------------------*
      *              * Preparazione messaggio                          *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           
           move      "<h1> Tipo operazione :"
                                          to   w-all-str-cat (1)      .
           move      w-exe-tip-ope        to   w-all-str-cat (2)      .
           move      "non riconosciuto </h1>"
                                          to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-prm-msg          .
      *              *-------------------------------------------------*
      *              * Messaggio in uscita                             *
      *              *-------------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   w-exe-prm-msg                                    .
           display   "</div>"                                         .
       exe-cph-err-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-err-999.
       exe-cph-err-999.
           exit.

      *    *===========================================================*
      *    * Routine di update record [zrm]                            *
      *    *-----------------------------------------------------------*
       upd-rec-zrm-000.
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [zrm]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
       upd-rec-zrm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome elemento        *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "tip_ope"
                     move  w-all-str-cat (2)
                                          to   w-exe-tip-ope
      *              *-------------------------------------------------*
      *              * Responsabile                                    *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "rsp_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-rsm
      *              *-------------------------------------------------*
      *              * Colore                                          *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_col"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-col          .
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
