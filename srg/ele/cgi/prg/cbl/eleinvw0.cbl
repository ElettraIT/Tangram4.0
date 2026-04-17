       Identification Division.
       Program-Id.                                 eleinvw0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    inv                 *
      *                                   Fase:    eleinv              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/06/23    *
      *                       Ultima revisione:    NdK del 04/01/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni di scrittura per inventario      *
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
      *        * [miu]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmiu"                          .
      *        *-------------------------------------------------------*
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .

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
           05  w-exe-cod-ute              pic  x(08)                  .
           05  w-exe-cod-fas              pic  x(06)                  .
           05  w-exe-cod-ubi              pic  x(07)                  .
           05  w-exe-qta-prs              pic  x(12)                  .
           05  w-exe-qta-rlv              pic  x(12)                  .
           05  w-exe-klb-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  x(07)                  .
           05  w-exe-flg-spn              pic  x(01)                  .
           05  w-exe-flg-spv              pic  x(01)                  .
           05  w-exe-ann-rlv              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-fld  occurs 20   pic  x(90)                  .
           05  w-exe-prm-ctr              pic  9(02)                  .
           05  w-exe-prm-max              pic  9(02) value 20         .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qtp-cnv              pic s9(10)v9(03)            .
           05  w-exe-qtr-cnv              pic s9(10)v9(03)            .
           05  w-exe-pro-cnv              pic  9(07)                  .
           05  w-exe-rsm-cnv              pic  9(03)                  .
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
      *        * Work per Let su archivio [zub]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zub.
               10  w-let-arc-zub-flg      pic  x(01)                  .
               10  w-let-arc-zub-dpz      pic  9(02)                  .
               10  w-let-arc-zub-cod      pic  x(07)                  .
               10  w-let-arc-zub-des      pic  x(30)                  .
               10  w-let-arc-zub-inx      pic  9(07)                  .

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
           move      10                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-cod-rsm          .
           move      spaces               to   w-exe-cod-ute          .
           move      spaces               to   w-exe-cod-fas          .
           move      spaces               to   w-exe-cod-ubi          .
           move      spaces               to   w-exe-num-pro          .
           move      spaces               to   w-exe-qta-prs          .
           move      spaces               to   w-exe-qta-rlv          .
           move      spaces               to   w-exe-flg-spn          .
           move      spaces               to   w-exe-flg-spv          .
           move      spaces               to   w-exe-ann-rlv          .
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
           move      p-num                to   w-exe-rsm-cnv          .
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    spaces
                     move  "resp_mag"     to   w-exe-cod-ute          .
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           if        w-exe-cod-fas        =    spaces
                     move  "eleinv"      to   w-exe-cod-fas          .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-cod-ubi        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-cod-ubi          .
      *                  *---------------------------------------------*
      *                  * Quantita' presunta                          *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qta-prs        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qtp-cnv          .
      *                  *---------------------------------------------*
      *                  * Quantita' rilevata                          *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qta-rlv        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qtr-cnv          .
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      07                   to   p-car                  .
           move      w-exe-num-pro        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-pro-cnv          .
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
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
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
      *              * [zub]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
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
           if        w-exe-tip-ope        =    "WI"
                     perform exe-cph-wri-000
                                          thru exe-cph-wri-999
           else if   w-exe-tip-ope        =    "WN"
                     perform exe-cph-wni-000
                                          thru exe-cph-wni-999
           else if   w-exe-tip-ope        =    "WD"
                     perform exe-cph-wdl-000
                                          thru exe-cph-wdl-999
           else if   w-exe-tip-ope        =    "WR"
                     perform exe-cph-win-000
                                          thru exe-cph-win-999
           else if   w-exe-tip-ope        =    "WS"
                     perform exe-cph-wsv-000
                                          thru exe-cph-wsv-999
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
      *    * Subroutine di scrittura riga [miu]                        *
      *    *-----------------------------------------------------------*
       exe-cph-wri-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-pro-cnv        not  = zero
                     go to exe-cph-wri-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wri-900.
       exe-cph-wri-050.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        not  = spaces
                     go to exe-cph-wri-060
           else      go to exe-cph-wri-070.
       exe-cph-wri-060.
      *                  *---------------------------------------------*
      *                  * Lettura di controllo                        *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-cod-ubi        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-wri-100.
       exe-cph-wri-070.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wri-900.
       exe-cph-wri-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [miu]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Ottenimento record [miu]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad uscita            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wri-600.
       exe-cph-wri-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "#"            to   rf-miu-flg-rlv
           else      move  spaces         to   rf-miu-flg-rlv         .
      *
           move      w-exe-rsm-cnv        to   rf-miu-cod-rsm         .
           move      w-exe-qtr-cnv        to   rf-miu-qta-rlv         .
           move      w-exe-dat-exe        to   rf-miu-ide-dat         .
           move      w-exe-cod-ute        to   rf-miu-ide-ute         .
           move      w-exe-cod-fas        to   rf-miu-ide-fas         .
       exe-cph-wri-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [miu]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-miu-000      thru upd-rec-miu-999        .
       exe-cph-wri-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wri-900.
       exe-cph-wri-600.
      *              *-------------------------------------------------*
      *              * Se record [miu] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad inserimento                              *
      *                  *---------------------------------------------*
           perform   ins-rec-miu-000      thru ins-rec-miu-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wri-900.
       exe-cph-wri-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wri-999.
       exe-cph-wri-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura riga [miu] per le annotazioni     *
      *    *-----------------------------------------------------------*
       exe-cph-wni-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-pro-cnv        not  = zero
                     go to exe-cph-wni-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wni-900.
       exe-cph-wni-050.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        =    spaces
                     go to exe-cph-wni-100.
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-cod-ubi        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-wni-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wni-900.
       exe-cph-wni-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [miu]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Ottenimento record [miu]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad inserimento       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wni-600.
       exe-cph-wni-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-ann-rlv        to   rf-miu-ann-rlv         .
       exe-cph-wni-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [miu]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-miu-000      thru upd-rec-miu-999        .
       exe-cph-wni-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wni-900.
       exe-cph-wni-600.
      *              *-------------------------------------------------*
      *              * Se record [miu] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad inserimento                              *
      *                  *---------------------------------------------*
           perform   ins-rec-miu-000      thru ins-rec-miu-999        .
       exe-cph-wni-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wni-999.
       exe-cph-wni-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di cancellazione riga [miu]                    *
      *    *-----------------------------------------------------------*
       exe-cph-wdl-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-pro-cnv        not  = zero
                     go to exe-cph-wdl-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE CANCELLARE IL RECORD !</h1>"    .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wdl-900.
       exe-cph-wdl-050.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        =    spaces
                     go to exe-cph-wdl-100.
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-cod-ubi        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-wdl-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE CANCELLARE IL RECORD !</h1>"    .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wdl-900.
       exe-cph-wdl-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [miu]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Lettura record [miu]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad uscita            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wdl-900.
       exe-cph-wdl-800.
      *              *-------------------------------------------------*
      *              * Delete record                                   *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       exe-cph-wdl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wdl-999.
       exe-cph-wdl-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di inserimento riga [miu]                      *
      *    *-----------------------------------------------------------*
       exe-cph-win-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-pro-cnv        not  = zero
                     go to exe-cph-win-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE INSERIRE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-win-900.
       exe-cph-win-050.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        =    spaces
                     go to exe-cph-win-100.
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-cod-ubi        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-win-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE INSERIRE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-win-900.
       exe-cph-win-100.
      *              *-------------------------------------------------*
      *              * Lettura record [miu]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se record trovato: ad uscita                *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to exe-cph-win-900.
       exe-cph-win-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [miu]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      01                   to   rf-miu-cod-dpz         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       exe-cph-win-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-win-999.
       exe-cph-win-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura riga [miu] per la supervisione    *
      *    *-----------------------------------------------------------*
       exe-cph-wsv-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           if        w-exe-pro-cnv        not  = zero
                     go to exe-cph-wsv-050.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wsv-900.
       exe-cph-wsv-050.
      *                  *---------------------------------------------*
      *                  * Test su codice ubicazione                   *
      *                  *---------------------------------------------*
           if        w-exe-cod-ubi        =    spaces
                     go to exe-cph-wsv-100.
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-cod-ubi        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-wsv-100.
      *                  *---------------------------------------------*
      *                  * Messaggio in uscita                         *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>IMPOSSIBILE SCRIVERE IL RECORD !</h1>"      .
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wsv-900.
       exe-cph-wsv-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [miu]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Ottenimento record [miu]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      01                   to   rf-miu-cod-dpz         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad inserimento       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wsv-600.
       exe-cph-wsv-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-cod-ute        to   rf-miu-ide-ute         .
           move      w-exe-cod-fas        to   rf-miu-ide-fas         .
______*    move      w-exe-ann-rlv        to   rf-miu-ann-rlv         .
           if        w-exe-flg-spv        =    "S"
                     move  "#"            to   rf-miu-flg-spv
           else      move  spaces         to   rf-miu-flg-spv         .
       exe-cph-wsv-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [miu]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-miu-000      thru upd-rec-miu-999        .
       exe-cph-wsv-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wsv-900.
       exe-cph-wsv-600.
      *              *-------------------------------------------------*
      *              * Se record [miu] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wsv-900.
       exe-cph-wsv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wsv-999.
       exe-cph-wsv-999.
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
      *    * Routine di insert record [miu]                            *
      *    *-----------------------------------------------------------*
       ins-rec-miu-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-miu-000      thru cmp-rec-miu-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       ins-rec-miu-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [miu]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-miu-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      01                   to   rf-miu-cod-dpz         .
           move      01                   to   rf-miu-tip-mag         .
           move      w-exe-pro-cnv        to   rf-miu-num-mag         .
           move      w-exe-cod-ubi        to   rf-miu-cod-ubi         .
           move      zero                 to   rf-miu-qta-prs         .
           move      spaces               to   rf-miu-ubi-rlv         .
      *
           if        w-exe-flg-spn        =    "S"
                     move  "#"            to   rf-miu-flg-rlv
           else      move  spaces         to   rf-miu-flg-rlv         .
      *
           move      w-exe-rsm-cnv        to   rf-miu-cod-rsm         .
           move      w-exe-qtr-cnv        to   rf-miu-qta-rlv         .
           move      w-exe-dat-exe        to   rf-miu-ide-dat         .
           move      w-exe-cod-ute        to   rf-miu-ide-ute         .
           move      w-exe-cod-fas        to   rf-miu-ide-fas         .
           move      w-exe-ann-rlv        to   rf-miu-ann-rlv         .
           move      spaces               to   rf-miu-alx-exp         .
       cmp-rec-miu-999.
           exit.

      *    *===========================================================*
      *    * Routine di update record [miu]                            *
      *    *-----------------------------------------------------------*
       upd-rec-miu-000.
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [miu]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmiu"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-miu                 .
       upd-rec-miu-999.
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
      *              * Utente                                          *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_ute"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ute
      *              *-------------------------------------------------*
      *              * Fase                                            *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_fas"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-fas
      *              *-------------------------------------------------*
      *              * Ubicazione                                      *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ubi
      *              *-------------------------------------------------*
      *              * Codice numerico prodotto                        *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "num_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-num-pro
      *              *-------------------------------------------------*
      *              * Quantita' presunta                              *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qta_prs"
                     move  w-all-str-cat (2)
                                          to   w-exe-qta-prs
      *              *-------------------------------------------------*
      *              * Quantita' rilevata                              *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qta_rlv"
                     move  w-all-str-cat (2)
                                          to   w-exe-qta-rlv
      *              *-------------------------------------------------*
      *              * Flag di spunta                                  *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "flg_spn"
                     move  w-all-str-cat (2)
                                          to   w-exe-flg-spn
      *              *-------------------------------------------------*
      *              * Flag di ssupervisione                           *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "flg_spv"
                     move  w-all-str-cat (2)
                                          to   w-exe-flg-spv
      *              *-------------------------------------------------*
      *              * Annotazione                                     *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "ann_ncf"
                     move  w-all-str-cat (2)
                                          to   w-exe-ann-rlv          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zub]                         *
      *    *-----------------------------------------------------------*
       let-arc-zub-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice ubicazione a spaces              *
      *              *-------------------------------------------------*
           if        w-let-arc-zub-cod    =    spaces
                     go to let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TIPUBI    "         to   f-key                  .
           move      w-let-arc-zub-dpz    to   rf-zub-cod-dpz         .
           move      w-let-arc-zub-cod    to   rf-zub-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zub-400.
       let-arc-zub-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zub-des-ubi       to   w-let-arc-zub-des      .
           move      rf-zub-inx-per       to   w-let-arc-zub-inx      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zub-999.
       let-arc-zub-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zub-flg      .
           move      all   "."            to   w-let-arc-zub-des      .
           go to     let-arc-zub-600.
       let-arc-zub-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zub-des      .
       let-arc-zub-600.
           move      zero                 to   w-let-arc-zub-inx      .
       let-arc-zub-999.
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
