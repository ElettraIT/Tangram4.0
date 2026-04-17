       Identification Division.
       Program-Id.                                 eleprogu           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    elepro              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 14/11/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Ordine di spedizione HTML con barcode       *
      *                                                                *
      *                    Espansione prodotti in ubicazione           *
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
      *        * Flag di spunta                                        *
      *        *-------------------------------------------------------*
           05  w-exe-flg-spn              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-ubi              pic  x(07)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione dati                       *
      *        *-------------------------------------------------------*
           05  w-exe-tot-qta              pic s9(11)                  .
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
      *        * Work per Det su giacenze per ubicazione               *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-ctr      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-qta      pic s9(10)v9(03)            .

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
           move      spaces               to   w-exe-cod-ubi          .
           move      spaces               to   w-exe-alf-pro          .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri                       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-cgi-tip-ope          .
           move      01                   to   w-cgi-str-num          .
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
      *                  * Codice ubicazione                           *
      *                  *---------------------------------------------*
           move      w-exe-cod-ubi        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-cod-ubi          .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-cgi-alf-pro          .
           perform   ext-prm-pro-000      thru ext-prm-pro-999        .
           move      w-cgi-alf-pro        to   w-exe-alf-pro          .
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
      *              * Determinazione buffer prodotti in ubicazione    *
      *              *-------------------------------------------------*
           move      "GU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      w-exe-cod-ubi        to   d-sld-ubi-cod-ubi      .
           move      01                   to   d-sld-ubi-tip-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
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
       emi-tes-600.
      *              *-------------------------------------------------*
      *              * Test se ubicazione trovata                      *
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
           move      "UBICAZIONE NON TROVATA !"
                                          to   w-exe-err-msg          .
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
       emi-tes-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-tes-999.
       emi-tes-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *-----------------------------------------------------------*
       emi-cor-000.
      *              *-------------------------------------------------*
      *              * Apertura tabella                                *
      *              *-------------------------------------------------*
           display   "<table class='bordotab' align='center' width=90% >
      -              ""                                               .
      *              *-------------------------------------------------*
      *              * Trattamento Ubicazione                          *
      *              *-------------------------------------------------*
           perform   emi-cor-ubi-000      thru emi-cor-ubi-999        .
       emi-cor-900.
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "table"              to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-cor-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per ubicazione                                 *
      *    *-----------------------------------------------------------*
       emi-cor-ubi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione totale                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-tot-qta          .
       emi-cor-ubi-100.


           go to     emi-cor-ubi-150.
      *              *-------------------------------------------------*
      *              * DEBUG                                           *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   o-pst                                            .
           display   "<br>"                                           .
           display   w-exe-alf-pro                                    .
           display   "<br>"                                           .

       emi-cor-ubi-150.
      *              *-------------------------------------------------*
      *              * Ciclo di stampa in base al numero di elementi   *
      *              * determinati                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio ciclo                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-gia-ubi-ctr      .
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
      *
           if        rf-dcp-alf-pro       =    w-exe-alf-pro
                     move  "r"            to   h-stl
           else      move  "B"            to   h-stl                  .
      *
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
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "cod_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ubi
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
