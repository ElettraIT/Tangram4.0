       Identification Division.
       Program-Id.                                 eleproos           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    elepro              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/06/03    *
      *                       Ultima revisione:    NdK del 15/10/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Ordine di spedizione HTML con barcode       *
      *                                                                *
      *                    Espansione prodotti in ordine di spedizione *
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
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .

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
           05  w-exe-saa-exe              pic  9(03)                  .
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
           05  w-exe-num-pro              pic  x(07)                  .
           05  w-exe-cnv-pro              pic  9(07)                  .
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
      *        * Per determinazione tabella giacenze presso terzi      *
      *        *-------------------------------------------------------*
           05  w-det-tbl-ros.
      *            *---------------------------------------------------*
      *            * Contatori elementi                                *
      *            *---------------------------------------------------*
               10  w-det-tbl-ros-ctr      pic  9(04)                  .
               10  w-det-tbl-ros-ctx      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-ros-ele  occurs  999.
                   15  w-det-tbl-ros-prt  pic  9(11)                  .
                   15  w-det-tbl-ros-prg  pic  9(05)                  .
                   15  w-det-tbl-ros-qta  pic s9(10)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Work per Det status riga ordine cliente               *
      *        *-------------------------------------------------------*
           05  w-det-sts-ror.
      *            *---------------------------------------------------*
      *            * Status riga ordine cliente                        *
      *            *                                                   *
      *            * - 01 : Chiusa                                     *
      *            * - 02 : Evasa parzialmente                         *
      *            * - 03 : Evasa                                      *
      *            * - 04 : Inevasa                                    *
      *            *---------------------------------------------------*
               10  w-det-sts-ror-sts      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Residuo riga ordine cliente                       *
      *            *                                                   *
      *            * - 01 : Chiusa                                     *
      *            * - 02 : Evasa parzialmente                         *
      *            * - 03 : Evasa                                      *
      *            * - 04 : Inevasa                                    *
      *            *---------------------------------------------------*
               10  w-det-sts-ror-qrs      pic s9(08)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Work per Det : quantita' in ordine da clienti         *
      *        *-------------------------------------------------------*
           05  w-det-qta-ods.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                         [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto                           [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data disponibilita'                       [input] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga         [work] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-wtr.
                   15  w-det-qta-ods-wtp  pic  x(01)                  .
                   15  w-det-qta-ods-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' in ordine da clienti           [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-qoc      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' in ordine da dipendenze        [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-qod      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' primo ordine cliente           [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-qpo      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Data consegna primo ordine cliente       [output] *
      *            *---------------------------------------------------*
               10  w-det-qta-ods-dpo      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-tle      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-abi      pic  9(05)                  .
               10  w-let-arc-dcc-cab      pic  9(05)                  .
               10  w-let-arc-dcc-ccc      pic  x(12)                  .
               10  w-let-arc-dcc-ban      pic  x(10)                  .
               10  w-let-arc-dcc-ccp      pic  x(10)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dtl"                   .

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
           move      spaces               to   w-exe-num-pro          .
           move      zero                 to   w-exe-cnv-pro          .
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
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      07                   to   p-car                  .
           move      w-exe-num-pro        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-cnv-pro          .
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
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [osr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qds-ros-opn-000  thru det-qds-ros-opn-999    .
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
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [osr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qds-ros-cls-000  thru det-qds-ros-cls-999    .
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
      *              * Determinazione tabella righe ordini clienti     *
      *              *-------------------------------------------------*
           perform   det-tbl-ros-000      thru det-tbl-ros-999        .
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
      *              * Test se prodotto trovato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-num-pro        not  = spaces
                     go to emi-tes-800.
      *                  *---------------------------------------------*
      *                  * Separatore                                  *
      *                  *---------------------------------------------*
           display   "<hr>"                                           .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "PRODOTTO NON TROVATO !"
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
      *              * Trattamento ordini clienti                      *
      *              *-------------------------------------------------*
           perform   emi-rig-orc-000      thru emi-rig-orc-999        .
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
      *    * Subroutine per righe ordini clienti                       *
      *    *-----------------------------------------------------------*
       emi-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tbl-ros-ctx      .
       emi-rig-orc-200.
           add       1                    to   w-det-tbl-ros-ctx      .
           if        w-det-tbl-ros-ctx    >    w-det-tbl-ros-ctr
                     go to emi-rig-orc-900.
       emi-rig-orc-400.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
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
      *                  * Normalizzazione [osr]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Lettura [osr]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-det-tbl-ros-prt
                    (w-det-tbl-ros-ctx)   to   rf-osr-num-prt         .
           move      w-det-tbl-ros-prg
                    (w-det-tbl-ros-ctx)   to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
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
           move      w-exe-cnv-pro        to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       emi-rig-orc-400.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      rf-osr-cod-tms       to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "R"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "b"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-osr-num-prt
                    (06 : 06)             to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
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
      *                  * Data documento                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-osr-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
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
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      w-det-tbl-ros-qta
                    (w-det-tbl-ros-ctx)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
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
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-osr-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
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
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           perform   emi-rig-orc-rsa-000  thru emi-rig-orc-rsa-999    .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-rig-orc-600.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     emi-rig-orc-200.
       emi-rig-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-rig-orc-999.
       emi-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Emissione corpo documento                                 *
      *    *                                                           *
      *    * Subroutine per giacenze presso terzi                      *
      *    *                                                           *
      *    * Sub-subroutine per ragione sociale archivio               *
      *    *-----------------------------------------------------------*
       emi-rig-orc-rsa-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo archivio        *
      *              *-------------------------------------------------*
           if        rf-osr-tip-arc      =    "C"
                     go to emi-rig-orc-rsa-310
           else      go to emi-rig-orc-rsa-390.
       emi-rig-orc-rsa-310.
      *              *-------------------------------------------------*
      *              * Se tipo archivio : Cliente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcc]                      *
      *                  *---------------------------------------------*
           move      rf-osr-cod-arc       to   w-let-arc-dcc-cod      .
           move      rf-osr-dpz-arc       to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      w-let-arc-dcc-rag    to   h-alf                  .
      *                  *---------------------------------------------*
      *                  * Ad emissione                                *
      *                  *---------------------------------------------*
           go to     emi-rig-orc-rsa-600.
       emi-rig-orc-rsa-390.
      *              *-------------------------------------------------*
      *              * Se tipo archivio : Non riconosciuto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale              *
      *                  *---------------------------------------------*
           move      all "."              to   h-alf                  .
      *                  *---------------------------------------------*
      *                  * Ad emissione                                *
      *                  *---------------------------------------------*
           go to     emi-rig-orc-rsa-600.
       emi-rig-orc-rsa-600.
      *              *-------------------------------------------------*
      *              * Emissione ragione sociale                       *
      *              *-------------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "td"                 to   h-sub                  .
           move      "1"                  to   h-col                  .
           move      "L"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "b"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       emi-rig-orc-rsa-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-rig-orc-rsa-999.
       emi-rig-orc-rsa-999.
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
      *    * Determinazione status riga ordine cliente                 *
      *    *-----------------------------------------------------------*
       det-sts-ror-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-ror-sts      .
           move      zero                 to   w-det-sts-ror-qrs      .
      *              *-------------------------------------------------*
      *              * Se riga ordine chiusa : oltre                   *
      *              *-------------------------------------------------*
           if        rf-osr-flg-rch       not  = spaces
                     move  01             to   w-det-sts-ror-sts
                     go to det-sts-ror-900.
      *              *-------------------------------------------------*
      *              * Se riga ordine comunque da considerarsi evasa : *
      *              * la riga ordine e' da considerarsi super-evasa,  *
      *              * e quindi si forza un valore negativo nella riga *
      *              * ordine                                          *
      *              *-------------------------------------------------*
______*    if        rf-osr-sdr-ccs       not  = spaces
______*              move  01             to   w-det-sts-ror-sts
______*              go to det-sts-ror-900.
       det-sts-ror-100.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine      *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qds-ros-tip-ope      .
           perform   det-qds-ros-cll-000  thru det-qds-ros-cll-999    .
       det-sts-ror-500.
      *              *-------------------------------------------------*
      *              * Determinazione residuo della riga ordine        *
      *              *-------------------------------------------------*
           move      d-qds-ros-qta-dsp    to   w-det-sts-ror-qrs      .
      *              *-------------------------------------------------*
      *              * Determinazione status della riga ordine         *
      *              *-------------------------------------------------*
      *              * - 01 : Chiusa                                   *
      *              * - 02 : Evasa parzialmente                       *
      *              * - 03 : Evasa                                    *
      *              * - 04 : Inevasa                                  *
      *              *-------------------------------------------------*
           if        d-qds-ros-qta-dsp    =    zero or
                     d-qds-ros-qta-dsp    <    zero
                     move  03             to   w-det-sts-ror-sts
                     go to det-sts-ror-900.
           if        d-qds-ros-qta-dsp    >    zero and
                     d-qds-ros-qta-dsp    =    d-qds-ros-qta-ord
                     move  04             to   w-det-sts-ror-sts
                     go to det-sts-ror-900.
           if        d-qds-ros-qta-dsp    >    zero and
                     d-qds-ros-qta-dsp    <    d-qds-ros-qta-ord
                     move  02             to   w-det-sts-ror-sts
                     go to det-sts-ror-900.
       det-sts-ror-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sts-ror-999.
       det-sts-ror-999.
           exit.

      *    *===========================================================*
      *    * Determinazione tabella ordini clienti per il prodotto     *
      *    *-----------------------------------------------------------*
       det-tbl-ros-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-tot-qta          .
           move      w-exe-dat-exe        to   s-dat                  .
           move      s-saa                to   w-exe-saa-exe          .
           move      w-exe-dat-exe        to   w-det-qta-ods-dat      .
           move      zero                 to   w-det-tbl-ros-ctr      .
       det-tbl-ros-100.
      *              *-------------------------------------------------*
      *              * Start su file [osr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHMAG    "         to   f-key                  .
           move      01                   to   rf-osr-cod-dpz         .
           move      spaces               to   rf-osr-flg-rch         .
           move      01                   to   rf-osr-tip-mag         .
           move      w-exe-cnv-pro        to   rf-osr-num-pro         .
           move      zero                 to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: a close                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-ros-900.
       det-tbl-ros-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [osr]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-ros-900.
       det-tbl-ros-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [osr]                      *
      *              *-------------------------------------------------*
           if        rf-osr-cod-dpz       not  = 01            or
                     rf-osr-flg-rch       not  = spaces        or
                     rf-osr-tip-mag       not  = 01            or
                     rf-osr-num-pro       not  = w-exe-cnv-pro
                     go to det-tbl-ros-900.
       det-tbl-ros-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo ordine                         *
      *                  *                                             *
      *                  * Non si considerano gli ordini pro-forma     *
      *                  *---------------------------------------------*
______*    if        rf-osr-tip-ord       =    "P" or
______*              rf-osr-tip-ord       =    "C"
______*              go to det-tbl-ros-200.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-osr-dat-doc       >    w-det-qta-ods-dat
                     go to det-tbl-ros-200.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo riga                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo riga in comodo ridefinito          *
      *                      *-----------------------------------------*
           move      rf-osr-tip-rig       to   w-det-qta-ods-wtr      .
      *                      *-----------------------------------------*
      *                      * Se prodotto similare  : riciclo         *
      *                      *-----------------------------------------*
           if        w-det-qta-ods-wtf    not  = spaces
                     go to det-tbl-ros-200.
      *                  *---------------------------------------------*
      *                  * Determinazione quantita' evasa riga ordine  *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qds-ros-tip-ope      .
           perform   det-qds-ros-cll-000  thru det-qds-ros-cll-999    .
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivi                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in ordine da clienti o da di- *
      *                      * dipendenze                              *
      *                      *-----------------------------------------*
           if        rf-osr-tip-arc       =    "D"
                     add  d-qds-ros-qta-dsp
                                          to   w-det-qta-ods-qod
           else      add  d-qds-ros-qta-dsp
                                          to   w-det-qta-ods-qoc      .
      *                      *-----------------------------------------*
      *                      * Se quantita' da evadere a zero : a ri-  *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        d-qds-ros-qta-dsp    =    zero
                     go to det-tbl-ros-200.
       det-tbl-ros-500.
      *              *-------------------------------------------------*
      *              * Incremento tabella                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento tabella                          *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ros-ctr      .
      *
           move      rf-osr-num-prt       to   w-det-tbl-ros-prt
                                              (w-det-tbl-ros-ctr)     .
           move      rf-osr-num-prg       to   w-det-tbl-ros-prg
                                              (w-det-tbl-ros-ctr)     .
           move      d-qds-ros-qta-dsp    to   w-det-tbl-ros-qta
                                              (w-det-tbl-ros-ctr)     .
       det-tbl-ros-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-tbl-ros-200.
       det-tbl-ros-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tbl-ros-999.
       det-tbl-ros-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio dipendenza cliente in [dcc]      *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cod    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces, solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    spaces and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a "*   ", solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    "*   " and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-dcc-cod    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-abi       to   w-let-arc-dcc-abi      .
           move      rf-dcc-cod-cab       to   w-let-arc-dcc-cab      .
           move      rf-dcc-ccc-app       to   w-let-arc-dcc-ccc      .
           if        w-let-arc-dcc-tle    =    "D"
                     move  spaces         to   w-let-arc-dcc-ban
                     move  spaces         to   w-let-arc-dcc-ccp
           else      move  rf-dcc-nos-ban to   w-let-arc-dcc-ban
                     move  rf-dcc-nos-ccp to   w-let-arc-dcc-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Normalizzazione per codice dipendenza "*   "    *
      *              *-------------------------------------------------*
           move      "Sia la sede che tutte le dipendenze     "
                                          to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-abi      .
           move      zero                 to   w-let-arc-dcc-cab      .
           move      spaces               to   w-let-arc-dcc-ccc      .
           move      spaces               to   w-let-arc-dcc-ban      .
           move      spaces               to   w-let-arc-dcc-ccp      .
       let-arc-dcc-999.
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
           if        w-all-str-cat (1)    =    "num_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-num-pro          .
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
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
