       Identification Division.
       Program-Id.                                 elebfop2           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/11/24    *
      *                       Ultima revisione:    NdK del 30/07/26    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Gestione spedizioni                         *
      *                                                                *
      *                    Ricerca documenti amcora aperti             *
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
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfs"                          .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione castelletto prodotti letti         *
      *        *-------------------------------------------------------*
           05  w-det-cst-pro.
      *            *---------------------------------------------------*
      *            * Numero elementi determinati                       *
      *            *---------------------------------------------------*
               10  w-det-cst-pro-ctr      pic  9(05)                  .
               10  w-det-cst-pro-max      pic  9(03) value 999        .
      *            *---------------------------------------------------*
      *            * Comodi                                            *
      *            *---------------------------------------------------*
               10  w-det-cst-pro-ct1      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Castelletto righe prodotto nei documenti          *
      *            *---------------------------------------------------*
               10  w-det-cst-pro-ele  occurs  999.
                   15  w-det-cst-pro-prt  pic  9(11)                  .
                   15  w-det-cst-pro-qta  pic  9(10)v9(03)            .
                   15  w-det-cst-pro-spn  pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work-area per numero documento                            *
      *    *-----------------------------------------------------------*
       01  w-num-doc.
      *        *-------------------------------------------------------*
      *        * Work per ridefinizione                                *
      *        *-------------------------------------------------------*
           05  w-num-doc-num              pic  9(11)                  .
           05  w-num-doc-num-r redefines
               w-num-doc-num.
               10  w-num-doc-saa          pic  9(03)                  .
               10  w-num-doc-dpz          pic  9(02)                  .
               10  w-num-doc-prg          pic  9(06)                  .
           05  w-num-doc-pps              pic  9(09)                  .
           05  w-num-doc-pps-r redefines
               w-num-doc-pps.
               10  w-num-doc-pss          pic  9(03)                  .
               10  w-num-doc-psp          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione stringa                        *
      *        *-------------------------------------------------------*
           05  w-num-doc-str              pic  x(07)                  .
           05  w-num-doc-pnt              pic  9(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [bfs]                        *
      *        *-------------------------------------------------------*
           05  w-let-rec-bfs.
               10  w-let-rec-bfs-flg      pic  x(01)                  .
               10  w-let-rec-bfs-prt      pic  9(11)                  .
               10  w-let-rec-bfs-prg      pic  9(05)                  .
               10  w-let-rec-bfs-prr      pic  9(05)                  .
               10  w-let-rec-bfs-qta      pic s9(10)v9(03)            .
               10  w-let-rec-bfs-spn      pic  x(01)                  .
               10  w-let-rec-bfs-odm      pic  x(03)                  .
               10  w-let-rec-bfs-ncf      pic  x(80)                  .

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
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .

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
           move      spaces               to   w-exe-alf-pro          .
           move      zero                 to   w-exe-num-pro          .
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
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
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
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [bfs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Test se documenti aperti                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione testate [bft] degli ultimi 6 mesi *
      *                  *---------------------------------------------*
           perform   exe-cph-bft-000      thru exe-cph-bft-999        .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-det-cst-pro-ctr    not  = zero
                     go to exe-cph-600.
      *                  *---------------------------------------------*
      *                  * Flag di documenti non trovati               *
      *                  *---------------------------------------------*
           display   "ND"                                             .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
       exe-cph-600.
      *              *-------------------------------------------------*
      *              * Lettura con successo                            *
      *              *-------------------------------------------------*
           perform   exe-cph-dsp-000      thru exe-cph-dsp-999        .
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
      *    * Subroutine per scansione distinta                         *
      *    *-----------------------------------------------------------*
       exe-cph-bft-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cst-pro-ctr      .
      *              *-------------------------------------------------*
      *              * Determinazione data 6 mesi precedenti           *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   w-det-nrg-dat-dtb      .
           move      180                  to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
       exe-cph-bft-100.
      *              *-------------------------------------------------*
      *              * Start su [bft]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-det-nrg-dat-dtd    to   rf-bft-dat-reg         .
           move      01                   to   rf-bft-cod-dpz         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bft-900.
       exe-cph-bft-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [bft]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bft-900.
       exe-cph-bft-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
       exe-cph-bft-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su codice fornitore ABB (3239)    *
      *                  *---------------------------------------------*
           if        rf-bft-cod-arc       =    0003239
                     go to exe-cph-bft-200.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo documento                 *
      *                  *---------------------------------------------*
           if        rf-bft-cod-tmb       =    "BRMD "
                     go to exe-cph-bft-200.
       exe-cph-bft-500.
      *              *-------------------------------------------------*
      *              * Scansione righe                                 *
      *              *-------------------------------------------------*
           perform   exe-cph-bfr-000      thru exe-cph-bfr-999        .
       exe-cph-bft-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-bft-200.
       exe-cph-bft-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-bft-999.
       exe-cph-bft-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per scansione distinta                         *
      *    *-----------------------------------------------------------*
       exe-cph-bfr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       exe-cph-bfr-100.
      *              *-------------------------------------------------*
      *              * Start su [bfr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bfr-900.
       exe-cph-bfr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [bfr]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bfr-900.
       exe-cph-bfr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bfr-num-prt       not  = rf-bft-num-prt
                     go to exe-cph-bfr-900.
       exe-cph-bfr-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione quantita' spuntata           *
      *                  *---------------------------------------------*
           move      rf-bfr-num-prt       to   w-let-rec-bfs-prt      .
           move      rf-bfr-num-prg       to   w-let-rec-bfs-prg      .
           move      zero                 to   w-let-rec-bfs-prr      .
           perform   let-rec-bfs-000      thru let-rec-bfs-999        .
           if        w-let-rec-bfs-spn    not  = spaces
                     go to exe-cph-bfr-200.
           if        w-let-rec-bfs-qta    =    rf-bfr-qta-acq
                     go to exe-cph-bfr-200.
       exe-cph-bfr-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore castelletto righe          *
      *              *-------------------------------------------------*
           add       1                    to   w-det-cst-pro-ctr      .
           if        w-det-cst-pro-ctr    >    w-det-cst-pro-max
                     go to exe-cph-bfr-900.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento in castelletto righe   *
      *              *-------------------------------------------------*
           move      rf-bfr-num-prt       to   w-det-cst-pro-prt
                                              (w-det-cst-pro-ctr)     .
           move      rf-bfr-qta-acq       to   w-det-cst-pro-qta
                                              (w-det-cst-pro-ctr)     .
           move      w-let-rec-bfs-qta    to   w-det-cst-pro-spn
                                              (w-det-cst-pro-ctr)     .
       exe-cph-bfr-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-bfr-200.
       exe-cph-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-bfr-999.
       exe-cph-bfr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per visualizzazione linee bufferizzate         *
      *    *-----------------------------------------------------------*
       exe-cph-dsp-000.
      *              *-------------------------------------------------*
      *              * Componente Javascript per il Dialog             *
      *              *-------------------------------------------------*
           display   "<script>"                                       .
      *
           display   "$('.dlg_row').click(function(){"                .
           display   "var row_dlg = $(this).closest('tr').find('#prt_idx
      -              "').val();"                                      .
           display   "$('#prt_bfo').val('row_dlg');"                  .
           display   "$.redirect('./elebfoB1',"                       .
           display   "{'num_doc':row_dlg.trim(),"                     .
           display   "'rsp_doc':"                                     .
           display   w-exe-cod-rsm                                    .
           display   ","                                              .
           display   "'liv_ope':1},"                                  .
           display   "'POST',"                                        .
           display   "'_self');"                                      .
           display   "});"                                            .
      *
           display   "</script>"                                      .
      *              *-------------------------------------------------*
      *              * Apertura tabella                                *
      *              *-------------------------------------------------*
           display   "<table class='bordotab' align='center' width=80%> 
      -              ""                                               .
       exe-cph-dsp-100.
      *              *-------------------------------------------------*
      *              * Intestazione colonne tabella                    *
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
      *                  * Literal per Protocollo                      *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Protocollo"         to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita'                       *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Qta"                to   h-alf                  .
           move      "1"                  to   h-col                  .
           move      "C"                  to   h-all                  .
           move      "S"                  to   h-wdt                  .
           move      "B"                  to   h-stl                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita' spuntata              *
      *                  *---------------------------------------------*
           move      "TD"                 to   h-ope                  .
           move      "th"                 to   h-sub                  .
           move      "Controllata"        to   h-alf                  .
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
       exe-cph-dsp-150.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cst-pro-ct1      .
       exe-cph-dsp-200.
      *              *-------------------------------------------------*
      *              * Ciclo di emissione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-cst-pro-ct1      .
           if        w-det-cst-pro-ct1    >    w-det-cst-pro-ctr
                     go to exe-cph-dsp-900.
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "O"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
                                         
                                         
      *                  *---------------------------------------------*
      *                  * Numero protocollo in work ridefinito        *
      *                  *---------------------------------------------*
           move      w-det-cst-pro-prt
                    (w-det-cst-pro-ct1)   to   w-num-doc-num          .
      *                  *---------------------------------------------*
      *                  * Completamento ridefinizione                 *
      *                  *---------------------------------------------*
           move      w-num-doc-saa        to   w-num-doc-pss          .
           move      w-num-doc-prg        to   w-num-doc-psp          .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      w-num-doc-pps        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<td class='dlg_row'>"                           .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "<h2>"
           else      display "<h3>"                                   .
      *
           display   p-edt                                            .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "</h2>"
           else      display "</h3>"                                  .
      *
           display   "</td>"                                          .
       exe-cph-dsp-300.
      *                  *---------------------------------------------*
      *                  * Campo 'hidden' per consentire selezione     *
      *                  *---------------------------------------------*
           display   "<input type='hidden' name='prt_idx' id='prt_idx' v
      -              "alue='"                                         .
           display   w-num-doc-prg                                    .
           display   "'>"                                             .
       exe-cph-dsp-400.
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      w-det-cst-pro-qta
                    (w-det-cst-pro-ct1)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<td class='dlg_row'>"                           .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "<h2>"
           else      display "<h3>"                                   .
      *
           display   p-edt                                            .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "</h2>"
           else      display "</h3>"                                  .
      *
           display   "</td>"                                          .
       exe-cph-dsp-500.
      *                  *---------------------------------------------*
      *                  * Editing quantita' spuntata                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<td class='dlg_row'>"                           .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "<h2>"
           else      display "<h3>"                                   .
      *
           display   p-edt                                            .
      *
           if        w-det-cst-pro-spn
                    (w-det-cst-pro-ct1)   =    zero
                     display "</h2>"
           else      display "</h3>"                                  .
      *
           display   "</td>"                                          .
       exe-cph-dsp-600.
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "ET"                 to   h-ope                  .
           move      "tr"                 to   h-tag                  .
           move      "C"                  to   h-sub                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
       exe-cph-dsp-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-dsp-200.
       exe-cph-dsp-900.
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
           go to     exe-cph-dsp-999.
       exe-cph-dsp-999.
           exit.

      *    *===========================================================*
      *    * Lettura eventuale riga di spunta                          *
      *    *-----------------------------------------------------------*
       let-rec-bfs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-rec-bfs-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-rec-bfs-qta      .
           move      spaces               to   w-let-rec-bfs-spn      .
           move      spaces               to   w-let-rec-bfs-odm      .
           move      spaces               to   w-let-rec-bfs-ncf      .
       let-rec-bfs-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *              *-------------------------------------------------*
      *              * Lettura record [bfs]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-rec-bfs-prt    to   rf-bfs-num-prt         .
           move      w-let-rec-bfs-prg    to   rf-bfs-num-prg         .
           move      w-let-rec-bfs-prr    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: flag di uscita       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-let-rec-bfs-flg   
                     go to let-rec-bfs-900.
       let-rec-bfs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-bfs-qta-ril       to   w-let-rec-bfs-qta      .
           move      rf-bfs-flg-spn       to   w-let-rec-bfs-spn      .
           move      rf-bfs-ann-spn       to   w-let-rec-bfs-ncf      .
       let-rec-bfs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-rec-bfs-999.
       let-rec-bfs-999.
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
      *    * Subroutines di trattamento variabile POST                 *
      *    *                                                           *
      *    * ELETTRA                                                   *
      *    *-----------------------------------------------------------*
           copy      "ele/cgi/prg/cpy/elecgi00.cps"                   .

      *    *===========================================================*
      *    * Subroutines per operazioni sulle date                     *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
