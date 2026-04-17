       Identification Division.
       Program-Id.                                 eleabbX0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    eleabb              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 23/02/23    *
      *                       Ultima revisione:    NdK del 03/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni di scrittura [bfo] per carichi   *
      *                    ABB                                         *
      *                                                                *
      *                    Tipi operazione:                            *
      *                                                                *
      *                    - WN: scrittura Non conformita'             *
      *                    - WC: scrittura conferma riga (spunta)      *
      *                    - WB: scrittura Barcode                     *
      *                                                                *
      *                    (utilizzato da eleabb01.php)                *
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
      *        * [bfk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfk"                          .
      *        *-------------------------------------------------------*
      *        * [bfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfs"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .

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
           05  w-exe-des-rsm              pic  x(30)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-prg-bfo              pic  x(05)                  .
           05  w-exe-pri-bfo              pic  x(03)                  .
           05  w-exe-col-bfo              pic  x(05)                  .
           05  w-exe-qta-bfo              pic  x(12)                  .
           05  w-exe-qtv-bfo              pic  x(12)                  .
           05  w-exe-qta-ubi              pic  x(12)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-klb-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  x(07)                  .
           05  w-exe-cod-ubi              pic  x(07)                  .
           05  w-exe-ann-ncf              pic  x(80)                  .
           05  w-exe-flg-spn              pic  x(01)                  .
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
           05  w-exe-qta-cnv              pic  9(10)v9(03)            .
           05  w-exe-qtv-cnv              pic  9(10)v9(03)            .
           05  w-exe-qtu-cnv              pic s9(10)v9(03)            .
           05  w-exe-pro-cnv              pic  9(07)                  .
           05  w-exe-rsm-cnv              pic  9(03)                  .
           05  w-exe-prt-mag              pic  9(11)                  .
           05  w-exe-drg-mag              pic  9(07)                  .
           05  w-exe-flg-stp              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-bft.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-nds      pic  9(11)                  .
               10  w-slc-num-bft-nds-r redefines
                   w-slc-num-bft-nds.
                   15  w-slc-num-bft-nsa  pic  9(03)                  .
                   15  w-slc-num-bft-ndp  pic  9(02)                  .
                   15  w-slc-num-bft-npg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Valori in input                                   *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-prt      pic  9(11)                  .
               10  w-slc-num-bft-prg      pic  9(05)                  .
               10  w-slc-num-bft-col      pic  9(03)                  .

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
      *    * Work per contatori ed indici                              *
      *    *-----------------------------------------------------------*
       01  w-cix.
      *        *-------------------------------------------------------*
      *        * Work per contatori righe                              *
      *        *-------------------------------------------------------*
           05  w-cix-ctr-rig.
      *            *---------------------------------------------------*
      *            * Contatore righe colli per la riga                 *
      *            *---------------------------------------------------*
               10  w-cix-ctr-rig-cpr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore righe da spuntare                       *
      *            *---------------------------------------------------*
               10  w-cix-ctr-rig-das      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore righe spuntate                          *
      *            *---------------------------------------------------*
               10  w-cix-ctr-rig-spu      pic  9(05)                  .

      *    *===========================================================*
      *    * Area di comunicazione per movimento di magazzino          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/pmag300z.pgl"                   .

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
           move      18                   to   w-cgi-str-num          .
           perform   ope-prm-inp-000      thru ope-prm-inp-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-cod-rsm          .
           move      spaces               to   w-exe-prt-bfo          .
           move      spaces               to   w-exe-prg-bfo          .
           move      spaces               to   w-exe-col-bfo          .
           move      spaces               to   w-exe-pri-bfo          .
           move      spaces               to   w-exe-qta-bfo          .
           move      spaces               to   w-exe-qtv-bfo          .
           move      spaces               to   w-exe-qta-ubi          .
           move      spaces               to   w-exe-cod-ubi          .
           move      spaces               to   w-exe-ann-ncf          .
           move      spaces               to   w-exe-flg-spn          .
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
      *              * Estrazione elementi                             *
      *              *-------------------------------------------------*
           move      "EX"                 to   w-cgi-tip-ope          .
           move      w-cgi-str-num        to   w-cgi-str-num          .
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
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prt      .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      05                   to   p-car                  .
           move      w-exe-prg-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prg      .
      *                  *---------------------------------------------*
      *                  * Progressivo collo                           *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      05                   to   p-car                  .
           move      w-exe-col-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-col      .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qta-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qta-cnv          .
      *                  *---------------------------------------------*
      *                  * Quantita' verificata                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qtv-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qtv-cnv          .
      *                  *---------------------------------------------*
      *                  * Quantita' per ubicazione                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      12                   to   p-car                  .
           move      w-exe-qta-ubi        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-qtu-cnv          .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-cod-ubi        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-cod-ubi          .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      07                   to   p-car                  .
           move      w-exe-num-pro        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-pro-cnv          .
      *                  *---------------------------------------------*
      *                  * Annotazioni                                 *
      *                  *                                             *
      *                  * Ricodifica unicode                          *
      *                  *---------------------------------------------*
           move      "DU"                 to   h-ope                  .
           move      w-exe-ann-ncf        to   h-alf                  .
           move      "nnn"                to   h-tip                  .
           call      "swd/mod/prg/obj/mhtml0"
                                         using h                      .
           move      h-alf                to   w-exe-ann-ncf          .
      *                  *---------------------------------------------*
      *                  * Pulizia eventuali '+'                       *
      *                  *---------------------------------------------*
           inspect   w-exe-ann-ncf   replacing all "+" by   " "       .
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
      *              * [bfk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
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
      *              *-------------------------------------------------*
      *              * Open modulo gestione movimenti di MAG           *
      *              *-------------------------------------------------*
           perform   mdl-agg-mag-opn-000  thru mdl-agg-mag-opn-999    .
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
      *              * [bfk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
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
      *              *-------------------------------------------------*
      *              * Close modulo gestione movimenti di MAG          *
      *              *-------------------------------------------------*
           perform   mdl-agg-mag-cls-000  thru mdl-agg-mag-cls-999    .
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
           if        w-exe-tip-ope        =    "WN"
                     perform exe-cph-wnc-000
                                          thru exe-cph-wnc-999
           else if   w-exe-tip-ope        =    "WC"
                     perform exe-cph-wcr-000
                                          thru exe-cph-wcr-999
           else if   w-exe-tip-ope        =    "WB"
                     perform exe-cph-wbc-000
                                          thru exe-cph-wbc-999
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
      *    * Subroutine di scrittura Non conformita'                   *
      *    *-----------------------------------------------------------*
       exe-cph-wnc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero collo         *
      *              *-------------------------------------------------*
           if        w-slc-num-bft-col    =    zero
                     perform exe-cph-wnc-bfr-000
                                          thru exe-cph-wnc-bfr-999
           else      perform exe-cph-wnc-bfk-000
                                          thru exe-cph-wnc-bfk-999    .
       exe-cph-wnc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wnc-999.
       exe-cph-wnc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura riga relativa a Non conformita'   *
      *    *-----------------------------------------------------------*
       exe-cph-wnc-bfr-000.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [bfr]                       *
      *              *-------------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
       exe-cph-wnc-bfr-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wnc-bfr-600.
       exe-cph-wnc-bfr-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
           move      w-exe-ann-ncf        to   rf-bfs-ann-spn         .
       exe-cph-wnc-bfr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wnc-bfr-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *                                                 *
      *              * ATTUALMENTE INIBITO                             *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wnc-bfr-900.
       exe-cph-wnc-bfr-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wnc-bfr-100.
       exe-cph-wnc-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wnc-bfr-999.
       exe-cph-wnc-bfr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di conferma riga relativa a [bfk]              *
      *    *-----------------------------------------------------------*
       exe-cph-wnc-bfk-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record [bfr]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
      *                  *---------------------------------------------*
      *                  * Record [bfk]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfk-000      thru rea-rec-bfk-999        .
       exe-cph-wnc-bfk-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wnc-bfk-600.
       exe-cph-wnc-bfk-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
           move      w-exe-ann-ncf        to   rf-bfs-ann-spn         .
       exe-cph-wnc-bfk-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wnc-bfk-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wnc-bfk-900.
       exe-cph-wnc-bfk-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wnc-bfk-100.
       exe-cph-wnc-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wnc-bfk-999.
       exe-cph-wnc-bfk-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura Barcode                           *
      *    *-----------------------------------------------------------*
       exe-cph-wbc-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero collo         *
      *              *-------------------------------------------------*
           if        w-slc-num-bft-col    =    zero
                     perform exe-cph-wbc-bfr-000
                                          thru exe-cph-wbc-bfr-999
           else      perform exe-cph-wbc-bfk-000
                                          thru exe-cph-wbc-bfk-999    .
       exe-cph-wbc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wbc-999.
       exe-cph-wbc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura riga relativa a Barcode           *
      *    *-----------------------------------------------------------*
       exe-cph-wbc-bfr-000.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [bfr]                       *
      *              *-------------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
       exe-cph-wbc-bfr-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wbc-bfr-600.
       exe-cph-wbc-bfr-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio annotazioni                    *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Rilevato Barcode :" to   w-all-str-cat (1)      .
           move      w-exe-klb-pro        to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
           move      w-all-str-alf        to   rf-bfs-ann-spn         .
       exe-cph-wbc-bfr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wbc-bfr-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wbc-bfr-900.
       exe-cph-wbc-bfr-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wbc-bfr-100.
       exe-cph-wbc-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wbc-bfr-999.
       exe-cph-wbc-bfr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di conferma riga relativa a [bfk]              *
      *    *-----------------------------------------------------------*
       exe-cph-wbc-bfk-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record [bfr]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
      *                  *---------------------------------------------*
      *                  * Record [bfk]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfk-000      thru rea-rec-bfk-999        .
       exe-cph-wbc-bfk-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wbc-bfk-600.
       exe-cph-wbc-bfk-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Assemblaggio annotazioni                    *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Rilevato Barcode :" to   w-all-str-cat (1)      .
           move      w-exe-klb-pro        to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
           move      w-all-str-alf        to   rf-bfs-ann-spn         .
       exe-cph-wbc-bfk-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wbc-bfk-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wbc-bfk-900.
       exe-cph-wbc-bfk-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wbc-bfk-100.
       exe-cph-wbc-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wbc-bfk-999.
       exe-cph-wbc-bfk-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di conferma riga                               *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero collo         *
      *              *-------------------------------------------------*
           if        w-slc-num-bft-col    =    zero
                     perform exe-cph-wcr-bfr-000
                                          thru exe-cph-wcr-bfr-999
           else      perform exe-cph-wcr-bfk-000
                                          thru exe-cph-wcr-bfk-999    .
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
      *    * Subroutine di conferma riga relativa a [bfr]              *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-bfr-000.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [bfr]                       *
      *              *-------------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
       exe-cph-wcr-bfr-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wcr-bfr-600.
       exe-cph-wcr-bfr-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to exe-cph-wcr-bfr-250.
      *                  *---------------------------------------------*
      *                  * Attribuzione protocollo di magazzino        *
      *                  *---------------------------------------------*
           move      "NP"                 to   l-mag-300-tip-ope      .
           move      w-exe-dat-exe        to   l-mag-300-dat-reg      .
           move      01                   to   l-mag-300-cod-dpz      .
           perform   mdl-agg-mag-cll-000  thru mdl-agg-mag-cll-999    .
           move      l-mag-300-num-prt    to   w-exe-prt-mag          .
           move      w-exe-dat-exe        to   w-exe-drg-mag          .
       exe-cph-wcr-bfr-250.
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
      *                  *---------------------------------------------*
      *                  * Quantita' rilevata                          *
      *                  *---------------------------------------------*
           move      w-exe-qtv-cnv        to   rf-bfs-qta-ril         .
      *                  *---------------------------------------------*
      *                  * Flag di spunta                              *
      *                  *---------------------------------------------*
           move      w-exe-flg-spn        to   rf-bfs-flg-spn         .
      *                  *---------------------------------------------*
      *                  * Codice responsabile                         *
      *                  *---------------------------------------------*
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
      *                  *---------------------------------------------*
      *                  * Numero protocollo di magazzino              *
      *                  *---------------------------------------------*
           move      w-exe-prt-mag        to   rf-bfs-prt-mag         .
      *                  *---------------------------------------------*
      *                  * Data registrazione di magazzino             *
      *                  *---------------------------------------------*
           move      w-exe-drg-mag        to   rf-bfs-drg-mag         .
      *                  *---------------------------------------------*
      *                  * Annotazione per la spunta                   *
      *                  *---------------------------------------------*
           move      w-exe-ann-ncf        to   rf-bfs-ann-spn         .
       exe-cph-wcr-bfr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wcr-bfr-400.
      *              *-------------------------------------------------*
      *              * Eventuale cambio di ubicazione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to exe-cph-wcr-bfr-500.
      *                  *---------------------------------------------*
      *                  * Scrittura movimento di MAG                  *
      *                  *---------------------------------------------*
           perform   exe-cph-wcr-mag-000  thru exe-cph-wcr-mag-999    .
       exe-cph-wcr-bfr-500.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-bfr-900.
       exe-cph-wcr-bfr-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-bfr-100.
       exe-cph-wcr-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-bfr-999.
       exe-cph-wcr-bfr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di scrittura movimento di magazzino per il     *
      *    * cambio di ubicazione                                      *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-mag-000.
      *              *-------------------------------------------------*
      *              * Test prelimiari                                 *
      *              *-------------------------------------------------*
           if        w-exe-prt-mag        =    zero or
                     w-exe-drg-mag        =    zero
                     go to exe-cph-wcr-mag-900.
       exe-cph-wcr-mag-100.
      *              *-------------------------------------------------*
      *              * Operazione di cambio ubicazione                 *
      *              *-------------------------------------------------*
           move      "CD"                 to   l-mag-300-tip-ope      .
      *              *-------------------------------------------------*
      *              * Data registrazione magazzino                    *
      *              *-------------------------------------------------*
           move      w-exe-drg-mag        to   l-mag-300-dat-reg      .
      *              *-------------------------------------------------*
      *              * Protocollo di magazzino                         *
      *              * automatica                                      *
      *              *-------------------------------------------------*
           move      w-exe-prt-mag        to   l-mag-300-num-prt      .
      *              *-------------------------------------------------*
      *              * Causale di magazzino                            *
      *              *-------------------------------------------------*
           move      00057                to   l-mag-300-cod-cau      .
      *              *-------------------------------------------------*
      *              * Data esecuzione come data documento             *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   l-mag-300-dat-doc      .
      *              *-------------------------------------------------*
      *              * Editing protocollo documento                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-bft-prt
                    (06 : 06)             to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   l-mag-300-num-doc      .
      *              *-------------------------------------------------*
      *              * Codice numerico prodotto                        *
      *              *-------------------------------------------------*
           move      rf-bfr-num-mag       to   l-mag-300-num-mag      .
      *              *-------------------------------------------------*
      *              * Codice alfanumerico prodotto                    *
      *              *-------------------------------------------------*
           move      rf-bfr-alf-mag       to   l-mag-300-alf-mag      .
      *              *-------------------------------------------------*
      *              * Quantita' movimentata                           *
      *              *-------------------------------------------------*
           move      w-exe-qtv-cnv        to   l-mag-300-qta-mov      .
      *              *-------------------------------------------------*
      *              * Ubicazione di origine (sempre 'LRIC')           *
      *              *                                                 *
      *              * ___ LEGGERE DA 'zbf' (elebfoB1) ___             *
      *              *-------------------------------------------------*
           move      "LRIC   "            to   l-mag-300-cod-dsl      .
      *              *-------------------------------------------------*
      *              * Ubicazione di destinazione ('LTRA')             *
      *              *-------------------------------------------------*
           move      "LTRA   "            to   l-mag-300-cod-dsd      .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           perform   mdl-agg-mag-cll-000  thru mdl-agg-mag-cll-999    .
       exe-cph-wcr-mag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-mag-999.
       exe-cph-wcr-mag-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di conferma riga relativa a [bfk]              *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-bfk-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record [bfr]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfr-000      thru rea-rec-bfr-999        .
      *                  *---------------------------------------------*
      *                  * Record [bfk]                                *
      *                  *---------------------------------------------*
           perform   rea-rec-bfk-000      thru rea-rec-bfk-999        .
       exe-cph-wcr-bfk-100.
      *              *-------------------------------------------------*
      *              * Ottenimento record [bfs]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: a scrittura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-wcr-bfk-600.
       exe-cph-wcr-bfk-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to exe-cph-wcr-bfk-250.
      *                  *---------------------------------------------*
      *                  * Attribuzione protocollo di magazzino        *
      *                  *---------------------------------------------*
           move      "NP"                 to   l-mag-300-tip-ope      .
           move      w-exe-dat-exe        to   l-mag-300-dat-reg      .
           move      01                   to   l-mag-300-cod-dpz      .
           perform   mdl-agg-mag-cll-000  thru mdl-agg-mag-cll-999    .
           move      l-mag-300-num-prt    to   w-exe-prt-mag          .
           move      w-exe-dat-exe        to   w-exe-drg-mag          .
       exe-cph-wcr-bfk-250.
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
      *                  *---------------------------------------------*
      *                  * Quantita' rilevata                          *
      *                  *---------------------------------------------*
           move      w-exe-qtv-cnv        to   rf-bfs-qta-ril         .
      *                  *---------------------------------------------*
      *                  * Flag di spunta                              *
      *                  *---------------------------------------------*
           move      w-exe-flg-spn        to   rf-bfs-flg-spn         .
      *                  *---------------------------------------------*
      *                  * Codice responsabile                         *
      *                  *---------------------------------------------*
           move      w-exe-rsm-cnv        to   rf-bfs-cod-rsm         .
      *                  *---------------------------------------------*
      *                  * Numero protocollo di magazzino              *
      *                  *---------------------------------------------*
           move      w-exe-prt-mag        to   rf-bfs-prt-mag         .
      *                  *---------------------------------------------*
      *                  * Data registrazione di magazzino             *
      *                  *---------------------------------------------*
           move      w-exe-drg-mag        to   rf-bfs-drg-mag         .
      *                  *---------------------------------------------*
      *                  * Annotazione per la spunta                   *
      *                  *---------------------------------------------*
           move      w-exe-ann-ncf        to   rf-bfs-ann-spn         .
       exe-cph-wcr-bfk-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
      *              *-------------------------------------------------*
           perform   upd-rec-bfs-000      thru upd-rec-bfs-999        .
       exe-cph-wcr-bfk-400.
      *              *-------------------------------------------------*
      *              * Eventuale cambio di ubicazione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to exe-cph-wcr-bfk-500.
      *                  *---------------------------------------------*
      *                  * Scrittura movimento di MAG                  *
      *                  *---------------------------------------------*
           perform   exe-cph-wcr-mag-000  thru exe-cph-wcr-mag-999    .
       exe-cph-wcr-bfk-500.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-bfk-900.
       exe-cph-wcr-bfk-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record [bfs] normalizzato         *
      *                  *---------------------------------------------*
           perform   nor-rec-bfs-000      thru nor-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Ritorno a scrittura                         *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-bfk-100.
       exe-cph-wcr-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wcr-bfk-999.
       exe-cph-wcr-bfk-999.
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
      *              *-------------------------------------------------*
      *              * Formattazione output                            *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Editing progressivo riga                        *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-pri-bfo        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Editing progressivo collo                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-bft-col    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (4)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "<h1> AGGIORNAMENTO RIGA"
                                          to   w-all-str-cat (1)      .
      *
           if        w-slc-num-bft-col    =    zero
                     move  spaces         to   w-all-str-cat (3)
                     move  spaces         to   w-all-str-cat (4)
           else      move  "-"            to   w-all-str-cat (3)      .
      *
           move      "</h1>"              to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-prm-msg          .
      *              *-------------------------------------------------*
      *              * Messaggio in uscita                             *
      *              *-------------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   w-exe-prm-msg                                    .
           display   "</div>"                                         .
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
      *    *                                                           *
      *    * Campi previsti                                            *
      *    * --------------                                            *
      *    * 'tip_ope' = Tipo operazione                               *
      *    * 'rsp_doc' = Responsabile di magazzino                     *
      *    * 'rsp_des' = Responsabile di magazzino - descrizione       *
      *    * 'prt_bfo' = Protocollo                                    *
      *    * 'prg_bfo' = Progressivo riga bfo                          *
      *    * 'pri_bfo' = Progressivo riga interno di comodo            *
      *    * 'col_bfo' = Progressivo collo                             *
      *    * 'qta_bfo' = Quantita' in riga                             *
      *    * 'qtv_bfo' = Quantita' in riga verificata                  *
      *    * 'qta_ubi' = Quantita' ubicazione                          *
      *    * 'ann_spn' = Annotazioni spunta                            *
      *    * 'alf_pro' = Codice prodotto alfanumerico                  *
      *    * 'num_pro' = Codice prodotto numerico                      *
      *    * 'ann_ncf' = Note di Non conformita'                       *
      *    * 'flg_spn' = Flag di spunta                                *
      *    * 'cod_ubi' = Codice ubicazione                             *
      *    * 'num_doc' = Numero documento                              *
      *    * 'klb_pro' = Codice a barre prodotto                       *
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
      *              * Responsabile - descrizione                      *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "rsp_des"
                     move  w-all-str-cat (2)
                                          to   w-exe-des-rsm
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prt_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-bfo
      *              *-------------------------------------------------*
      *              * Progressivo riga bfo                            *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prg_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prg-bfo
      *              *-------------------------------------------------*
      *              * Progressivo riga interno di comodo              *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "pri_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-pri-bfo
      *              *-------------------------------------------------*
      *              * Progressivo collo                               *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "col_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-col-bfo
      *              *-------------------------------------------------*
      *              * Quantita' in riga                               *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qta_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-qta-bfo
      *              *-------------------------------------------------*
      *              * Quantita' in riga verificata                    *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qtv_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-qtv-bfo
      *              *-------------------------------------------------*
      *              * Quantita' ubicazione                            *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "qta_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-qta-ubi
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "ann_spn"
                     move  w-all-str-cat (2)
                                          to   w-exe-ann-ncf
      *              *-------------------------------------------------*
      *              * Codice prodotto alfanumerico                    *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "alf_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-alf-pro
      *              *-------------------------------------------------*
      *              * Codice prodotto numerico                        *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "num_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-num-pro
      *              *-------------------------------------------------*
      *              * Note                                            *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "ann_ncf"
                     move  w-all-str-cat (2)
                                          to   w-exe-ann-ncf
      *              *-------------------------------------------------*
      *              * Flag di spunta                                  *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "flg_spn"
                     move  w-all-str-cat (2)
                                          to   w-exe-flg-spn
      *              *-------------------------------------------------*
      *              * Codice ubicazione                               *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "cod_ubi"
                     move  w-all-str-cat (2)
                                          to   w-exe-cod-ubi
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "num_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-bfo
      *              *-------------------------------------------------*
      *              * Barcode                                         *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "klb_pro"
                     move  w-all-str-cat (2)
                                          to   w-exe-klb-pro          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
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
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
       det-num-pro-050.
      *              *-------------------------------------------------*
      *              * Test su valore in input                         *
      *              *-------------------------------------------------*
           if        w-det-num-pro-alf    =    spaces
                     go to det-num-pro-950.
       det-num-pro-050.
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
       det-num-pro-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [dcp] per codice alfanumerico *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-900.
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
                     go to det-num-pro-900.
       det-num-pro-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [dcp]                      *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-900.
       det-num-pro-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
       det-num-pro-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-num-pro-200.
       det-num-pro-900.
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
       det-num-pro-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-999.
       det-num-pro-999.
           exit.

      *    *===========================================================*
      *    * Lettura record [bfr]                                      *
      *    *-----------------------------------------------------------*
       rea-rec-bfr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [bfr]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Lettura record [bfr]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfr-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       rea-rec-bfr-999.
           exit.

      *    *===========================================================*
      *    * Lettura record [bfk]                                      *
      *    *-----------------------------------------------------------*
       rea-rec-bfk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [bfk]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *              *-------------------------------------------------*
      *              * Lettura record [bfk]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bfk-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfk-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfk-num-prc         .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
       rea-rec-bfk-999.
           exit.

      *    *===========================================================*
      *    * Routine di scrittura record [bfs] normalizzato            *
      *    *-----------------------------------------------------------*
       nor-rec-bfs-000.
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
      *              * Campi chiave                                    *
      *              *-------------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       nor-rec-bfs-999.
           exit.

      *    *===========================================================*
      *    * Routine di update record [bfs]                            *
      *    *-----------------------------------------------------------*
       upd-rec-bfs-000.
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [bfs]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       upd-rec-bfs-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di lettura eventuale riga di spunta            *
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
      *    * Subroutines per modulo di aggiornamento del magazzino     *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/pmag300z.pgs"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
