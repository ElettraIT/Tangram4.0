       Identification Division.
       Program-Id.                                 eleabbX1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    www                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    ele                 *
      *                                   Fase:    eleabb              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 23/02/23    *
      *                       Ultima revisione:    NdK del 27/03/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni varie su [bfo] per carichi ABB   *
      *                                                                *
      *                    Tipi operazione:                            *
      *                                                                *
      *                    - SS: stato delle spunte di un documento    *
      *                    - PT: test se prodotto presente in LTRA     *
      *                    - LP: determinazione codice alfanumerico    *
      *                    - GU: giacenza ubicazioni prodotto          *
      *                                                                *
      *                    (utilizzato da eleabb01.php e eleabb02.php) *
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
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .

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
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
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
           05  w-exe-qta-ubi              pic s9(10)v9(03)            .
           05  w-exe-ubi-ppr              pic  x(07)                  .
           05  w-exe-ubi-ppp              pic  x(07)                  .
           05  w-exe-inx-ppr              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per emissione JSON                             *
      *        *-------------------------------------------------------*
           05  w-exe-jsn-lab              pic  x(07)                  .
           05  w-exe-jsn-val              pic  x(80)                  .
           05  w-exe-jsn-ctr              pic  x(01)                  .
           05  w-exe-jsn-alf              pic  x(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(24)                  .
           05  w-exe-prm-vx2              pic  x(24)                  .

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
               10  w-slc-num-bft-col      pic  9(05)                  .

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
      *            *---------------------------------------------------*
      *            * Descrizione prodotto                              *
      *            *---------------------------------------------------*
               10  w-det-num-pro-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione file JSON                          *
      *        *-------------------------------------------------------*
           05  w-det-fil-jsn.
      *            *---------------------------------------------------*
      *            * Campi di comodo                                   *
      *            *---------------------------------------------------*
               10  w-det-fil-jsn-rig      pic  9(03)                  .
               10  w-det-fil-jsn-vrg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det su giacenze per ubicazione               *
      *        *-------------------------------------------------------*
           05  w-det-gia-ubi.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Quantita' di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-gia-ubi-qta      pic s9(10)v9(03)            .

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
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    * per ubicazione                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dtl"                   .

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
           move      spaces               to   w-exe-prt-bfo          .
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
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prt      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico prodotto                *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-cgi-alf-pro          .
           perform   ext-prm-pro-000      thru ext-prm-pro-999        .
           move      w-cgi-alf-pro        to   w-exe-alf-pro          .
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
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-exe-tip-ope        =    "SS"
                     perform exe-cph-sts-000
                                          thru exe-cph-sts-999
           else if   w-exe-tip-ope        =    "PT"
                     perform exe-cph-spt-000
                                          thru exe-cph-spt-999
           else if   w-exe-tip-ope        =    "LP"
                     perform exe-cph-lpd-000
                                          thru exe-cph-lpd-999
           else if   w-exe-tip-ope        =    "GU"
                     perform exe-cph-gup-000
                                          thru exe-cph-gup-999
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
      *    * Subroutine di determinazione stato delle spunte           *
      *    *-----------------------------------------------------------*
       exe-cph-sts-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      "N"                  to   w-exe-flg-stp          .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   ctl-prl-spn-bfr-000  thru ctl-prl-spn-bfr-999    .
      *              *-------------------------------------------------*
      *              * Se i contatori sono a zero: oltre               *
      *              *-------------------------------------------------*
           if        w-cix-ctr-rig-das    =    zero and
                     w-cix-ctr-rig-spu    =    zero
                     go to exe-cph-sts-800.
      *              *-------------------------------------------------*
      *              * Confronto tra i contatori                       *
      *              *-------------------------------------------------*
           if        w-cix-ctr-rig-das    =    w-cix-ctr-rig-spu
                     move  "S"            to   w-exe-flg-stp          .
       exe-cph-sts-800.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           display   w-exe-flg-stp                                    .
       exe-cph-sts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-sts-999.
       exe-cph-sts-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di determinazione se prodotto in ubicazione di *
      *    * transito                                                  *
      *    *-----------------------------------------------------------*
       exe-cph-spt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-qta-ubi          .
       exe-cph-spt-100.
      *              *-------------------------------------------------*
      *              * Determinazione codice numerico prodotto         *
      *              *-------------------------------------------------*
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
      *              *-------------------------------------------------*
      *              * Determinazione buffer giacenze ubicazione       *
      *              *-------------------------------------------------*
           move      "PU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      w-det-num-pro-num    to   d-sld-ubi-num-mag      .
           move      "LTRA"               to   d-sld-ubi-cod-ubi      .
           move      spaces               to   d-sld-ubi-lit-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Test su esito determinazione                    *
      *              *-------------------------------------------------*
           if        d-sld-ubi-num-ele    =    zero
                     go to exe-cph-spt-900.
           if        d-sld-ubi-ubi-gia (1)
                                          not  = "LTRA   "
                     go to exe-cph-spt-900.
           if        d-sld-ubi-qta-gia (1)
                                          =    zero
                     go to exe-cph-spt-900.
      *              *-------------------------------------------------*
      *              * Valore di uscita                                *
      *              *-------------------------------------------------*
           move       d-sld-ubi-qta-gia (1)
                                          to   w-exe-qta-ubi          .
       exe-cph-spt-900.
      *              *-------------------------------------------------*
      *              * Editing valore in uscita                        *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-qta-ubi        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           display   p-edt                                            .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-spt-999.
       exe-cph-spt-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di determinazione codice alfanumerico prodotto *
      *    *-----------------------------------------------------------*
       exe-cph-lpd-000.
      *              *-------------------------------------------------*
      *              * Determinazione codice numerico prodotto         *
      *              *-------------------------------------------------*
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           display   w-det-num-pro-alf                                .
       exe-cph-lpd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lpd-999.
       exe-cph-lpd-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di determinazione giacenza prodotto per        *
      *    * ubicazione ed altri dati relativi                         *
      *    *-----------------------------------------------------------*
       exe-cph-gup-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-inx-ppr          .
           move      spaces               to   w-exe-ubi-ppr          .
           move      spaces               to   w-exe-ubi-ppp          .
       exe-cph-gup-100.
      *              *-------------------------------------------------*
      *              * Determinazione codice numerico prodotto         *
      *              *-------------------------------------------------*
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
      *              *-------------------------------------------------*
      *              * Determinazione ubicazione principale            *
      *              *-------------------------------------------------*
           move      "UP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      w-det-num-pro-num    to   d-sld-ubi-num-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *              *-------------------------------------------------*
      *              * Indice percorso e ubicazione principale         *
      *              *-------------------------------------------------*
           move      d-sld-ubi-inx-ppr    to   w-exe-inx-ppr          .
           move      d-sld-ubi-cod-ubi    to   w-exe-ubi-ppr          .
       exe-cph-gup-200.
      *              *-------------------------------------------------*
      *              * Determinazione buffer giacenze ubicazione       *
      *              *-------------------------------------------------*
           move      "PU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      w-det-num-pro-num    to   d-sld-ubi-num-mag      .
           move      spaces               to   d-sld-ubi-cod-ubi      .
           move      spaces               to   d-sld-ubi-lit-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
       exe-cph-gup-400.
      *              *-------------------------------------------------*
      *              * Emissione risultati in formato JSON             *
      *              *-------------------------------------------------*
           perform   exe-cph-jsn-000      thru exe-cph-jsn-999        .
       exe-cph-gup-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-gup-999.
       exe-cph-gup-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione ','                             *
      *              *-------------------------------------------------*
           move      ","                  to   w-det-fil-jsn-vrg      .
      *              *-------------------------------------------------*
      *              * Header per JSON                                 *
      *              *-------------------------------------------------*
           display   "Content-Type: application/json"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Apertura JSON                                   *
      *              *-------------------------------------------------*
           display   "["                                              .
           display   "   {"                                           .
       exe-cph-jsn-100.
      *              *-------------------------------------------------*
      *              * Ciclo castelletto ubicazioni                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-gia-ubi-ctr      .
       exe-cph-jsn-200.
           add       1                    to   w-det-gia-ubi-ctr      .
           if        w-det-gia-ubi-ctr    >    d-sld-ubi-num-ele
                     go to exe-cph-jsn-500.
      *              *-------------------------------------------------*
      *              * Test se giacenza ubicazione a zero              *
      *              *-------------------------------------------------*
           if        d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   =    zero
                     go to exe-cph-jsn-200.
       exe-cph-jsn-250.
      *              *-------------------------------------------------*
      *              * Codice ubicazione con giacenza                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento ubicazione principale           *
      *                  *---------------------------------------------*
           if        w-exe-ubi-ppr        not  = spaces
                     go to exe-cph-jsn-300.
           if        w-exe-ubi-ppp        not  = spaces
                     go to exe-cph-jsn-300.
           move      d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   to   w-exe-ubi-ppp          .
       exe-cph-jsn-300.
      *                  *---------------------------------------------*
      *                  * Codice ubicazione come label                *
      *                  *---------------------------------------------*
           move      d-sld-ubi-ubi-gia
                    (w-det-gia-ubi-ctr)   to   w-exe-jsn-lab          .
      *                  *---------------------------------------------*
      *                  * Editing quantita'                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      d-sld-ubi-qta-gia
                    (w-det-gia-ubi-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-400.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-200.
       exe-cph-jsn-500.
      *              *-------------------------------------------------*
      *              * Ubicazione principale                           *
      *              *-------------------------------------------------*
           move      "ubi_pri"            to   w-exe-jsn-lab          .
           move      w-exe-ubi-ppr        to   w-exe-jsn-val          .   
      *
           if        w-exe-jsn-val        =    spaces
                     move  w-exe-ubi-ppp  to   w-exe-jsn-val          .
      *
           if        w-exe-jsn-val        =    spaces
                     move  "n"            to   w-exe-jsn-val          .
      *
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-550.
      *              *-------------------------------------------------*
      *              * Indice di percorso                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se determinato                         *
      *                  *---------------------------------------------*
           if        w-exe-inx-ppr        not  = zero
                     go to exe-cph-jsn-600.
      *                  *---------------------------------------------*
      *                  * Determinazione indice di percorso           *
      *                  *---------------------------------------------*
           move      "IP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      w-exe-ubi-ppp        to   d-sld-ubi-cod-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                  *---------------------------------------------*
      *                  * Indice di percorso                          *
      *                  *---------------------------------------------*
           move      d-sld-ubi-inx-ppr    to   w-exe-inx-ppr          .
       exe-cph-jsn-600.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-inx-ppr        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "inx_pri"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-800.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di chiusura JSON           *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-fil-jsn-vrg      .
      *              *-------------------------------------------------*
      *              * Preparazione chiusura JSON                      *
      *              *-------------------------------------------------*
           move      "cls_jsn"            to   w-exe-jsn-lab          .
           move      "Z"                  to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
      *              *-------------------------------------------------*
      *              * Chiusura JSON                                   *
      *              *-------------------------------------------------*
           display   "   }"               with no advancing           .
           display   "]"                  with no advancing           .
       exe-cph-jsn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-999.
       exe-cph-jsn-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *                                                           *
      *    * Sub-subroutine di emissione singolo elemento              *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-emi-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-exe-jsn-lab        =    spaces or
                     w-exe-jsn-val        =    spaces
                     go to exe-cph-jsn-emi-900.
       exe-cph-jsn-emi-100.
       exe-cph-jsn-emi-200.
      *              *-------------------------------------------------*
      *              * Label                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario contatore                *
      *                  *---------------------------------------------*
           if        w-exe-jsn-ctr        =    "N"
                     go to exe-cph-jsn-emi-240.
       exe-cph-jsn-emi-220.
      *                  *---------------------------------------------*
      *                  * Label con contatore                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing contatore                       *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-fil-jsn-rig    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Label                                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-jsn-alf          .
           string    p-edt
                                delimited by   spaces
                     "."        delimited by   size
                     w-exe-jsn-lab
                                delimited by   spaces
                                          into w-exe-jsn-alf          .
      *                      *-----------------------------------------*
      *                      * A emissione                             *
      *                      *-----------------------------------------*
           go to     exe-cph-jsn-emi-400.
       exe-cph-jsn-emi-240.
      *                  *---------------------------------------------*
      *                  * Label senza contatore                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In campo di output                      *
      *                      *-----------------------------------------*
           move      w-exe-jsn-lab        to   w-exe-jsn-alf          .
      *                      *-----------------------------------------*
      *                      * A emissione                             *
      *                      *-----------------------------------------*
           go to     exe-cph-jsn-emi-400.
       exe-cph-jsn-emi-400.
      *              *-------------------------------------------------*
      *              * Label e Value                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    """"
                                delimited by   size
                     w-exe-jsn-alf
                                delimited by   size
                     """ : """
                                delimited by   size
                     w-exe-jsn-val
                                delimited by   size
                     """"
                                delimited by   size
                     w-det-fil-jsn-vrg
                                delimited by   spaces
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp        with no advancing           .
       exe-cph-jsn-emi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-emi-999.
       exe-cph-jsn-emi-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per messaggio di aggiornamento                 *
      *    *-----------------------------------------------------------*
       exe-cph-agg-000.
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
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *                                                           *
      *    * Campi previsti                                            *
      *    * --------------                                            *
      *    * 'tip_ope' = Tipo operazione                               *
      *    * 'prt_bfo' = Protocollo                                    *
      *    * 'alf_pro' = Codice prodotto alfanumerico                  *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome elemento        *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "tip_ope"
                     move  w-all-str-cat (2)
                                          to   w-exe-tip-ope
      *              *-------------------------------------------------*
      *              * Protocollo                                      *
      *              *-------------------------------------------------*
           else if   w-all-str-cat (1)    =    "prt_bfo"
                     move  w-all-str-cat (2)
                                          to   w-exe-prt-bfo
      *              *-------------------------------------------------*
      *              * Codice prodotto alfanumerico                    *
      *              *-------------------------------------------------*
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
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
           move      zero                 to   w-det-num-pro-des      .
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
           move      rf-dcp-des-pro       to   w-det-num-pro-des      .
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
       det-num-pro-700.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode confezione          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-cfz-000  thru det-num-pro-cfz-999    .
      *                  *---------------------------------------------*
      *                  * Test se codice prodotto determinato         *
      *                  *---------------------------------------------*
           if        w-det-num-pro-num    not  = zero
                     go to det-num-pro-900.
       det-num-pro-800.
      *              *-------------------------------------------------*
      *              * Eventuale scansione barcode imballo             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   det-num-pro-imb-000  thru det-num-pro-imb-999    .
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
      *    * Subroutine per barcode della confezione                   *
      *    *-----------------------------------------------------------*
       det-num-pro-cfz-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [pdk] per barcode confezione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-pdk-alf-pro         .
           move      "B"                  to   rf-pdk-tip-rec         .
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
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-200.
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
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-300.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-cfz-900.
           if        rf-pdk-tip-rec       not  = "B"
                     go to det-num-pro-cfz-900.
       det-num-pro-cfz-400.
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
       det-num-pro-cfz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-cfz-999.
       det-num-pro-cfz-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *                                                           *
      *    * Subroutine per barcode dell'imballo                       *
      *    *-----------------------------------------------------------*
       det-num-pro-imb-000.
      *              *-------------------------------------------------*
      *              * Start su archivio [pdk] per barcode confezione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-pdk-alf-pro         .
           move      "D"                  to   rf-pdk-tip-rec         .
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
                     go to det-num-pro-imb-900.
       det-num-pro-imb-200.
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
                     go to det-num-pro-imb-900.
       det-num-pro-imb-300.
      *              *-------------------------------------------------*
      *              * Test max su [pdk]                               *
      *              *-------------------------------------------------*
           if        rf-pdk-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-imb-900.
           if        rf-pdk-tip-rec       not  = "D"
                     go to det-num-pro-imb-900.
       det-num-pro-imb-400.
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
       det-num-pro-imb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-imb-999.
       det-num-pro-imb-999.
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
      *    * Controllo preliminare se documento stampabile [bfr]       *
      *    *-----------------------------------------------------------*
       ctl-prl-spn-bfr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-rig-das      .
           move      zero                 to   w-cix-ctr-rig-spu      .
       ctl-prl-spn-bfr-050.
      *              *-------------------------------------------------*
      *              * Lettura iniziale [bft]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       ctl-prl-spn-bfr-100.
      *              *-------------------------------------------------*
      *              * Start su [bfr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-bft-prt    to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : fine lettura           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  ctl-prl-spn-bfr-900.
       ctl-prl-spn-bfr-200.
      *              *-------------------------------------------------*
      *              * Next su [bfr]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se at end : fine lettura                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  ctl-prl-spn-bfr-900.
       ctl-prl-spn-bfr-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-slc-num-bft-prt
                     go to  ctl-prl-spn-bfr-900.
       ctl-prl-spn-bfr-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     go to  ctl-prl-spn-bfr-200.
       ctl-prl-spn-bfr-600.
      *              *-------------------------------------------------*
      *              * Scansione righe collo                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scansione                                   *
      *                  *---------------------------------------------*
           perform   ctl-prl-spn-bfk-000  thru ctl-prl-spn-bfk-999    .
      *                  *---------------------------------------------*
      *                  * Test su numero colli per la riga            *
      *                  *---------------------------------------------*
           if        w-cix-ctr-rig-cpr    =    zero
                     add  1               to   w-cix-ctr-rig-das      .
       ctl-prl-spn-bfr-700.
      *              *-------------------------------------------------*
      *              * Eventuale verifica spunta                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se riga non suddivisa in colli         *
      *                  *---------------------------------------------*
           if        w-cix-ctr-rig-cpr    not  = zero
                     go to ctl-prl-spn-bfr-800.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-bfr-num-prt       to   w-let-rec-bfs-prt      .
           move      rf-bfr-num-prg       to   w-let-rec-bfs-prg      .
           move      zero                 to   w-let-rec-bfs-prr      .
           perform   let-rec-bfs-000      thru let-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-let-rec-bfs-spn    =    "S"
                     add  1               to   w-cix-ctr-rig-spu      .
       ctl-prl-spn-bfr-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     ctl-prl-spn-bfr-200.
       ctl-prl-spn-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ctl-prl-spn-bfr-999.
       ctl-prl-spn-bfr-999.
           exit.

      *    *===========================================================*
      *    * Controllo preliminare se documento stampabile [bfk]       *
      *    *-----------------------------------------------------------*
       ctl-prl-spn-bfk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cix-ctr-rig-cpr      .
       ctl-prl-spn-bfk-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-bft-prt    to   rf-bfk-num-prt         .
           move      rf-bfr-num-prg       to   rf-bfk-num-prg         .
           move      zero                 to   rf-bfk-num-prc         .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : fine lettura           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  ctl-prl-spn-bfk-900.
       ctl-prl-spn-bfk-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bfk]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *                  *---------------------------------------------*
      *                  * Se at end : fine lettura                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  ctl-prl-spn-bfk-900.
       ctl-prl-spn-bfk-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
           if        rf-bfk-num-prt       not  = w-slc-num-bft-prt
                     go to  ctl-prl-spn-bfk-900.
           if        rf-bfk-num-prg       not  = rf-bfr-num-prg
                     go to  ctl-prl-spn-bfk-900.
       ctl-prl-spn-bfk-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
       ctl-prl-spn-bfk-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-cix-ctr-rig-cpr      .
           add       1                    to   w-cix-ctr-rig-das      .
       ctl-prl-spn-bfk-600.
      *              *-------------------------------------------------*
      *              * Lettura riga di spunta                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-bfk-num-prt       to   w-let-rec-bfs-prt      .
           move      rf-bfk-num-prg       to   w-let-rec-bfs-prg      .
           move      rf-bfk-num-prc       to   w-let-rec-bfs-prr      .
           perform   let-rec-bfs-000      thru let-rec-bfs-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di spunta                      *
      *                  *---------------------------------------------*
           if        w-let-rec-bfs-spn    =    "S"
                     add  1               to   w-cix-ctr-rig-spu      .
       ctl-prl-spn-bfk-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     ctl-prl-spn-bfk-200.
       ctl-prl-spn-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ctl-prl-spn-bfk-999.
       ctl-prl-spn-bfk-999.
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
