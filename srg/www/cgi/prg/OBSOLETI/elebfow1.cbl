       Identification Division.
       Program-Id.                                 elebfow0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 23/02/23    *
      *                       Ultima revisione:    NdK del 12/04/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Operazioni di scrittura [bfo]               *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    ___ OBSOLETO ___                            *
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
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .
      
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
           05  w-exe-rsp_doc              pic  x(03)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-prg-bfo              pic  x(05)                  .
           05  w-exe-pri-bfo              pic  x(03)                  .
           05  w-exe-col-bfo              pic  x(05)                  .
           05  w-exe-qta-bfo              pic  x(12)                  .
           05  w-exe-ann-bfo              pic  x(80)                  .
           05  w-exe-ubi-bfo              pic  x(07)                  .
           05  w-exe-snc-bfo              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-v01              pic  x(10)                  .
           05  w-exe-prm-v02              pic  x(20)                  .
           05  w-exe-prm-v03              pic  x(20)                  .
           05  w-exe-prm-v04              pic  x(15)                  .
           05  w-exe-prm-v05              pic  x(20)                  .
           05  w-exe-prm-v06              pic  x(20)                  .
           05  w-exe-prm-v07              pic  x(20)                  .
           05  w-exe-prm-v08              pic  x(90)                  .
           05  w-exe-prm-v09              pic  x(20)                  .
           05  w-exe-prm-v10              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qta-cnv              pic  9(11)                  .

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
           move      spaces               to   w-exe-prm-v01          .
           move      spaces               to   w-exe-prm-v02          .
           move      spaces               to   w-exe-prm-v03          .
           move      spaces               to   w-exe-prm-v04          .
           move      spaces               to   w-exe-prm-v05          .
           move      spaces               to   w-exe-prm-v06          .
           move      spaces               to   w-exe-prm-v07          .
           move      spaces               to   w-exe-prm-v08          .
           move      spaces               to   w-exe-prm-v09          .
           move      spaces               to   w-exe-prm-v10          .
      *
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-rsp_doc          .
           move      spaces               to   w-exe-prt-bfo          .
           move      spaces               to   w-exe-prg-bfo          .
           move      spaces               to   w-exe-col-bfo          .
           move      spaces               to   w-exe-pri-bfo          .
           move      spaces               to   w-exe-qta-bfo          .
           move      spaces               to   w-exe-ann-bfo          .
           move      spaces               to   w-exe-ubi-bfo          .
           move      spaces               to   w-exe-snc-bfo          .
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
           move      o-pst                to   w-all-str-alf          .
           move      "&"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *
           move      w-all-str-cat (1)    to   w-exe-prm-v01          .
           move      w-all-str-cat (2)    to   w-exe-prm-v02          .
           move      w-all-str-cat (3)    to   w-exe-prm-v03          .
           move      w-all-str-cat (4)    to   w-exe-prm-v04          .
           move      w-all-str-cat (5)    to   w-exe-prm-v05          .
           move      w-all-str-cat (6)    to   w-exe-prm-v06          .
           move      w-all-str-cat (7)    to   w-exe-prm-v07          .
           move      w-all-str-cat (8)    to   w-exe-prm-v08          .
           move      w-all-str-cat (9)    to   w-exe-prm-v09          .
           move      w-all-str-cat (10)   to   w-exe-prm-v10          .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      w-exe-prm-v01        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-tip-ope          .
      *                  *---------------------------------------------*
      *                  * Subroutine di estrazione parametri in       *
      *                  * funzione del tipo operazione                *
      *                  *---------------------------------------------*
           if        w-exe-tip-ope        =    "WC"
                     perform ext-prm-wcr-000
                                          thru ext-prm-wcr-999
           else if   w-exe-tip-ope        =    "WR"
                     perform ext-prm-wrr-000
                                          thru ext-prm-wrr-999
           else if   w-exe-tip-ope        =    "DE"
                     perform ext-prm-del-000
                                          thru ext-prm-del-999
           else if   w-exe-tip-ope        =    "CU"
                     perform ext-prm-ubi-000
                                          thru ext-prm-ubi-999        .
       ext-prm-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-999.
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di estrazione parametri per conferma riga      *
      *    *-----------------------------------------------------------*
       ext-prm-wcr-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Operatore                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-rsp-doc          .
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      w-exe-prm-v03        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prt-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo interno riga documento          *
      *                  *---------------------------------------------*
           move      w-exe-prm-v04        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prg-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga di comodo                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-v05        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-pri-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo collo                           *
      *                  *---------------------------------------------*
           move      w-exe-prm-v06        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-col-bfo          .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-v07        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-qta-bfo          .
      *                  *---------------------------------------------*
      *                  * Si/no check riga                            *
      *                  *---------------------------------------------*
           move      w-exe-prm-v08        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-snc-bfo          .
       ext-prm-wcr-500.
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
           move      03                   to   p-car                  .
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
       ext-prm-wcr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-wcr-999.
       ext-prm-wcr-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di estrazione parametri per scrittura riga     *
      *    *-----------------------------------------------------------*
       ext-prm-wrr-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Operatore                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-rsp-doc          .
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      w-exe-prm-v03        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prt-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo interno riga documento          *
      *                  *---------------------------------------------*
           move      w-exe-prm-v04        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prg-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga di comodo                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-v05        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-pri-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo collo                           *
      *                  *---------------------------------------------*
           move      w-exe-prm-v06        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-col-bfo          .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-v07        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-qta-bfo          .
      *                  *---------------------------------------------*
      *                  * Annotazioni                                 *
      *                  *---------------------------------------------*
           move      w-exe-prm-v08        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-ann-bfo          .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-v09        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-ubi-bfo          .
      *                  *---------------------------------------------*
      *                  * Si/no check riga                            *
      *                  *---------------------------------------------*
           move      w-exe-prm-v10        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-snc-bfo          .
       ext-prm-wrr-500.
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
           move      03                   to   p-car                  .
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
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-ubi-bfo        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-ubi-bfo          .
       ext-prm-wrr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-wrr-999.
       ext-prm-wrr-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di estrazione parametri per cancellazione      *
      *    *-----------------------------------------------------------*
       ext-prm-del-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prt-bfo          .
       ext-prm-del-500.
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
       ext-prm-del-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-del-999.
       ext-prm-del-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di estrazione parametri per cambio ubicazione  *
      *    *-----------------------------------------------------------*
       ext-prm-ubi-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-ubi-bfo          .
      *                  *---------------------------------------------*
      *                  * Uppercase                                   *
      *                  *---------------------------------------------*
           move      w-exe-ubi-bfo        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-ubi-bfo          .
       ext-prm-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ubi-999.
       ext-prm-ubi-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
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
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
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
           if        w-exe-tip-ope        =    "WC"
                     perform exe-cph-wcr-000
                                          thru exe-cph-wcr-999
           else if   w-exe-tip-ope        =    "WR"
                     perform exe-cph-wrr-000
                                          thru exe-cph-wrr-999
           else if   w-exe-tip-ope        =    "DE"
                     perform exe-cph-drs-000
                                          thru exe-cph-drs-999
           else if   w-exe-tip-ope        =    "CU"
                     perform exe-cph-ceu-000
                                          thru exe-cph-ceu-999
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
      *    * Subroutine di conferma riga reltiva a [bfr]               *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-bfr-000.
      *              *-------------------------------------------------*
      *              * Lettura preliminare [bfr]                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bfr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bfr]                        *
      *                  *---------------------------------------------*
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
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      rf-bfr-qta-acq       to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      spaces               to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      spaces               to   rf-bfs-alx-exp         .
       exe-cph-wcr-bfr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
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
       exe-cph-wcr-bfr-400.
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
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      rf-bfr-qta-acq       to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      spaces               to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      spaces               to   rf-bfs-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Messaggio per aggiornamento eseguito        *
      *                  *---------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-bfr-900.
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
      *    * Subroutine di conferma riga reltiva a [bfk]               *
      *    *-----------------------------------------------------------*
       exe-cph-wcr-bfk-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bfr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bfr]                        *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bfk]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfk                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bfk]                        *
      *                  *---------------------------------------------*
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
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      rf-bfk-qta-prc       to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      spaces               to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      spaces               to   rf-bfs-alx-exp         .
       exe-cph-wcr-bfk-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
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
       exe-cph-wcr-bfk-400.
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
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      rf-bfk-qta-prc       to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      spaces               to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      spaces               to   rf-bfs-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Messaggio per aggiornamento eseguito        *
      *                  *---------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wcr-bfk-900.
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
      *    * Subroutine di scrittura riga                              *
      *    *-----------------------------------------------------------*
       exe-cph-wrr-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test di esistenza ubicazione                *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-ubi-bfo        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-wrr-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice ubicazione           *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-ubi-bfo          .
       exe-cph-wrr-100.
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
                     go to exe-cph-wrr-600.
       exe-cph-wrr-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento                                   *
      *              *-------------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-qta-cnv        to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      w-exe-ann-bfo        to   rf-bfs-ann-spn         .
           move      spaces               to   rf-bfs-alx-exp         .
       exe-cph-wrr-300.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [bfs]                      *
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
       exe-cph-wrr-400.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
      *              *-------------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     exe-cph-wrr-900.
       exe-cph-wrr-600.
      *              *-------------------------------------------------*
      *              * Se record [bfs] non trovato                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record                      *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-slc-num-bft-prt    to   rf-bfs-num-prt         .
           move      w-slc-num-bft-prg    to   rf-bfs-num-prg         .
           move      w-slc-num-bft-col    to   rf-bfs-num-prr         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe        to   rf-bfs-ide-dat         .
           move      w-exe-qta-cnv        to   rf-bfs-qta-ril         .
           move      zero                 to   rf-bfs-num-pro         .
           move      w-exe-snc-bfo        to   rf-bfs-flg-spn         .
______*    move      w-exe-rsp-doc        to   rf-bfs-sgl-odm         .
           move      w-exe-ubi-bfo        to   rf-bfs-cod-ub1         .
           move      spaces               to   rf-bfs-cod-ub2         .
           move      spaces               to   rf-bfs-cod-ub3         .
           move      spaces               to   rf-bfs-cod-ub4         .
           move      w-exe-ann-bfo        to   rf-bfs-ann-spn         .
           move      zero                 to   rf-bfs-cod-ncf         .
           move      spaces               to   rf-bfs-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Put record                                  *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Messaggio per aggiornamento eseguito        *
      *                  *---------------------------------------------*
           perform   exe-cph-agg-000      thru exe-cph-agg-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-wrr-900.
       exe-cph-wrr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-wrr-999.
       exe-cph-wrr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per messaggio di aggiornamento                 *
      *    *-----------------------------------------------------------*
       exe-cph-agg-000.
      *              *-------------------------------------------------*
      *              * Formattazione output                            *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Editing                                         *
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
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           
           move      "<h1> AGGIORNAMENTO RIGA"
                                          to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           move      "</h1>"              to   w-all-str-cat (3)      .
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
      *    * Subroutine di cancellazione righe di spunta               *
      *    *-----------------------------------------------------------*
       exe-cph-drs-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero protocollo                   *
      *                  *---------------------------------------------*
           if        w-slc-num-bft-prt    not  = zero
                     go to exe-cph-drs-100.
      *                  *---------------------------------------------*
      *                  * Errore in uscita                            *
      *                  *---------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<h1> Manca iL PROTOCOLLO </h1>"                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-drs-900.
       exe-cph-drs-100.
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
                     go to  exe-cph-drs-900.
       exe-cph-drs-200.
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
                     go to  exe-cph-drs-900.
       exe-cph-drs-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-slc-num-bft-prt
                     go to  exe-cph-drs-900.
       exe-cph-drs-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfr]                              *
      *              *-------------------------------------------------*
       exe-cph-drs-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave [bfs]                       *
      *              *-------------------------------------------------*
           move      rf-bfr-num-prt       to   rf-bfs-num-prt         .
           move      rf-bfr-num-prg       to   rf-bfs-num-prg         .
           move      zero                 to   rf-bfs-num-prr         .
      *              *-------------------------------------------------*
      *              * Delete record [bfs]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       exe-cph-drs-600.
      *              *-------------------------------------------------*
      *              * Sub-ciclo su [bfk]                              *
      *              *-------------------------------------------------*
           perform   exe-cph-bfk-000      thru exe-cph-bfk-999        .
       exe-cph-drs-800.
      *              *-------------------------------------------------*
      *              * Riciclo su [bfr]                                *
      *              *-------------------------------------------------*
           go to     exe-cph-drs-200.
       exe-cph-drs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-drs-999.
       exe-cph-drs-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di cancellazione righe di spunta               *
      *    *                                                           *
      *    * Sub-subroutine di scansione righe colli                   *
      *    *-----------------------------------------------------------*
       exe-cph-bfk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       exe-cph-bfk-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-bfr-num-prt       to   rf-bfk-num-prt         .
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
                     go to exe-cph-bfk-900.
       exe-cph-bfk-200.
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
                     go to exe-cph-bfk-900.
       exe-cph-bfk-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
           if        rf-bfk-num-prt       not  = rf-bfr-num-prt
                     go to  exe-cph-bfk-900.
           if        rf-bfk-num-prg       not  = rf-bfr-num-prg
                     go to  exe-cph-bfk-900.
       exe-cph-bfk-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
       exe-cph-bfk-500.
      *              *-------------------------------------------------*
      *              * Composizione chiave [bfs]                       *
      *              *-------------------------------------------------*
           move      rf-bfk-num-prt       to   rf-bfs-num-prt         .
           move      rf-bfk-num-prg       to   rf-bfs-num-prg         .
           move      rf-bfk-num-prc       to   rf-bfs-num-prr         .
      *              *-------------------------------------------------*
      *              * Delete record [bfs]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
       exe-cph-bfk-800.
      *              *-------------------------------------------------*
      *              * Riciclo su [bfk]                                *
      *              *-------------------------------------------------*
           go to     exe-cph-bfk-200.
       exe-cph-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-bfk-999.
       exe-cph-bfk-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di controllo esistenza ubicazione              *
      *    *-----------------------------------------------------------*
       exe-cph-ceu-000.
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
       exe-cph-ceu-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [zub]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                       *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zub-dpz      .
           move      w-exe-ubi-bfo        to   w-let-arc-zub-cod      .
           perform   let-arc-zub-000      thru let-arc-zub-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        w-let-arc-zub-flg    =    spaces
                     go to exe-cph-ceu-900.
       exe-cph-ceu-200.
      *              *-------------------------------------------------*
      *              * Se lettura errata                               *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<h1> CODICE UBICAZIONE :"
                                          to   w-all-str-cat (1)      .
           move      w-exe-ubi-bfo        to   w-all-str-cat (2)      .
           move      "NON ESISTENTE </h1>"
                                          to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-exe-prm-msg          .
      *              *-------------------------------------------------*
      *              * Messaggio in uscita                             *
      *              *-------------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   w-exe-prm-msg                                    .
           display   "</div>"                                         .
       exe-cph-ceu-900.
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
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-ceu-999.
       exe-cph-ceu-999.
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
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di uscita per errore                           *
      *    *-----------------------------------------------------------*
       exe-cph-err-000.
      *              *-------------------------------------------------*
      *              * Esito dell'aggiornamento                        *
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
