       Identification Division.
       Program-Id.                                 eleabbD0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:    bfo                 *
      *                                   Fase:    eleabb              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/02/25    *
      *                       Ultima revisione:    NdK del 19/02/26    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Carichi di magazzino                        *
      *                                                                *
      *                    Preparazione JSON per dati (eleabb01.php)   *
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
      *        *-------------------------------------------------------*
      *        * [bfk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfk"                          .
      *        *-------------------------------------------------------*
      *        * [bfe]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfe"                          .
      *        *-------------------------------------------------------*
      *        * [zrm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzrm"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .

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
           05  w-exe-ide-doc              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Parametri interni                                     *
      *        *-------------------------------------------------------*
           05  w-exe-sgl-ctn              pic  x(20)                  .
           05  w-exe-prt-doc              pic  9(11)                  .
           05  w-exe-num-doc              pic  x(10)                  .
           05  w-exe-sts-doc              pic  x(01)                  .
           05  w-exe-cts-num              pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione riga tabella               *
      *        *-------------------------------------------------------*
           05  w-exe-rig.
               10  w-exe-rig-prt          pic  9(11)                  .
               10  w-exe-rig-prg          pic  9(05)                  .
               10  w-exe-flg-spn          pic  x(01)                  .
               10  w-exe-rig-qta          pic  9(11)                  .
               10  w-exe-rig-qtv          pic  9(11)                  .
               10  w-exe-rig-col          pic  9(03)                  .
               10  w-exe-rig-ctn          pic  x(15)                  .
               10  w-exe-ann-ncf          pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per regolarizzazioni                           *
      *        *-------------------------------------------------------*
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-ele              pic  9(05)                  .
           05  w-exe-ctr-bfk              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodi vari                                           *
      *        *-------------------------------------------------------*
           05  w-exe-wrk-prt              pic  x(06)                  .
           05  w-exe-lun-prt              pic  9(02)                  .
           05  w-exe-saa-prt              pic  x(03)                  .
           05  w-exe-dpz-prt              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per emissione JSON                             *
      *        *-------------------------------------------------------*
           05  w-exe-rif-001              pic  x(80)                  .
           05  w-exe-rif-002              pic  x(80)                  .
           05  w-exe-jsn-lab              pic  x(07)                  .
           05  w-exe-jsn-val              pic  x(80)                  .
           05  w-exe-jsn-ctr              pic  x(01)                  .
           05  w-exe-jsn-alf              pic  x(11)                  .
      *        *-------------------------------------------------------*
      *        * Stringa di comodo per display                         *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    * per ubicazione                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

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
               10  w-slc-num-bft-dds      pic  9(07)                  .
               10  w-slc-num-bft-nds      pic  9(11)                  .
               10  w-slc-num-bft-nds-r redefines
                   w-slc-num-bft-nds.
                   15  w-slc-num-bft-nsa  pic  9(03)                  .
                   15  w-slc-num-bft-ndp  pic  9(02)                  .
                   15  w-slc-num-bft-npg  pic  9(06)                  .
               10  w-slc-num-prt-num      pic  9(09)                  .
               10  w-slc-num-prt-num-r    redefines
                   w-slc-num-prt-num.
                   15  w-slc-num-prt-saa  pic  9(03)                  .
                   15  w-slc-num-prt-prg  pic  9(06)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Determinazione se documento in distinta di verifica   *
      *        *-------------------------------------------------------*
           05  w-det-dst-num-prt          pic  9(11)                  .
           05  w-det-dst-num-dst          pic  9(09)                  .
           05  w-det-dst-sts-dst          pic  x(40)                  .
           05  w-det-dst-nik-nam          pic  x(20)                  .
           05  w-det-dst-ctr-ele          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det responsabile spunte documento            *
      *        *-------------------------------------------------------*
           05  w-det-rsm-doc.
      *            *---------------------------------------------------*
      *            * Protocollo documento                              *
      *            *---------------------------------------------------*
               10  w-det-rsm-doc-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Responsabile                                      *
      *            *---------------------------------------------------*
               10  w-det-rsm-doc-rsm      pic  9(05)                  .
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
      *        * Per determinazione componente numerica contrassegno   *
      *        *-------------------------------------------------------*
           05  w-det-cmp-num.
      *            *---------------------------------------------------*
      *            * Codice alfanumerico contrassegno                  *
      *            *---------------------------------------------------*
               10  w-det-cmp-num-alf      pic  x(15)                  .
      *            *---------------------------------------------------*
      *            * Componente numerica                               *
      *            *---------------------------------------------------*
               10  w-det-cmp-num-num      pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-ndq      pic  9(01)                  .
               10  w-let-arc-dcp-tip      pic  9(02)                  .
               10  w-let-arc-dcp-klb      pic  x(14)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zrm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zrm.
               10  w-let-arc-zrm-flg      pic  x(01)                  .
               10  w-let-arc-zrm-cod      pic  9(05)                  .
               10  w-let-arc-zrm-des      pic  x(40)                  .
               10  w-let-arc-zrm-fds      pic  x(01)                  .

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
           move      spaces               to   w-exe-ide-doc          .
           move      spaces               to   w-exe-sgl-ctn          .
           move      zero                 to   w-exe-prt-doc          .
           move      spaces               to   w-exe-num-doc          .
           move      spaces               to   w-exe-sts-doc          .
           move      zero                 to   w-exe-cts-num          .
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
      *                  * Eliminazione eventuale prefisso contras-    *
      *                  * segno collo                                 *
      *                  *---------------------------------------------*
           if        w-exe-ide-doc
                    (01 : 05)             =    "%2BC1"
                     move  w-exe-ide-doc
                          (06 : 15)       to   w-exe-ide-doc          .
      *                  *---------------------------------------------*
      *                  * Estrazione componente numerica contrassegno *
      *                  * collo                                       *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      20                   to   p-car                  .
           move      w-exe-ide-doc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-cts-num          .
      *                  *---------------------------------------------*
      *                  * Numero collo in uppercase                   *
      *                  *---------------------------------------------*
           move      w-exe-ide-doc        to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-ide-doc          .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di eventuale estrazione protocollo             *
      *    *-----------------------------------------------------------*
       ext-prm-prt-000.
      *              *-------------------------------------------------*
      *              * Test su valore del campo in input               *
      *              *-------------------------------------------------*
           if        w-exe-ide-doc        =    spaces
                     go to ext-prm-prt-900.
           if        w-exe-prt-doc        not  = zero
                     go to ext-prm-prt-900.
       ext-prm-prt-050.
      *              *-------------------------------------------------*
      *              * Test su presenza del carattere '.'              *
      *              *-------------------------------------------------*
           move      w-exe-ide-doc        to   w-all-str-alf          .
           move      "."                  to   w-all-str-del          .
           perform   all-str-chr-000      thru all-str-chr-999        .
           if        w-all-str-num        =    1
                     perform ext-prm-prt-sep-000
                                          thru ext-prm-prt-sep-999
                     go to ext-prm-prt-900.
       ext-prm-prt-100.
      *              *-------------------------------------------------*
      *              * Test su lunghezza del campo in input            *
      *              *-------------------------------------------------*
           move      w-exe-ide-doc        to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           if        w-all-str-lun        >    6
                     go to ext-prm-prt-400.
       ext-prm-prt-200.
      *              *-------------------------------------------------*
      *              * Se lunghezza di 6 caratteri o inferiore         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lunghezza stringa                           *
      *                  *---------------------------------------------*
           move      w-all-str-lun        to   w-exe-lun-prt          .
      *                  *---------------------------------------------*
      *                  * Secolo/anno da data di sistema              *
      *                  *---------------------------------------------*
           move      w-exe-dat-exe
                    (01 : 03)             to   w-exe-saa-prt          .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      "01"                 to   w-exe-dpz-prt          .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-exe-saa-prt        to   w-all-str-cat (1)      .
           move      w-exe-dpz-prt        to   w-all-str-cat (2)      .
      *
           if        w-exe-lun-prt        =    1
                     move  "00000"        to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    2
                     move  "0000"         to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    3
                     move  "000"          to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    4
                     move  "00"           to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    5
                     move  "0"            to   w-all-str-cat (3)
           else      move  spaces         to   w-all-str-cat (3)      .
      *
           move      w-exe-ide-doc        to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-prt-doc          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     ext-prm-prt-900.
       ext-prm-prt-400.
      *              *-------------------------------------------------*
      *              * Se lunghezza 11                                 *
      *              *-------------------------------------------------*
           if        w-all-str-lun        not  = 11
                     go to ext-prm-prt-900.
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-ide-doc        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-prt-doc          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     ext-prm-prt-900.
       ext-prm-prt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-prt-999.
       ext-prm-prt-999.
           exit.

      *    *===========================================================*
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di eventuale estrazione protocollo             *
      *    *                                                           *
      *    * Sub-subroutine di estrazione protocollo e anno            *
      *    *-----------------------------------------------------------*
       ext-prm-prt-sep-000.
      *              *-------------------------------------------------*
      *              * Estrazione                                      *
      *              *-------------------------------------------------*
           move      w-exe-ide-doc        to   w-all-str-alf          .
           move      "."                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
           move      w-all-str-cat (2)    to   w-exe-wrk-prt          .
      *              *-------------------------------------------------*
      *              * Estrazione secolo/anno                          *
      *              *-------------------------------------------------*
           move      w-all-str-cat (1)    to   w-exe-saa-prt
                                              (02 : 02)               .
           move      "1"                  to   w-exe-saa-prt
                                              (01 : 01)               .
      *              *-------------------------------------------------*
      *              * Test su lunghezza del componente numero         *
      *              *-------------------------------------------------*
           move      w-exe-wrk-prt        to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           move      w-all-str-lun        to   w-exe-lun-prt          .
       ext-prm-prt-sep-200.
      *              *-------------------------------------------------*
      *              * Codice dipendenza                               *
      *              *-------------------------------------------------*
           move      "01"                 to   w-exe-dpz-prt          .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-exe-saa-prt        to   w-all-str-cat (1)      .
           move      w-exe-dpz-prt        to   w-all-str-cat (2)      .
      *
           if        w-exe-lun-prt        =    1
                     move  "00000"        to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    2
                     move  "0000"         to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    3
                     move  "000"          to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    4
                     move  "00"           to   w-all-str-cat (3)
           else if   w-exe-lun-prt        =    5
                     move  "0"            to   w-all-str-cat (3)
           else      move  spaces         to   w-all-str-cat (3)      .
      *
           move      w-exe-wrk-prt        to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-prt-doc          .
       ext-prm-prt-sep-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-prt-sep-999.
       ext-prm-prt-sep-999.
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
      *              * [bfe]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfe                 .
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
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magaz-   *
      *              * zino per ubicazione                             *
      *              *-------------------------------------------------*
           perform   det-sld-ubi-opn-000  thru det-sld-ubi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magazz.  *
      *              *-------------------------------------------------*
           perform   det-sld-mag-opn-000  thru det-sld-mag-opn-999    .
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
      *              * [bfe]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfe                 .
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
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magaz-  *
      *              * zino per ubicazione                             *
      *              *-------------------------------------------------*
           perform   det-sld-ubi-cls-000  thru det-sld-ubi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magazz. *
      *              *-------------------------------------------------*
           perform   det-sld-mag-cls-000  thru det-sld-mag-cls-999    .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-ele          .
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Ricerca record testata da stampare tramite il   *
      *              * numero collo                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca                                     *
      *                  *---------------------------------------------*
           perform   exe-cph-ctn-000      thru exe-cph-ctn-999        .
      *                  *---------------------------------------------*
      *                  * Test se record trovato                      *
      *                  *---------------------------------------------*
           if        w-exe-prt-doc        not  = zero
                     go to exe-cph-400.
       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Ricerca record testata da stampare tramite il   *
      *              * numero documento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ricerca                                     *
      *                  *---------------------------------------------*
           perform   exe-cph-ndo-000      thru exe-cph-ndo-999        .
      *                  *---------------------------------------------*
      *                  * Test se record trovato                      *
      *                  *---------------------------------------------*
           if        w-exe-prt-doc        not  = zero
                     go to exe-cph-400.
       exe-cph-300.
      *              *-------------------------------------------------*
      *              * Trattamento protocollo                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Formattazione                               *
      *                  *---------------------------------------------*
           perform   ext-prm-prt-000      thru ext-prm-prt-999        .
      *                  *---------------------------------------------*
      *                  * Test se record trovato                      *
      *                  *---------------------------------------------*
           if        w-exe-prt-doc        not  = zero
                     go to exe-cph-400.
      *                  *---------------------------------------------*
      *                  * Status 'Non trovato'                        *
      *                  *---------------------------------------------*
           move      "N"                  to   w-exe-sts-doc          .
      *                  *---------------------------------------------*
      *                  * Ad uscita JSON                              *
      *                  *---------------------------------------------*
           go to     exe-cph-800.
       exe-cph-400.
      *              *-------------------------------------------------*
      *              * Lettura record testata da stampare              *
      *              *-------------------------------------------------*
           perform   exe-cph-lte-000      thru exe-cph-lte-999        .
      *                  *---------------------------------------------*
      *                  * Test se documento non trovato               *
      *                  *---------------------------------------------*
           if        w-exe-sts-doc        =    "N"
                     go to exe-cph-600.
       exe-cph-500.
      *              *-------------------------------------------------*
      *              * Test se fornitore ABB                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-bft-cod-arc       =    0003239
                     go to exe-cph-600.
      *                  *---------------------------------------------*
      *                  * Status 'Altro fornitore'                    *
      *                  *---------------------------------------------*
           move      "X"                  to   w-exe-sts-doc          .
       exe-cph-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero documento                *
      *              *-------------------------------------------------*
           move      rf-bft-num-doc       to   w-exe-num-doc          .
       exe-cph-800.
      *              *-------------------------------------------------*
      *              * Emissione risultati in formato JSON             *
      *              *-------------------------------------------------*
           perform   exe-cph-jsn-000      thru exe-cph-jsn-999        .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *                                                           *
      *    * Subroutine di determinazione se documento in distinta     *
      *    *-----------------------------------------------------------*
       exe-cph-bfe-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dst-ctr-ele      .
           move      zero                 to   w-det-dst-num-dst      .
           move      spaces               to   w-det-dst-sts-dst      .
           move      spaces               to   w-det-dst-nik-nam      .
       exe-cph-bfe-100.
      *              *-------------------------------------------------*
      *              * Start su [bfe]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DOCDST    "         to   f-key                  .
           move      w-det-dst-num-prt    to   rf-bfe-prt-doc         .
           move      zero                 to   rf-bfe-num-dst         .
           move      zero                 to   rf-bfe-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfe                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bfe-900.
       exe-cph-bfe-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [bfe]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfe"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfe                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to exe-cph-bfe-900.
       exe-cph-bfe-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bfe-prt-doc       not  = w-det-dst-num-prt
                     go to exe-cph-bfe-900.
       exe-cph-bfe-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       exe-cph-bfe-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-dst-ctr-ele      .
      *              *-------------------------------------------------*
      *              * Numero distinta                                 *
      *              *-------------------------------------------------*
           move      rf-bfe-num-dst       to   w-det-dst-num-dst      .
      *              *-------------------------------------------------*
      *              * Nickname distinta                               *
      *              *-------------------------------------------------*
           move      rf-bfe-nik-nam       to   w-det-dst-nik-nam      .
       exe-cph-bfe-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-bfe-999.
       exe-cph-bfe-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di ricerca documento tramite collo (CTN)       *
      *    *-----------------------------------------------------------*
       exe-cph-ctn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-prt-doc          .
       exe-cph-ctn-050.
      *              *-------------------------------------------------*
      *              * Test iniziali                                   *
      *              *-------------------------------------------------*
           if        w-exe-ide-doc        =    spaces
                     go to exe-cph-ctn-900.
       exe-cph-ctn-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk] per contrassegno                 *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CTSPRC    "         to   f-key                  .
           move      w-exe-ide-doc        to   rf-bfk-cts-prc         .
           move      zero                 to   rf-bfk-num-prt         .
           move      zero                 to   rf-bfk-num-prg         .
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
                     go to  exe-cph-ctn-900.
       exe-cph-ctn-200.
      *              *-------------------------------------------------*
      *              * Next su [bfk]                                   *
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
                     go to  exe-cph-ctn-900.
       exe-cph-ctn-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bfk-cts-prc       not  = w-exe-ide-doc
                     go to  exe-cph-ctn-900.
       exe-cph-ctn-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       exe-cph-ctn-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione protocollo individuato          *
      *              *-------------------------------------------------*
           move      rf-bfk-num-prt       to   w-exe-prt-doc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione identificativo come CTN         *
      *              *-------------------------------------------------*
           move      w-exe-ide-doc        to   w-exe-sgl-ctn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione record testata                  *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Lettura record testata                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-exe-prt-doc        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero documento                *
      *              *-------------------------------------------------*
           move      rf-bft-num-doc       to   w-exe-num-doc          .
       exe-cph-ctn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-ctn-999.
       exe-cph-ctn-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di ricerca documento tramite numero            *
      *    *-----------------------------------------------------------*
       exe-cph-ndo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare protocollo          *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-prt-doc          .
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-exe-ide-doc        =    spaces
                     go to exe-cph-ndo-900.
       exe-cph-ndo-100.
      *              *-------------------------------------------------*
      *              * Start su [bfr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RCHARCDOC "         to   f-key                  .
           move      01                   to   rf-bfr-cod-dpz         .
           move      spaces               to   rf-bfr-flg-rch         .
           move      "F"                  to   rf-bfr-tip-arc         .
           move      zero                 to   rf-bfr-cod-arc         .
           move      w-exe-ide-doc        to   rf-bfr-num-doc         .
           move      zero                 to   rf-bfr-num-prt         .
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
                     go to  exe-cph-ndo-900.
       exe-cph-ndo-200.
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
                     go to  exe-cph-ndo-900.
       exe-cph-ndo-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza e tipo archivio   *
      *                  *---------------------------------------------*
           if        rf-bfr-cod-dpz       not  = 01     or
                     rf-bfr-flg-rch       not  = spaces or
                     rf-bfr-tip-arc       not  = "F"
                     go to  exe-cph-ndo-900.
       exe-cph-ndo-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     go to  exe-cph-ndo-200.
      *                  *---------------------------------------------*
      *                  * Test su numero documento                    *
      *                  *---------------------------------------------*
           if        w-exe-ide-doc        not  = spaces        and
                     rf-bfr-num-doc       not  = w-exe-ide-doc
                     go to  exe-cph-ndo-200.
       exe-cph-ndo-500.
      *              *-------------------------------------------------*
      *              * Protocollo determinato                          *
      *              *-------------------------------------------------*
           move      rf-bfr-num-prt       to   w-exe-prt-doc          .
       exe-cph-ndo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-ndo-999.
       exe-cph-ndo-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di lettura testata documento                   *
      *    *-----------------------------------------------------------*
       exe-cph-lte-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di status                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-sts-doc          .
       exe-cph-lte-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [bft]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Lettura record testata da stampare              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-exe-prt-doc        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
       exe-cph-lte-400.
      *              *-------------------------------------------------*
      *              * Flag di status                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se documento non trovato                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "N"            to   w-exe-sts-doc          .
      *                  *---------------------------------------------*
      *                  * Se documento chiuso                         *
      *                  *---------------------------------------------*
           if        rf-bft-flg-bch       not  = spaces
                     move  "C"            to   w-exe-sts-doc          .
       exe-cph-lte-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-lte-999.
       exe-cph-lte-999.
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
       exe-cph-jsn-200.
      *              *-------------------------------------------------*
      *              * Subroutine di trattamento testata               *
      *              *-------------------------------------------------*
           perform   exe-cph-jsn-tes-000  thru exe-cph-jsn-tes-999    .
      *              *-------------------------------------------------*
      *              * Se status a 'N': ad emissione valori JSON       *
      *              *-------------------------------------------------*
           if        w-exe-sts-doc        =    "N"
                     go to exe-cph-jsn-600.
       exe-cph-jsn-400.
      *              *-------------------------------------------------*
      *              * Subroutine di trattamento righe                 *
      *              *-------------------------------------------------*
           perform   exe-cph-jsn-bfr-000  thru exe-cph-jsn-bfr-999    .
       exe-cph-jsn-600.
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
      *    * Sub-subroutine di emissione JSON per dati di testata      *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-tes-000.
       exe-cph-jsn-tes-100.
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *                                                 *
      *              * - 'N' : Non trovato                             *
      *              * - 'C' : Chiuso                                  *
      *              * - 'X' : Altro fornitore (non ABB)               *
      *              *-------------------------------------------------*
           move      "sta_tus"            to   w-exe-jsn-lab          .
           move      w-exe-sts-doc        to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
      *              *-------------------------------------------------*
      *              * Se status a 'N': ad uscita                      *
      *              *-------------------------------------------------*
           if        w-exe-sts-doc        =    "N"
                     go to exe-cph-jsn-tes-900.
       exe-cph-jsn-tes-200.
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      "num_doc"            to   w-exe-jsn-lab          .
           move      w-exe-num-doc        to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-200.
      *              *-------------------------------------------------*
      *              * Protocollo documento - interno                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "num_prt"            to   w-exe-jsn-lab          .
           move      rf-bft-num-prt
                    (01 : 11)             to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-200.
      *              *-------------------------------------------------*
      *              * Protocollo documento - formattato               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scorporo dipendenza                         *
      *                  *---------------------------------------------*
           move      rf-bft-num-prt       to   w-slc-num-bft-nds      .
           move      w-slc-num-bft-nsa    to   w-slc-num-prt-saa      .
           move      w-slc-num-bft-npg    to   w-slc-num-prt-prg      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-prt-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (9)      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "prt_doc"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-250.
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-bft-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "dat_doc"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-300.
      *              *-------------------------------------------------*
      *              * Codice fornitore                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-bft-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "cod_fnt"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-350.
      *              *-------------------------------------------------*
      *              * Distinta                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-bft-num-prt       to   w-det-dst-num-prt      .
           perform   exe-cph-bfe-000      thru exe-cph-bfe-999        .
      *                  *---------------------------------------------*
      *                  * Scorporo dipendenza                         *
      *                  *---------------------------------------------*
           move      w-det-dst-num-dst    to   w-slc-num-bft-nds      .
           move      w-slc-num-bft-nsa    to   w-slc-num-prt-saa      .
           move      w-slc-num-bft-npg    to   w-slc-num-prt-prg      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-prt-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (9)      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "num_dst"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
      *              *-------------------------------------------------*
      *              * Distinta - literal                              *
      *              *-------------------------------------------------*
           move      "lit_dst"            to   w-exe-jsn-lab          .
           move      w-det-dst-nik-nam    to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-400.
      *              *-------------------------------------------------*
      *              * Protocollo di magazzino                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scorporo dipendenza                         *
      *                  *---------------------------------------------*
           move      rf-bft-pr2-mag       to   w-slc-num-bft-nds      .
           move      w-slc-num-bft-nsa    to   w-slc-num-prt-saa      .
           move      w-slc-num-bft-npg    to   w-slc-num-prt-prg      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-prt-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (9)      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "prt_mag"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-425.
      *              *-------------------------------------------------*
      *              * Data registrazione di magazzino                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data                                *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-bft-dat-reg       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "drg_mag"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-450.
      *              *-------------------------------------------------*
      *              * Sigla responsabile                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione responsabile                 *
      *                  *---------------------------------------------*
           move      rf-bft-num-prt       to   w-det-rsm-doc-prt      .
           perform   det-rsm-doc-000      thru det-rsm-doc-999        .
      *                  *---------------------------------------------*
      *                  * Lettura responsabile                        *
      *                  *---------------------------------------------*
           move      w-det-rsm-doc-rsm    to   w-let-arc-zrm-cod      .
           perform   let-arc-zrm-000      thru let-arc-zrm-999        .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "des_rsm"            to   w-exe-jsn-lab          .
           move      w-let-arc-zrm-des    to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-800.
      *              *-------------------------------------------------*
      *              * Ubicazioni virtuali                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "UV"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      "ubi_vrt"            to   w-exe-jsn-lab          .
           move      d-sld-ubi-lit-ubi    to   w-exe-jsn-val          .
           move      "N"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-tes-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-tes-999.
       exe-cph-jsn-tes-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per emissione risultati in formato JSON        *
      *    *                                                           *
      *    * Sub-subroutine di emissione JSON per dati riga            *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-bfr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-fil-jsn-rig      .
      *              *-------------------------------------------------*
      *              * Protocollo per scansione                        *
      *              *-------------------------------------------------*
           move      rf-bft-num-prt       to   w-exe-rig-prt          .
       exe-cph-jsn-bfr-100.
      *              *-------------------------------------------------*
      *              * Start su [bfr]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-exe-rig-prt        to   rf-bfr-num-prt         .
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
                     go to  exe-cph-jsn-bfr-900.
       exe-cph-jsn-bfr-200.
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
                     go to  exe-cph-jsn-bfr-900.
       exe-cph-jsn-bfr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bfr-num-prt       not  = w-exe-rig-prt
                     go to  exe-cph-jsn-bfr-900.
       exe-cph-jsn-bfr-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     go to  exe-cph-jsn-bfr-200.
       exe-cph-jsn-bfr-500.
      *              *-------------------------------------------------*
      *              * Test preliminare se per la riga in corso di     *
      *              * trattamento esistono dei colli                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Progressivo riga in comodo di trattamento   *
      *                  *---------------------------------------------*
           move      rf-bfr-num-prg       to   w-exe-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           perform   exe-cph-jsn-bfk-000  thru exe-cph-jsn-bfk-999    .
           if        w-exe-ctr-bfk        =    zero
                     go to exe-cph-jsn-bfr-700.
       exe-cph-jsn-bfr-600.
      *                  *---------------------------------------------*
      *                  * Se colli                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione colli                *
      *                      *-----------------------------------------*
           perform   exe-cph-jsn-col-000  thru exe-cph-jsn-col-999    .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     exe-cph-jsn-bfr-200.
       exe-cph-jsn-bfr-700.
      *                  *---------------------------------------------*
      *                  * Se senza colli                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura spunte riga                     *
      *                      *-----------------------------------------*
           move      rf-bfr-num-prt       to   w-let-rec-bfs-prt      .
           move      rf-bfr-num-prg       to   w-let-rec-bfs-prg      .
           move      zero                 to   w-let-rec-bfs-prr      .
           perform   let-rec-bfs-000      thru let-rec-bfs-999        .
           move      w-let-rec-bfs-qta    to   w-exe-rig-qtv          .
           move      w-let-rec-bfs-spn    to   w-exe-flg-spn          .
           move      w-let-rec-bfs-ncf    to   w-exe-ann-ncf          .
      *                      *-----------------------------------------*
      *                      * Preparazione elementi riga              *
      *                      *-----------------------------------------*
           move      rf-bfr-num-prt       to   w-exe-rig-prt          .
           move      rf-bfr-num-prg       to   w-exe-rig-prg          .
           move      zero                 to   w-exe-rig-col          .
           move      "(no)"               to   w-exe-rig-ctn          .
           move      rf-bfr-qta-acq       to   w-exe-rig-qta          .
      *                      *-----------------------------------------*
      *                      * Emissione riga                          *
      *                      *-----------------------------------------*
           perform   exe-cph-jsn-rig-000  thru exe-cph-jsn-rig-999    .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     exe-cph-jsn-bfr-800.
       exe-cph-jsn-bfr-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-bfr-200.
       exe-cph-jsn-bfr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-bfr-999.
       exe-cph-jsn-bfr-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di test preliminare esistenza colli            *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-bfk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-bfk          .
       exe-cph-jsn-bfk-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-exe-rig-prt        to   rf-bfk-num-prt         .
           move      w-exe-rig-prg        to   rf-bfk-num-prg         .
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
                     go to  exe-cph-jsn-bfk-900.
       exe-cph-jsn-bfk-200.
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
                     go to  exe-cph-jsn-bfk-900.
       exe-cph-jsn-bfk-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
           if        rf-bfk-num-prt       not  = w-exe-rig-prt
                     go to  exe-cph-jsn-bfk-900.
           if        rf-bfk-num-prg       not  = w-exe-rig-prg
                     go to  exe-cph-jsn-bfk-900.
       exe-cph-jsn-bfk-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
       exe-cph-jsn-bfk-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-ctr-bfk          .
       exe-cph-jsn-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-jsn-bfk-999.
       exe-cph-jsn-bfk-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per ciclo di scansione colli (CTN)             *
      *    *-----------------------------------------------------------*
        exe-cph-jsn-col-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
        exe-cph-jsn-col-100.
      *              *-------------------------------------------------*
      *              * Start su [bfk]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-exe-rig-prt        to   rf-bfk-num-prt         .
           move      w-exe-rig-prg        to   rf-bfk-num-prg         .
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
                     go to   exe-cph-jsn-col-900.
        exe-cph-jsn-col-200.
      *              *-------------------------------------------------*
      *              * Next su [bfk]                                   *
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
                     go to   exe-cph-jsn-col-900.
        exe-cph-jsn-col-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfk-num-prt       not  = w-exe-rig-prt or
                     rf-bfk-num-prg       not  = w-exe-rig-prg
                     go to   exe-cph-jsn-col-900.
        exe-cph-jsn-col-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contrassegno collo                  *
      *                  *---------------------------------------------*
           if        w-exe-sgl-ctn        =    spaces
                     go to   exe-cph-jsn-col-500.
      *                  *---------------------------------------------*
      *                  * Il confronto avviene sulla componente       *
      *                  * numerica del contrassegno                   *
      *                  *---------------------------------------------*
           move      rf-bfk-cts-prc       to   w-det-cmp-num-alf      .
           perform   det-cmp-num-000      thru det-cmp-num-999        .
           if        w-det-cmp-num-num    not  = w-exe-cts-num
                     go to   exe-cph-jsn-col-200.
        exe-cph-jsn-col-500.
      *              *-------------------------------------------------*
      *              * Letture complementari                           *
      *              *-------------------------------------------------*
        exe-cph-jsn-col-600.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura spunte riga                         *
      *                  *---------------------------------------------*
           move      rf-bfk-num-prt       to   w-let-rec-bfs-prt      .
           move      rf-bfk-num-prg       to   w-let-rec-bfs-prg      .
           move      rf-bfk-num-prc       to   w-let-rec-bfs-prr      .
      *                  *---------------------------------------------*
      *                  *              ___ IMPORTANTE ___             *
      *                  *                                             *
      *                  * Per mantenere la compatibilita' con le      *
      *                  * spunte registrate col metodo precedente,    *
      *                  * si azzera il progressivo collo per i do-    *
      *                  * menti verificati prima del __/__/__         *
      *                  *---------------------------------------------*
           
           if        rf-bfr-dat-reg       <    1250409
                     move  zero           to   w-let-rec-bfs-prr      .
      *
           
           
           perform   let-rec-bfs-000      thru let-rec-bfs-999        .
           move      w-let-rec-bfs-qta    to   w-exe-rig-qtv          .
           move      w-let-rec-bfs-spn    to   w-exe-flg-spn          .
           move      w-let-rec-bfs-ncf    to   w-exe-ann-ncf          .
      *                  *---------------------------------------------*
      *                  * Preparazione elementi riga                  *
      *                  *---------------------------------------------*
           move      rf-bfk-num-prt       to   w-exe-rig-prt          .
           move      rf-bfk-num-prg       to   w-exe-rig-prg          .
           move      rf-bfk-num-prc       to   w-exe-rig-col          .
           move      rf-bfk-cts-prc       to   w-exe-rig-ctn          .
           move      rf-bfk-qta-prc       to   w-exe-rig-qta          .
      *                  *---------------------------------------------*
      *                  * Emissione riga                              *
      *                  *---------------------------------------------*
           perform   exe-cph-jsn-rig-000  thru exe-cph-jsn-rig-999    .
        exe-cph-jsn-col-800.
      *              *-------------------------------------------------*
      *              * Riciclo a record successivo                     *
      *              *-------------------------------------------------*
           go to      exe-cph-jsn-col-200.
        exe-cph-jsn-col-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to      exe-cph-jsn-col-999.
        exe-cph-jsn-col-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di emissione della riga                        *
      *    *-----------------------------------------------------------*
       exe-cph-jsn-rig-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-det-fil-jsn-rig      .
       exe-cph-jsn-rig-100.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura prodotto                            *
      *                  *---------------------------------------------*
           move      rf-bfr-num-mag       to   w-let-arc-dcp-num      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [aaq]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Lettura codice per il Produttore            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-bfr-tip-mag       to   rf-aaq-tip-mag         .
           move      rf-bfr-num-mag       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Determinazione buffer giacenze ubicazione   *
      *                  *---------------------------------------------*
           move      "PU"                 to   d-sld-ubi-tip-ope      .
           move      w-exe-dat-exe        to   d-sld-ubi-dat-ela      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-bfr-num-mag       to   d-sld-ubi-num-mag      .
           move      spaces               to   d-sld-ubi-cod-ubi      .
           move      spaces               to   d-sld-ubi-lit-ubi      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
       exe-cph-jsn-rig-200.
      *              *-------------------------------------------------*
      *              * Emissione dati per la riga                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Progressivo [bfr]                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-rig-prg        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "num_prg"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-250.
      *                  *---------------------------------------------*
      *                  * Progressivo [bfk]                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-rig-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "num_prc"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-300.
      *                  *---------------------------------------------*
      *                  * Codice prodotto numerico                    *
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
           move      rf-bfr-num-mag       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "num_pro"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-350.
      *                  *---------------------------------------------*
      *                  * Codice prodotto alfanumerico                *
      *                  *---------------------------------------------*
           move      "alf_pro"            to   w-exe-jsn-lab          .
           move      rf-bfr-alf-mag       to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-400.
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
           move      "des_pro"            to   w-exe-jsn-lab          .
           move      w-let-arc-dcp-des    to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-450.
      *                  *---------------------------------------------*
      *                  * Barcode prodotto                            *
      *                  *---------------------------------------------*
           move      "bcd_pro"            to   w-exe-jsn-lab          .
           move      w-let-arc-dcp-klb    to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-500.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il produttore           *
      *                  *---------------------------------------------*
           move      "cdp_pdt"            to   w-exe-jsn-lab          .
           move      rf-aaq-cdp-pdt       to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-550.
      *                  *---------------------------------------------*
      *                  * Ubicazioni prodotto                         *
      *                  *---------------------------------------------*
           move      "ubi_pro"            to   w-exe-jsn-lab          .
           move      d-sld-ubi-lit-ubi    to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-600.
      *                  *---------------------------------------------*
      *                  * Quantita' in riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GD"                to   p-edm                  .
           move      w-exe-rig-qta        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "qta_ord"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-650.
      *                  *---------------------------------------------*
      *                  * Quantita' del collo                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<D"                 to   p-edm                  .
           move      w-exe-rig-qtv        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "qta_vrf"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-700.
      *                  *---------------------------------------------*
      *                  * Flag di spunta                              *
      *                  *---------------------------------------------*
           move      "flg_spn"            to   w-exe-jsn-lab          .
           move      w-exe-flg-spn        to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-750.
      *                  *---------------------------------------------*
      *                  * Ubicazione principale prodotto              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione ubicazione principale    *
      *                      *-----------------------------------------*
           move      "UP"                 to   d-sld-ubi-tip-ope      .
           move      01                   to   d-sld-ubi-cod-dpz      .
           move      01                   to   d-sld-ubi-tip-mag      .
           move      rf-bfr-num-mag       to   d-sld-ubi-num-mag      .
           perform   det-sld-ubi-cll-000  thru det-sld-ubi-cll-999    .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "ubi_pri"            to   w-exe-jsn-lab          .
           move      d-sld-ubi-cod-ubi    to   w-exe-jsn-val          .
      *
           if        d-sld-ubi-cod-ubi    =    spaces
                     move  "n"            to   w-exe-jsn-val          .
      *
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-800.
      *                  *---------------------------------------------*
      *                  * Collo (CTN)                                 *
      *                  *---------------------------------------------*
           move      "num_ctn"            to   w-exe-jsn-lab          .
           move      w-exe-rig-ctn        to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-850.
      *                  *---------------------------------------------*
      *                  * Giacenza                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      w-exe-dat-exe        to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      01                   to   d-sld-mag-cod-dpz      .
           move      01                   to   d-sld-mag-tip-mag      .
           move      rf-bfr-num-mag       to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
           perform   det-sld-mag-cll-000  thru det-sld-mag-cll-999    .
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<GD"                to   p-edm                  .
           move      d-sld-mag-sld-mag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      "qta_gia"            to   w-exe-jsn-lab          .
           move      p-edt                to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
       exe-cph-jsn-rig-870.
      *                  *---------------------------------------------*
      *                  * Note Non conformita'                        *
      *                  *---------------------------------------------*
           move      "ann_ncf"            to   w-exe-jsn-lab          .
           move      w-exe-ann-ncf        to   w-exe-jsn-val          .
           move      "S"                  to   w-exe-jsn-ctr          .
           perform   exe-cph-jsn-emi-000  thru exe-cph-jsn-emi-999    .
        exe-cph-jsn-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to      exe-cph-jsn-rig-999.
        exe-cph-jsn-rig-999.
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
      *    * Determinazione responsabile spunte documento              *
      *    *-----------------------------------------------------------*
       det-rsm-doc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-rsm-doc-rsm      .
       det-rsm-doc-100.
      *              *-------------------------------------------------*
      *              * Start su [bfs]                                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-det-rsm-doc-prt    to   rf-bfs-num-prt         .
           move      zero                 to   rf-bfs-num-prg         .
           move      zero                 to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : fine lettura           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-rsm-doc-900.
       det-rsm-doc-200.
      *              *-------------------------------------------------*
      *              * Next su [bfs]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se at end : fine lettura                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  det-rsm-doc-900.
       det-rsm-doc-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfs-num-prt       not  = w-exe-prt-doc
                     go to  det-rsm-doc-900.
       det-rsm-doc-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfs]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice responsabile                 *
      *                  *---------------------------------------------*
           if        rf-bfs-cod-rsm       =    zero
                     go to  det-rsm-doc-200.
       det-rsm-doc-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice responsabile             *
      *              *-------------------------------------------------*
           move      rf-bfs-cod-rsm       to   w-det-rsm-doc-rsm      .
       det-rsm-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-rsm-doc-999.
       det-rsm-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della componente numerica contrassegno     *
      *    *-----------------------------------------------------------*
       det-cmp-num-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cmp-num-num      .
       det-cmp-num-100.
      *              *-------------------------------------------------*
      *              * Conversione da alfanumerico a numerico          *
      *              *-------------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      15                   to   p-car                  .
           move      w-det-cmp-num-alf    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-det-cmp-num-num      .
       det-cmp-num-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cmp-num-999.
       det-cmp-num-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zrm]                         *
      *    *-----------------------------------------------------------*
       let-arc-zrm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zrm-cod    =    zero
                     go to let-arc-zrm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODRSP"             to   f-key                  .
           move      w-let-arc-zrm-cod    to   rf-zrm-cod-rsp         .
           move      "pgm/mag/fls/ioc/obj/iofzrm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zrm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zrm-400.
       let-arc-zrm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zrm-des-rsp       to   w-let-arc-zrm-des      .
           move      rf-zrm-flg-spn       to   w-let-arc-zrm-fds      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zrm-999.
       let-arc-zrm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zrm-flg      .
           move      all   "."            to   w-let-arc-zrm-des      .
           move      "?"                  to   w-let-arc-zrm-fds      .
           go to     let-arc-zrm-600.
       let-arc-zrm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zrm-des      .
           move      spaces               to   w-let-arc-zrm-fds      .
       let-arc-zrm-600.
       let-arc-zrm-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp]                            *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Prodotto di vendita a zero       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-ndq      .
           move      rf-dcp-tip-pro       to   w-let-arc-dcp-tip      .
           move      rf-dcp-klb-pro       to   w-let-arc-dcp-klb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-des      .
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
       let-arc-dcp-600.
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-ndq      .
           move      zero                 to   w-let-arc-dcp-tip      .
           move      spaces               to   w-let-arc-dcp-klb      .
       let-arc-dcp-999.
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
      *    * Estrazione parametri                                      *
      *    *                                                           *
      *    * Subroutine di assegnazione del valore in base al nome del *
      *    * campo in input                                            *
      *    *-----------------------------------------------------------*
       ext-prm-ass-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del nome campo           *
      *              *-------------------------------------------------*
           if        w-all-str-cat (1)    =    "ide_doc"
                     move  w-all-str-cat (2)
                                          to   w-exe-ide-doc          .
       ext-prm-ass-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-ass-999.
       ext-prm-ass-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino per     *
      *    * ubicazione                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dts"                   .

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
