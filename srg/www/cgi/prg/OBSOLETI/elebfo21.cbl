       Identification Division.
       Program-Id.                                 elebfo21           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 23/02/23    *
      *                       Ultima revisione:    NdK del 17/04/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Documenti di carico HTML con barcode        *
      *                                                                *
      *                    Versione da 1 riga per volta                *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    ___ NON UTILIZZATO ___                      *
      *                                                                *
      *                    ___ IN LAVORAZIONE ___                      *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Indice di percorso ubicazione                     *
      *            *---------------------------------------------------*
               10  srt-inx-key            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico prodotto per ordinamento      *
      *            *---------------------------------------------------*
               10  srt-alf-key            pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Progressivo riga per ordinamento                  *
      *            *---------------------------------------------------*
               10  srt-rig-key            pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Parametri di ubicazione                           *
      *            *---------------------------------------------------*
               10  srt-prm-ubi            pic  x(28)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico prodotto                          *
      *            *---------------------------------------------------*
               10  srt-num-pro            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico prodotto                      *
      *            *---------------------------------------------------*
               10  srt-alf-pro            pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto selezionato                       *
      *            *---------------------------------------------------*
               10  srt-sel-pro            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di presenza colli                            *
      *            *---------------------------------------------------*
               10  srt-snx-pcr            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Record [bfr]                                      *
      *            *---------------------------------------------------*
               10  srt-rec-bfr.
                   15  filler  occurs  1536
                                          pic  x(01)                  .

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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .
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
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdk"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .
      
      *    *===========================================================*
      *    * Work-area richieste                                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Numero ordine                                         *
      *        *-------------------------------------------------------*
           05  rr-num-ord                 pic  9(09)                  .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di esecuzione                                    *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Stringhe di comodo per display                        *
      *        *-------------------------------------------------------*
           05  w-exe-str-dsp              pic  x(512)                 .
           05  w-exe-str-chd              pic  x(15)                  .
           05  w-exe-str-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Flag di comodo                                        *
      *        *-------------------------------------------------------*
           05  w-exe-flg-ini              pic  x(01)                  .
           05  w-exe-flg-scn              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-rig              pic  9(05)                  .
           05  w-exe-ctr-bfk              pic  9(05)                  .
           05  w-exe-sel-pro              pic  9(05)                  .
           05  w-exe-snx-pcr              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-cod-ute              pic  x(03)                  .
           05  w-exe-des-ute              pic  x(20)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-qta-pro              pic  x(20)                  .
           05  w-exe-num-pro              pic  9(07)                  .
           05  w-exe-ann-rig              pic  x(80)                  .
           05  w-exe-cts-bfo              pic  x(20)                  .
           05  w-exe-cts-num              pic  9(13)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-v01              pic  x(20)                  .
           05  w-exe-prm-v02              pic  x(20)                  .
           05  w-exe-prm-v03              pic  x(25)                  .
           05  w-exe-prm-v04              pic  x(20)                  .
           05  w-exe-prm-v05              pic  x(20)                  .
           05  w-exe-prm-v06              pic  x(20)                  .
           05  w-exe-prm-vx1              pic  x(20)                  .
           05  w-exe-prm-vx2              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione tag generico               *
      *        *-------------------------------------------------------*
           05  w-exe-tag.
               10  w-exe-tag-tag          pic  x(20)                  .
               10  w-exe-tag-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione tag 'hidden'               *
      *        *-------------------------------------------------------*
           05  w-exe-hid.
               10  w-exe-hid-nam          pic  x(03)                  .
               10  w-exe-hid-val          pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione tag tabella                *
      *        *-------------------------------------------------------*
           05  w-exe-ttd.
               10  w-exe-ttd-snh          pic  x(01)                  .
               10  w-exe-ttd-tg1          pic  x(20)                  .
               10  w-exe-ttd-tg2          pic  x(30)                  .
               10  w-exe-ttd-txt          pic  x(120)                 .
               10  w-exe-ttd-col          pic  9(02)                  .
               10  w-exe-ttd-cow          pic  x(20)                  .
               10  w-exe-ttd-all          pic  x(01)                  .
               10  w-exe-ttd-alw          pic  x(20)                  .
               10  w-exe-ttd-wdt          pic  x(01)                  .
               10  w-exe-ttd-wdw          pic  x(20)                  .
               10  w-exe-ttd-stl          pic  x(01)                  .
               10  w-exe-ttd-st1          pic  x(25)                  .
               10  w-exe-ttd-st2          pic  x(20)                  .
               10  w-exe-ttd-cli          pic  x(07)                  .
               10  w-exe-ttd-prg          pic  x(05)                  .
               10  w-exe-ttd-prr          pic  x(03)                  .
               10  w-exe-ttd-nco          pic  x(03)                  .
               10  w-exe-ttd-ub1          pic  x(07)                  .
               10  w-exe-ttd-ub2          pic  x(07)                  .
               10  w-exe-ttd-ub3          pic  x(07)                  .
               10  w-exe-ttd-ub4          pic  x(07)                  .
               10  w-exe-ttd-cki          pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione riga tabella               *
      *        *-------------------------------------------------------*
           05  w-exe-rig.
               10  w-exe-rig-prt          pic  9(11)                  .
               10  w-exe-rig-prg          pic  9(05)                  .
               10  w-exe-rig-col          pic  9(03)                  .
               10  w-exe-rig-sco          pic  x(15)                  .
               10  w-exe-rig-qta          pic  9(11)                  .
               10  w-exe-flg-spn          pic  x(01)                  .
               10  w-exe-sgl-odm          pic  x(03)                  .
               10  w-exe-cod-ub1          pic  x(07)                  .
               10  w-exe-rwi-idx          pic  x(07)                  .
               10  w-exe-lun-prt          pic  9(02)                  .
               10  w-exe-saa-prt          pic  x(03)                  .
               10  w-exe-dpz-prt          pic  x(02)                  .
               10  w-exe-num-prt          pic  x(11)                  .

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
      *    * Area di comunicazione per determinazione ubicazione       *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

      *    *===========================================================*
      *    * Work area per esecuzione stampa                           *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Sub-work per intestazione pagina                      *
      *        *-------------------------------------------------------*
           05  w-stp-int.
      *            *---------------------------------------------------*
      *            * Campi di comodo                                   *
      *            *---------------------------------------------------*
               10  w-stp-int-nde          pic  x(08)                  .
               10  w-stp-int-ndo          pic  9(11)                  .
               10  w-stp-int-ndo-r redefines
                   w-stp-int-ndo.
                   15  w-stp-int-ndo-saa  pic  9(03)                  .
                   15  w-stp-int-ndo-dpz  pic  9(02)                  .
                   15  w-stp-int-ndo-prg  pic  9(06)                  .
               10  w-stp-int-npr          pic  9(09)                  .
               10  w-stp-int-npr-r redefines
                   w-stp-int-npr.
                   15  w-stp-int-npr-saa  pic  9(03)                  .
                   15  w-stp-int-npr-prg  pic  9(06)                  .

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
               10  w-slc-num-prt-num      pic  9(09)                  .
               10  w-slc-num-prt-num-r    redefines
                   w-slc-num-prt-num.
                   15  w-slc-num-prt-saa  pic  9(03)                  .
                   15  w-slc-num-prt-prg  pic  9(06)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-sel      pic  x(01)                  .
               10  w-slc-num-bft-toc      pic  x(05)                  .
               10  w-slc-num-bft-dat      pic  9(07)                  .
               10  w-slc-num-bft-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-bft-crb      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

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
      *
           move      spaces               to   w-exe-cod-ute          .
           move      spaces               to   w-exe-prt-bfo          .
           move      spaces               to   w-exe-alf-pro          .
           move      spaces               to   w-exe-qta-pro          .
           move      zero                 to   w-exe-num-pro          .
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
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Operatore                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-v01        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-cod-ute          .
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      w-exe-prm-v02        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prt-bfo          .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  * ___ TEST SE CODICE O NUMERO COLLO ___       *
      *                  *---------------------------------------------*
           move      w-exe-prm-v03        to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-alf-pro          .
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Regolarizzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           perform   ext-prm-prt-000      thru ext-prm-prt-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione operatore                       *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    "CRO"
                     move  "Crozzoletto"  to   w-exe-des-ute
           else if   w-exe-cod-ute        =    "LUC"
                     move  "Lucio"        to   w-exe-des-ute
           else if   w-exe-cod-ute        =    "PIN"
                     move  "Pin"          to   w-exe-des-ute
           else if   w-exe-cod-ute        =    "RIC"
                     move  "Riccardo"     to   w-exe-des-ute
           else      move  "???"          to   w-exe-des-ute          .
      *                  *---------------------------------------------*
      *                  * Codice prodotto - trattamento '/'           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se a spazi                         *
      *                      *-----------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to ext-prm-900.
      *                      *-----------------------------------------*
      *                      * Uppercase                               *
      *                      *-----------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      20                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
      *                      *-----------------------------------------*
      *                      * Test se presente il carattere '%'       *
      *                      *-----------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-chr-000      thru all-str-chr-999        .
           if        w-all-str-num        =    zero
                     go to ext-prm-900.
      *                      *-----------------------------------------*
      *                      * Scomposizione                           *
      *                      *-----------------------------------------*
           move      w-exe-alf-pro        to   w-all-str-alf          .
           move      "%"                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *                      *-----------------------------------------*
      *                      * Test sui componenti                     *
      *                      *-----------------------------------------*
           if        w-all-str-cat (2)
                    (01 : 02)             not  = "2F"
                     go to ext-prm-900.
           move      w-all-str-cat (1)    to   w-exe-prm-vx1          .
           move      w-all-str-cat (2)
                    (03 : 12)             to   w-exe-prm-vx2          .
      *                      *-----------------------------------------*
      *                      * Riassemblaggio                          *
      *                      *-----------------------------------------*
           move      14                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-exe-prm-vx1        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      w-exe-prm-vx2        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-alf-pro          .
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
      *    * Subroutine di trattamento protocollo                      *
      *    *-----------------------------------------------------------*
       ext-prm-prt-000.
      *              *-------------------------------------------------*
      *              * Test su valore del campo in input               *
      *              *-------------------------------------------------*
           if        w-exe-prt-bfo        =    spaces
                     go to ext-prm-prt-900.
       ext-prm-prt-100.
      *              *-------------------------------------------------*
      *              * Test su lunghezza del campo in input            *
      *              *-------------------------------------------------*
           move      w-exe-prt-bfo        to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           if        w-all-str-lun        >    6
                     go to ext-prm-prt-300.
       ext-prm-prt-200.
      *              *-------------------------------------------------*
      *              * Se lunghezza di 6 caratteri o inferiore         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lunghezzaz stringa                          *
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
           else      move  "0"            to   w-all-str-cat (3)      .
      *
           move      w-exe-prt-bfo        to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-prt-bfo          .
       ext-prm-prt-300.
      *              *-------------------------------------------------*
      *              * Se lunghezza maggiore di 6 caratteri            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conversione in numerico                     *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      11                   to   p-car                  .
           move      w-exe-prt-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-prt      .
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
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * [ybf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
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
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione ubicazioni        *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-opn-000  thru det-prm-ubi-opn-999    .
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
      *              * [ybf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
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
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
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
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione ubicazioni       *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-cls-000  thru det-prm-ubi-cls-999    .
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
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-pre-000      thru exe-cph-pre-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-ini        not  = spaces
                     go to exe-cph-800.
       
       



       exe-cph-200.
      *              *-------------------------------------------------*
      *              * Eventuale stampa intestazione pagina            *
      *              *-------------------------------------------------*
           perform   emi-htm-int-000      thru emi-htm-int-999        .
      *              *-------------------------------------------------*
      *              * Stampa testata documento                        *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-000      thru stp-tes-doc-999        .
      *              *-------------------------------------------------*
      *              * Stampa corpo documento                          *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-000      thru stp-cor-doc-999        .
      *              *-------------------------------------------------*
      *              * Stampa piede documento                          *
      *              *-------------------------------------------------*
           perform   stp-pie-doc-000      thru stp-pie-doc-999        .
      *              *-------------------------------------------------*
      *              * Chiusura html                                   *
      *              *-------------------------------------------------*
           perform   emi-htm-cls-000      thru emi-htm-cls-999        .




           go to     exe-cph-900.
       
       
       
       
       
       exe-cph-800.
      *              *-------------------------------------------------*
      *              * Uscita con messaggio                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   emi-htm-int-000      thru emi-htm-int-999        .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
      *
           move      90                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<h1>"               to   w-all-str-cat (1)      .
           move      w-exe-str-msg        to   w-all-str-cat (2)      .
           move      "</h1>"              to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
      *
           display   "</div>"                                         .
      *                  *---------------------------------------------*
      *                  * Chiusura html                               *
      *                  *---------------------------------------------*
           perform   emi-htm-cls-000      thru emi-htm-cls-999        .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-900.
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
      *    * Subroutine per operazioni preliminari                     *
      *    *-----------------------------------------------------------*
       exe-cph-pre-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-ini          .
       exe-cph-pre-100.
      *              *-------------------------------------------------*
      *              * Test preliminare su operatore                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        not  = spaces
                     go to exe-cph-pre-200.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "SELEZIONARE UN OPERATORE!"
                                          to   w-exe-str-msg          .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-800.
       exe-cph-pre-200.
      *              *-------------------------------------------------*
      *              * Test preliminare su numero ordine               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-prt-bfo        not  = spaces
                     go to exe-cph-pre-300.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "INSERIRE UN NUMERO DOCUMENTO!"
                                          to   w-exe-str-msg          .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-800.
       exe-cph-pre-300.
      *              *-------------------------------------------------*
      *              * Lettura record testata da stampare              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bft-prt    to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
           if        f-sts                =    e-not-err
                     go to exe-cph-pre-400.
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita con erro- *
      *                  * re                                          *
      *                  *---------------------------------------------*
           move      "DOCUMENTO NON TROVATO!"
                                          to   w-exe-str-msg          .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-800.
       exe-cph-pre-400.
      *              *-------------------------------------------------*
      *              * Test se documento chiuso                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-bft-flg-bch       =    spaces
                     go to exe-cph-pre-500.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "DOCUMENTO CHIUSO!"  to   w-exe-str-msg          .
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-800.
       exe-cph-pre-500.
      *              *-------------------------------------------------*
      *              * Scansione preliminare righe documento per de-   *
      *              * terminare i contatori                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   exe-cph-scn-000      thru exe-cph-scn-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita                      *
      *                  *---------------------------------------------*
           if        w-exe-flg-scn        =    spaces
                     go to exe-cph-pre-600.
      *                  *---------------------------------------------*
      *                  * Ad uscita con errore                        *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-800.
       exe-cph-pre-600.
      *              *-------------------------------------------------*
      *              * ___                                             *
      *              *-------------------------------------------------*


      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-pre-900.
       exe-cph-pre-800.
      *              *-------------------------------------------------*
      *              * Flag di uscita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-exe-flg-ini          .
       exe-cph-pre-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-pre-999.
       exe-cph-pre-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Scansione preliminare righe documento                     *
      *    *-----------------------------------------------------------*
       exe-cph-scn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-scn          .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-rig          .
       exe-cph-scn-100.
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
      *                  * Se errore di start : a test di uscita       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  exe-cph-scn-800.
       exe-cph-scn-200.
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
      *                  * Se at end :  a test di uscita               *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  exe-cph-scn-800.
       exe-cph-scn-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo :  a test di uscita         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-slc-num-bft-prt
                     go to  exe-cph-scn-800.
       exe-cph-scn-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     go to  exe-cph-scn-200.
       exe-cph-scn-500.
      *              *-------------------------------------------------*
      *              * Incremento contatori                            *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-ctr-rig          .

       exe-cph-scn-700.
      *              *-------------------------------------------------*
      *              * Riciclo a record [bfr] successivo               *
      *              *-------------------------------------------------*
           go to     exe-cph-scn-200.
       exe-cph-scn-800.
      *              *-------------------------------------------------*
      *              * Test su contatori                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se almeno una riga documento                *
      *                  *---------------------------------------------*
           if        w-exe-ctr-rig        >    zero
                     go to exe-cph-scn-900.


      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-exe-flg-scn          .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "DOCUMENTO SENZA RIGHE!"
                                          to   w-exe-str-msg          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     exe-cph-scn-900.


      
      
       exe-cph-scn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-scn-999.
       exe-cph-scn-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *-----------------------------------------------------------*
       stp-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Inizio testata                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A capo                                      *
      *                  *---------------------------------------------*
           display   "<br>"                                           .
      *                  *---------------------------------------------*
      *                  * Intestazione tabella                        *
      *                  *---------------------------------------------*
           display   "<table bgcolor=#FFFFFF border=1 cellspacing=1 cell
      -              "padding=3 width=100% id='tes'>"                 .
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "O"                  to   w-exe-tag-flg          .
           move      "tr"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-tes-doc-100.
      *                  *---------------------------------------------*
      *                  * Estremi documento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing protocollo                      *
      *                      *-----------------------------------------*
           move      w-slc-num-bft-prt    to   w-slc-num-bft-nds      .
           move      w-slc-num-bft-nsa    to   w-slc-num-prt-saa      .
           move      w-slc-num-bft-npg    to   w-slc-num-prt-prg      .
      *
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-prt-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *                      *-----------------------------------------*
      *                      * Editing data documento                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-bft-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (4)      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      70                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      rf-bft-cod-tmb       to   w-all-str-cat (1)      .
           move      "del"                to   w-all-str-cat (3)      .
           move      "- numero :"         to   w-all-str-cat (5)      .
           move      rf-bft-num-doc       to   w-all-str-cat (6)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      w-all-str-alf        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "B"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           move      "tr"                 to   w-exe-tag-tag          .
           move      "C"                  to   w-exe-tag-flg          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "tr"                 to   w-exe-tag-tag          .
           move      "O"                  to   w-exe-tag-flg          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-tes-doc-200.
      *                  *---------------------------------------------*
      *                  * Fornitore                                   *
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
           move      rf-bft-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-exe-ttd-cli          .
       stp-tes-doc-300.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica fornitore            *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-bft-cod-arc       to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-fnt-rag-soc         .
       stp-tes-doc-320.
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      70                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-exe-ttd-cli        to   w-all-str-cat (1)      .
           move      rf-bft-dpz-arc       to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           move      rf-fnt-rag-soc       to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      w-all-str-alf        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "B"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
      *                  *---------------------------------------------*
      *                  * Chiusura tabella                            *
      *                  *---------------------------------------------*
           display   "</table>"                                       .
       stp-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *-----------------------------------------------------------*
       stp-cor-doc-000.
      *              *-------------------------------------------------*
      *              * Intestazione colonne                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A capo                                      *
      *                  *---------------------------------------------*
           display   "<br>"                                           .
      *                  *---------------------------------------------*
      *                  * Intestazione tabella                        *
      *                  *---------------------------------------------*
           display   "<table bgcolor=#FFFFFF border=1 cellspacing=1 cell
      -              "padding=3 width=100% id='rig' name='rig'>"      .
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           move      "O"                  to   w-exe-tag-flg          .
           move      "tr"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Progressivo riga                *
      *                  *---------------------------------------------*
           move      "NR"                 to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Progressivo collo               *
      *                  *---------------------------------------------*
           move      "C"                 to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Codice                          *
      *                  *---------------------------------------------*
           move      "Codice"             to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Descrizione                     *
      *                  *---------------------------------------------*
           move      "Descrizione"        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per codice Produttore               *
      *                  *---------------------------------------------*
           move      "C.Produttore"       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Numero ordine                   *
      *                  *---------------------------------------------*
           move      "Ordine"             to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Quantita' ordinata              *
      *                  *---------------------------------------------*
           move      "Ordinato"           to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Spunta                          *
      *                  *---------------------------------------------*
           move      "V"                  to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Giacenza                        *
      *                  *---------------------------------------------*
           move      "Giacenza"           to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Colli                           *
      *                  *---------------------------------------------*
           move      "Collo"              to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Ubicazione 1                    *
      *                  *---------------------------------------------*
           move      "Ubicazione"         to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Ubicazione 2                    *
      *                  *---------------------------------------------*
           move      "Riserva 1"          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Ubicazione 3                    *
      *                  *---------------------------------------------*
           move      "Riserva 2"          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Ubicazione 4                    *
      *                  *---------------------------------------------*
           move      "Riserva 3"          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "C"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Literal per Immagine                        *
      *                  *---------------------------------------------*
       stp-cor-doc-200.
      *                  *---------------------------------------------*
      *                  * Literal per Barcode                         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       stp-cor-doc-800.
      *              *-------------------------------------------------*
      *              * Esecuzione sort per ordinamento                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   stp-cor-doc-inp-000
                                          thru stp-cor-doc-inp-999
                     output procedure     is   stp-cor-doc-out-000
                                          thru stp-cor-doc-out-999    .
       stp-cor-doc-900.
      *              *-------------------------------------------------*
      *              * Fine Stampa                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura tabella                            *
      *                  *---------------------------------------------*
           display   "</table>"                                       .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-999.
       stp-cor-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di input                                       *
      *    *-----------------------------------------------------------*
       stp-cor-doc-inp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-rig          .
           move      zero                 to   w-exe-sel-pro          .
           move      zero                 to   w-exe-snx-pcr          .
      *              *-------------------------------------------------*
      *              * Ricerca del codice numerico prodotto            *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to stp-cor-doc-inp-100.
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
           move      w-det-num-pro-num    to   w-exe-num-pro          .
           move      w-det-num-pro-alf    to   w-exe-alf-pro          .
       stp-cor-doc-inp-100.
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
                     go to  stp-cor-doc-inp-900.
       stp-cor-doc-inp-200.
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
                     go to  stp-cor-doc-inp-900.
       stp-cor-doc-inp-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-slc-num-bft-prt
                     go to  stp-cor-doc-inp-900.
       stp-cor-doc-inp-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     go to  stp-cor-doc-inp-200.
       stp-cor-doc-inp-500.
      *              *-------------------------------------------------*
      *              * Letture complementari                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione ubicazione                   *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-prm-ubi-tip-ope      .
           move      01                   to   d-prm-ubi-cod-dpz      .
           move      01                   to   d-prm-ubi-tip-mag      .
           move      rf-bfr-num-mag       to   d-prm-ubi-num-mag      .
           move      rf-bfr-sgl-vrn       to   d-prm-ubi-var-mag      .
           perform   det-prm-ubi-cll-000  thru det-prm-ubi-cll-999    .
       stp-cor-doc-inp-600.
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record di sort              *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Composizione chiave                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiustamento indice di percorso        *
      *                      *-----------------------------------------*
           if        d-prm-ubi-ubi-inx    =    zero
                     move  9999999        to   d-prm-ubi-ubi-inx      .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo         *
      *                      * ordinamento                             *
      *                      * ___                                     *
      *                      *-----------------------------------------*
           move      zero                 to   srt-inx-key            .
           move      spaces               to   srt-alf-key            .
      *                      *-----------------------------------------*
      *                      * Progressivo riga                        *
      *                      *-----------------------------------------*
           move      rf-bfr-num-prg       to   srt-rig-key            .
      *                  *---------------------------------------------*
      *                  * Composizione dati                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico prodotto                *
      *                      *-----------------------------------------*
           move      rf-bfr-num-mag       to   srt-num-pro            .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico prodotto            *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   srt-alf-pro            .
      *                      *-----------------------------------------*
      *                      * Codice prodotto selezionato             *
      *                      *-----------------------------------------*
           if        rf-bfr-num-mag       =    w-exe-num-pro
                     move  "S"            to   srt-sel-pro
                     add   1              to   w-exe-sel-pro
           else      move  "N"            to   srt-sel-pro            .
      *                      *-----------------------------------------*
      *                      * Parametri di ubicazione                 *
      *                      *-----------------------------------------*
           move      d-prm-ubi-ubi-lit    to   srt-prm-ubi            .
      *                      *-----------------------------------------*
      *                      * Un intero record [bfr]                  *
      *                      *-----------------------------------------*
           move      rf-bfr               to   srt-rec-bfr            .
       stp-cor-doc-inp-700.
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-cor-doc-inp-800.
      *              *-------------------------------------------------*
      *              * Riciclo a record successivo                     *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-inp-200.
       stp-cor-doc-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-inp-999.
       stp-cor-doc-inp-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di output                                      *
      *    *-----------------------------------------------------------*
       stp-cor-doc-out-000.
      *              *-------------------------------------------------*
      *              * Test su numero prodotti selezionati             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to stp-cor-doc-out-200.
           if        w-exe-sel-pro        not  = zero
                     go to stp-cor-doc-out-200.
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           display   "<div id='msg' class='sel'>"                     .
           display   "<h1>CODICE PRODOTTO '"                          .
           display   w-exe-alf-pro                                    .
           display   "' NON TROVATO !"                                .
           display   "</h1>"                                          .
           display   "</div>"                                         .
       stp-cor-doc-out-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se At End : uscita                          *
      *                  *---------------------------------------------*
           return    srt    at end
                            go to stp-cor-doc-out-900                 .
       stp-cor-doc-out-300.
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
      *              * Ripresa record [bfr]                            *
      *              *-------------------------------------------------*
           move      srt-rec-bfr          to   rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-ctr-rig          .
       stp-cor-doc-out-400.
      *              *-------------------------------------------------*
      *              * Test preliminare se per la riga in corso di     *
      *              * trattamento esistono dei colli                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-bfk-000  thru stp-cor-doc-bfk-999    .
           if        w-exe-ctr-bfk        =    zero
                     go to stp-cor-doc-out-700.
       stp-cor-doc-out-600.
      *                  *---------------------------------------------*
      *                  * Se colli                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di scansione colli                *
      *                      *-----------------------------------------*
           perform   stp-cor-doc-col-000  thru stp-cor-doc-col-999    .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     stp-cor-doc-out-800.
       stp-cor-doc-out-700.
      *                  *---------------------------------------------*
      *                  * Se senza colli                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione elementi riga              *
      *                      *-----------------------------------------*
           move      rf-bfr-num-prt       to   w-exe-rig-prt          .
           move      rf-bfr-num-prg       to   w-exe-rig-prg          .
           move      zero                 to   w-exe-rig-col          .
           move      spaces               to   w-exe-rig-sco          .
           move      rf-bfr-qta-acq       to   w-exe-rig-qta          .
      *                      *-----------------------------------------*
      *                      * Emissione della riga                    *
      *                      *-----------------------------------------*
           perform   stp-cor-doc-rig-000  thru stp-cor-doc-rig-999    .
      *                      *-----------------------------------------*
      *                      * A riciclo                               *
      *                      *-----------------------------------------*
           go to     stp-cor-doc-out-800.
       stp-cor-doc-out-800.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-out-200.
       stp-cor-doc-out-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-out-999.
       stp-cor-doc-out-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di test preliminare esistenza colli            *
      *    *-----------------------------------------------------------*
       stp-cor-doc-bfk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-bfk          .
       stp-cor-doc-bfk-100.
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
                     go to  stp-cor-doc-bfk-900.
       stp-cor-doc-bfk-200.
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
                     go to  stp-cor-doc-bfk-900.
       stp-cor-doc-bfk-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
           if        rf-bfk-num-prt       not  = w-slc-num-bft-prt
                     go to  stp-cor-doc-bfk-900.
           if        rf-bfk-num-prg       not  = rf-bfr-num-prg
                     go to  stp-cor-doc-bfk-900.
       stp-cor-doc-bfk-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
       stp-cor-doc-bfk-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-exe-ctr-bfk          .
       stp-cor-doc-bfk-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-bfk-999.
       stp-cor-doc-bfk-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine per ciclo di scansione colli                   *
      *    *-----------------------------------------------------------*
       stp-cor-doc-col-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       stp-cor-doc-col-100.
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
                     go to  stp-cor-doc-col-900.
       stp-cor-doc-col-200.
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
                     go to  stp-cor-doc-col-900.
       stp-cor-doc-col-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : fine lettura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su protocollo                          *
      *                  *---------------------------------------------*
           if        rf-bfk-num-prt       not  = w-slc-num-bft-prt
                     go to  stp-cor-doc-col-900.
      *                  *---------------------------------------------*
      *                  * Test su progressivo                         *
      *                  *---------------------------------------------*
           if        rf-bfk-num-prg       not  = rf-bfr-num-prg
                     go to  stp-cor-doc-col-900.
       stp-cor-doc-col-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bfk]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contrassegno collo                  *
      *                  *---------------------------------------------*
           if        w-exe-cts-bfo        =    spaces
                     go to  stp-cor-doc-col-500.
      *                  *---------------------------------------------*
      *                  * Il confronto avviene sulla componente       *
      *                  * numerica del contrassegno                   *
      *                  *---------------------------------------------*
           move      rf-bfk-cts-prc       to   w-det-cmp-num-alf      .
           perform   det-cmp-num-000      thru det-cmp-num-999        .
           if        w-det-cmp-num-num    not  = w-exe-cts-num
                     go to  stp-cor-doc-col-200.
       stp-cor-doc-col-500.
      *              *-------------------------------------------------*
      *              * Letture complementari                           *
      *              *-------------------------------------------------*
       stp-cor-doc-col-600.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
           move      rf-bfk-num-prt       to   w-exe-rig-prt          .
           move      rf-bfk-num-prg       to   w-exe-rig-prg          .
           move      rf-bfk-num-prc       to   w-exe-rig-col          .
           move      rf-bfk-cts-prc       to   w-exe-rig-sco          .
           move      rf-bfk-qta-prc       to   w-exe-rig-qta          .
           perform   stp-cor-doc-rig-000  thru stp-cor-doc-rig-999    .
       stp-cor-doc-col-800.
      *              *-------------------------------------------------*
      *              * Riciclo a record successivo                     *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-col-200.
       stp-cor-doc-col-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-col-999.
       stp-cor-doc-col-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di emissione della riga                        *
      *    *-----------------------------------------------------------*
       stp-cor-doc-rig-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-bfs-000  thru stp-cor-doc-bfs-999    .
       stp-cor-doc-rig-050.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione ID riga                      *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-rwi-000  thru stp-cor-doc-rwi-999    .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<tr id='rw1_"       to   w-all-str-cat (1)      .
           move      w-exe-rwi-idx        to   w-all-str-cat (2)      .
           move      "' "                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
      *                  *---------------------------------------------*
      *                  * Eventuale evidenziazione                    *
      *                  *---------------------------------------------*
           if        srt-sel-pro          =    "S"
                     display  " class='sel' "                         .
           display   "/>"                                             .
      *              *-------------------------------------------------*
      *              * Apertura Form                                   *
      *              *-------------------------------------------------*
           display   "<form "                                         .
______*    display   "action='./elebfo02' "                           .
           display   "action='' "                                     .
           display   "method='post'>"                                 .
      *              *-------------------------------------------------*
      *              * Campi 'hidden' per la riga                      *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-hid-000  thru stp-cor-doc-hid-999    .
       stp-cor-doc-rig-100.
      *              *-------------------------------------------------*
      *              * Progressivo riga                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ctr-rig        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-120.
      *              *-------------------------------------------------*
      *              * Progressivo collo                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-rig-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-150.
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           move      rf-bfr-alf-mag       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-170.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica prodotto                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione prodotto                    *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Lettura prodotto                            *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      srt-num-pro          to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       stp-cor-doc-rig-200.
      *              *-------------------------------------------------*
      *              * Descrizione prodotto                            *
      *              *-------------------------------------------------*
           move      rf-dcp-des-pro       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "g"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-250.
      *              *-------------------------------------------------*
      *              * Codice prodotto per il Produttore               *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-cop-000  thru stp-cor-doc-cop-999    .
       stp-cor-doc-rig-300.
      *              *-------------------------------------------------*
      *              * Numero ordine                                   *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-ord-000  thru stp-cor-doc-ord-999    .
       stp-cor-doc-rig-400.
      *              *-------------------------------------------------*
      *              * Quantita' in ordine                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      w-exe-rig-qta        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "R"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-450.
      *              *-------------------------------------------------*
      *              * Spunta                                          *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-spn-000  thru stp-cor-doc-spn-999    .
       stp-cor-doc-rig-500.
      *              *-------------------------------------------------*
      *              * Giacenza                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Preparazione link-area                      *
      *                  *---------------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      s-dat                to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      01                   to   d-sld-mag-cod-dpz      .
           move      01                   to   d-sld-mag-tip-mag      .
           move      rf-bfr-num-mag       to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
      *                  *---------------------------------------------*
      *                  * Richiamo del sottoprogramma                 *
      *                  *---------------------------------------------*
           perform   det-sld-mag-cll-000  thru det-sld-mag-cll-999    .
      *                  *---------------------------------------------*
      *                  * Correzione con quantita' ricevuta           *
      *                  *---------------------------------------------*
           subtract  w-exe-rig-qta        from d-sld-mag-sld-mag      .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<GD"                to   p-edm                  .
           move      d-sld-mag-sld-mag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "R"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
      *
           if        d-sld-mag-sld-mag    <    zero
                     move  "r"            to   w-exe-ttd-stl          .
      *
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-550.
      *              *-------------------------------------------------*
      *              * Collo                                           *
      *              *-------------------------------------------------*
           move      w-exe-rig-sco        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-600.
      *              *-------------------------------------------------*
      *              * Ubicazione - 4 parametri                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione ubicazione                   *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-prm-ubi-tip-ope      .
           move      01                   to   d-prm-ubi-cod-dpz      .
           move      01                   to   d-prm-ubi-tip-mag      .
           move      rf-dcp-num-pro       to   d-prm-ubi-num-mag      .
           move      spaces               to   d-prm-ubi-var-mag      .
           perform   det-prm-ubi-cll-000  thru det-prm-ubi-cll-999    .
      *                  *---------------------------------------------*
      *                  * Scorporo parametri                          *
      *                  *---------------------------------------------*
           move      d-prm-ubi-ubi-lit    to   w-all-str-alf          .
           move      " "                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        .
      *
           move      w-all-str-cat (1)    to   w-exe-ttd-ub1          .
           move      w-all-str-cat (2)    to   w-exe-ttd-ub2          .
           move      w-all-str-cat (3)    to   w-exe-ttd-ub3          .
           move      w-all-str-cat (4)    to   w-exe-ttd-ub4          .
      *                  *---------------------------------------------*
      *                  * Principale                                  *
      *                  *---------------------------------------------*
           move      w-exe-ttd-ub1        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Alternativa                                 *
      *                  *---------------------------------------------*
           move      w-exe-ttd-ub2        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Alternativa                                 *
      *                  *---------------------------------------------*
           move      w-exe-ttd-ub3        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Alternativa                                 *
      *                  *---------------------------------------------*
           move      w-exe-ttd-ub4        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-rig-800.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           display   "</tr>"                                          .
       stp-cor-doc-rig-750.
      *              *-------------------------------------------------*
      *              * Annotazioni, quantita' e ubicazione             *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-aqu-000  thru stp-cor-doc-aqu-999    .
       stp-cor-doc-rig-790.
      *              *-------------------------------------------------*
      *              * Chiusura Form                                   *
      *              *-------------------------------------------------*
           display   "</form>"                                        .
       stp-cor-doc-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-rig-999.
       stp-cor-doc-rig-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di determinazione ID della riga                *
      *    *-----------------------------------------------------------*
       stp-cor-doc-rwi-000.
      *              *-------------------------------------------------*
      *              * Editing del progressivo interno riga            *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-ctr-rig        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (1)      .
      *              *-------------------------------------------------*
      *              * Editing progressivo collo                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-rig-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (3)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "_"                  to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-rwi-idx          .
       stp-cor-doc-rwi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-rwi-999.
       stp-cor-doc-rwi-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di lettura [bfs]                               *
      *    *-----------------------------------------------------------*
       stp-cor-doc-bfs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-flg-spn          .
           move      spaces               to   w-exe-sgl-odm          .
           move      spaces               to   w-exe-cod-ub1          .
           move      spaces               to   w-exe-ann-rig          .
       stp-cor-doc-bfs-100.
      *              *-------------------------------------------------*
      *              * Lettura record [bfs]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-exe-rig-prt        to   rf-bfs-num-prt         .
           move      w-exe-rig-prg        to   rf-bfs-num-prg         .
           move      w-exe-rig-col        to   rf-bfs-num-prr         .
           move      "pgm/bfo/fls/ioc/obj/iofbfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfs                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato: ad uscita            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-bfs-900.
       stp-cor-doc-bfs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori letti                    *
      *              *-------------------------------------------------*
           move      rf-bfs-flg-spn       to   w-exe-flg-spn          .
______*    move      rf-bfs-sgl-odm       to   w-exe-sgl-odm          .
           move      rf-bfs-cod-ub1       to   w-exe-cod-ub1          .
           move      rf-bfs-ann-spn       to   w-exe-ann-rig          .
       stp-cor-doc-bfs-400.
      *              *-------------------------------------------------*
      *              * Eventuale segnale di operatore diverso          *
      *              * ___                                             *
      *              *-------------------------------------------------*
       stp-cor-doc-bfs-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-bfs-999.
       stp-cor-doc-bfs-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di trattamento numero ordine                   *
      *    *-----------------------------------------------------------*
       stp-cor-doc-ord-000.
      *              *-------------------------------------------------*
      *              * Test se riferimenti esistenti                   *
      *              *-------------------------------------------------*
           if        rf-bfr-orf-num       =    zero and
                     rf-bfr-orf-dat       =    zero
                     move  spaces         to   w-all-str-alf
                     go to stp-cor-doc-ord-480.
      *              *-------------------------------------------------*
      *              * Editing numero ordine                           *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-bfr-orf-num       to   w-stp-int-ndo          .
           move      w-stp-int-ndo-saa    to   w-stp-int-npr-saa      .
           move      w-stp-int-ndo-prg    to   w-stp-int-npr-prg      .
           move      w-stp-int-npr        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-nde          .
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           move      08                   to   w-all-str-lun          .
           move      w-stp-int-nde        to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
       stp-cor-doc-ord-480.
      *              *-------------------------------------------------*
      *              * Valore                                          *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "R"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-ord-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-ord-999.
       stp-cor-doc-ord-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine per i campi 'hidden'                           *
      *    *-----------------------------------------------------------*
       stp-cor-doc-hid-000.
      *              *-------------------------------------------------*
      *              * Progressivo riga [bfr] 'hidden'                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      rf-bfr-num-prg       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione                              *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-prg          .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "prg"                to   w-exe-hid-nam          .
           move      w-exe-ttd-prg        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       stp-cor-doc-hid-200.
      *              *-------------------------------------------------*
      *              * Progressivo riga interno 'hidden'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-ctr-rig        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione                              *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-prr          .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "prr"                to   w-exe-hid-nam          .
           move      w-exe-ttd-prr        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       stp-cor-doc-hid-200.
      *              *-------------------------------------------------*
      *              * Progressivo collo 'hidden'                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-rig-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione                              *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-nco          .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "nco"                to   w-exe-hid-nam          .
           move      w-exe-ttd-nco        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       stp-cor-doc-hid-400.
      *              *-------------------------------------------------*
      *              * Si/no riga spuntata 'hidden'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "pcr"                to   w-exe-hid-nam          .
           move      srt-snx-pcr          to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       stp-cor-doc-hid-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-hid-999.
       stp-cor-doc-hid-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di accettazione spunta                         *
      *    *-----------------------------------------------------------*
       stp-cor-doc-spn-000.
      *              *-------------------------------------------------*
      *              * Editing del progressivo interno riga            *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-ctr-rig        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (1)      .
      *              *-------------------------------------------------*
      *              * Editing progressivo collo                       *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-exe-rig-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (3)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "_"                  to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-ttd-cki          .
       stp-cor-doc-spn-200.
      *              *-------------------------------------------------*
      *              * Spunta                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td width='10%' align='center'>"                .
      *                  *---------------------------------------------*
      *                  * Stile                                       *
      *                  *---------------------------------------------*
           display   "<label class='container'>"                      .
      *                  *---------------------------------------------*
      *                  * Test se attiva o no                         *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "checked"      to   w-exe-str-chd
           else      move  spaces         to   w-exe-str-chd          .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='checkbox' class='spunta' id='"
                                delimited by size
                     w-exe-ttd-cki
                                delimited by spaces
                     "' "       delimited by size
                     w-exe-str-chd
                                delimited by spaces
                     ">"        delimited by size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * Stile                                       *
      *                  *---------------------------------------------*
           display   "<span class='checkmark'></span>"                .
           display   "</label>"                                       .
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-cor-doc-spn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-spn-999.
       stp-cor-doc-spn-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di accettazione annotazioni, quantita' e ubi-  *
      *    * cazione                                                   *
      *    *-----------------------------------------------------------*
       stp-cor-doc-aqu-000.
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione ID riga                      *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-rwi-000  thru stp-cor-doc-rwi-999    .
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "<tr id='rw2_"       to   w-all-str-cat (1)      .
           move      w-exe-rwi-idx        to   w-all-str-cat (2)      .
           move      "' "                 to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           display   w-all-str-alf                                    .
      *                  *---------------------------------------------*
      *                  * Inibizione iniziale                         *
      *                  * ___ DA VERIFICA CAMPI MODIFICA              *
      *                  *---------------------------------------------*
           display   " style='display:none;' "                        .
      *                  *---------------------------------------------*
      *                  * Chiusura Tag                                *
      *                  *---------------------------------------------*
           display   "/>"                                             .
       stp-cor-doc-aqu-200.
      *              *-------------------------------------------------*
      *              * Annotazioni                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td colspan='6'>"                               .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           display   "<input type='text' "                            .
           display   "id='ann' name='ann' size='70' "                            .
      *
           move      spaces               to   w-exe-str-dsp          .
           string    "value='"  delimited by size
                     w-exe-ann-rig
                                delimited by size
                     "' "       delimited by size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * Suggerimento                                *
      *                  *---------------------------------------------*
           display   "placeholder='annotazioni' "                     .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to stp-cor-doc-aqu-250.
           display   "disabled "                                      .
       stp-cor-doc-aqu-250.
           display   "/>"                                             .
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-cor-doc-aqu-300.
      *              *-------------------------------------------------*
      *              * Quantita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td colspan='1'>"                               .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      w-exe-rig-qta        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<input type='text' "                            .
           display   "id='qta' name ='qta' "                          .
           display   "class='numbersOnly' "                           .
           display   "style='text-align:right;' "                     .
      *
           move      spaces               to   w-exe-str-dsp          .
           string    "value='"  delimited by size
                     p-edt
                                delimited by spaces
                     "'"        delimited by size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to stp-cor-doc-aqu-350.
           display   "disabled "                                      .
       stp-cor-doc-aqu-350.
      *                  *---------------------------------------------*
      *                  * Chiusura input                              *
      *                  *---------------------------------------------*
           display   "/>"                                             .
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-cor-doc-aqu-400.
      *              *-------------------------------------------------*
      *              * Celle vuote                                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-ttd-txt          .
           move      03                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-aqu-600.
      *              *-------------------------------------------------*
      *              * Ubicazione                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura cella                              *
      *                  *---------------------------------------------*
           display   "<td colspan='1'>"                               .
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           display   "<input type='text' "                            .
           display   "id='ubi' name='ubi' "                           .
           move      spaces               to   w-exe-str-dsp          .
           string    "value='"  delimited by size
                     w-exe-cod-ub1
                                delimited by spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='07' "
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * Suggerimento                                *
      *                  *---------------------------------------------*
           display   "placeholder='ubicazione' "                      .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to stp-cor-doc-aqu-650.
           display   "disabled "                                      .
       stp-cor-doc-aqu-650.
           display   "/>"                                             .
      *                  *---------------------------------------------*
      *                  * Chiusura cella                              *
      *                  *---------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       stp-cor-doc-aqu-700.
      *              *-------------------------------------------------*
      *              * Celle vuote                                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-ttd-txt          .
           move      03                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-aqu-800.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           display   "</tr>"                                          .
       stp-cor-doc-aqu-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-aqu-999.
       stp-cor-doc-aqu-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di output                                      *
      *    *                                                           *
      *    * Codice del fornitore                                      *
      *    *-----------------------------------------------------------*
       stp-cor-doc-cop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione [aaq]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * Lettura codice per il Produttore                *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * In campo di destinazione                        *
      *              *-------------------------------------------------*
           move      rf-aaq-cdp-pdt       to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "R"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
       stp-cor-doc-cop-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-cop-999.
       stp-cor-doc-cop-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa piede documento                         *
      *    *-----------------------------------------------------------*
       stp-pie-doc-000.
       stp-pie-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-999.
       stp-pie-doc-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *-----------------------------------------------------------*
       emi-htm-int-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   emi-htm-int-hea-000  thru emi-htm-int-hea-999    .
       emi-htm-int-200.
      *              *-------------------------------------------------*
      *              * Modulo                                          *
      *              *-------------------------------------------------*
           display   "<form name='bfo_001' id='bfo_001' method='post' ac
      -              "tion='/cgi-bin/elebfo21'>"                     .
       emi-htm-int-300.
      *              *-------------------------------------------------*
      *              * Allineamento                                    *
      *              *-------------------------------------------------*
           display   "<center>"                                       .
      *              *-------------------------------------------------*
      *              * Area di input testata ordine                    *
      *              *-------------------------------------------------*
           display   "<table border=2 cellspacing=2 cellpadding=2>"   .
      *              *-------------------------------------------------*
      *              * Apertura riga                                   *
      *              *-------------------------------------------------*
           display   "<tr>"                                           .
      *              *-------------------------------------------------*
      *              * Responsabile                                    *
      *              *-------------------------------------------------*
           perform   emi-htm-int-rsp-000  thru emi-htm-int-rsp-999    .
      *              *-------------------------------------------------*
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
           perform   emi-htm-int-prt-000  thru emi-htm-int-prt-999    .
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
           perform   emi-htm-int-pro-000  thru emi-htm-int-pro-999    .
      *              *-------------------------------------------------*
      *              * Tasto di cancellazione                          *
      *              *-------------------------------------------------*
           display   "<td id='cel_del'>"                              .
           display   "<button type='button' id='del' style='background-c
      -              "olor: #FFFFFF;'>"                               .
           display   "<img src='../icons/trash.png' width='70'>"      .
           display   "</button>"                                      .
           display   "</td>"                                          .
      *              *-------------------------------------------------*
      *              * Tasto di stampa                                 *
      *              *-------------------------------------------------*
           display   "<td id='cel_stp'>"                              .
           display   "<button type='button' id='prn' style='background-c
      -              "olor: #4CAF50;'>"                                .
           display   "<img src='../icons/printer.png' width='70'>"    .
           display   "</button>"                                      .
           display   "</td>"                                          .
       emi-htm-int-400.
      *              *-------------------------------------------------*
      *              * Chiusura riga                                   *
      *              *-------------------------------------------------*
           display   "</tr>"                                          .
      *              *-------------------------------------------------*
      *              * Chiusura tabella                                *
      *              *-------------------------------------------------*
           display   "</table>"                                       .
       emi-htm-int-500.
      *              *-------------------------------------------------*
      *              * Area di input prodotto e quantita'              *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Inizio corpo                                    *
      *              *-------------------------------------------------*
           display   "</center>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura Form 'ods'                             *
      *              *-------------------------------------------------*
           display   "</form>"                                        .
           display   "<br>"                                           .
           display   "<hr>"                                           .
      *              *-------------------------------------------------*
      *              * Messaggio eventuale                             *
      *              *-------------------------------------------------*
______*    display   "<div id='msg'>"                                 .
______*    display   "</div>"                                         .
      *              *-------------------------------------------------*
      *              * Box di conferma                                 *
      *              *-------------------------------------------------*
           display   "<div id='dialog-confirm' title='conferma'>"     .
           display   "<p><span class='ui-icon ui-icon-alert' "        .
           display   "style='float:left; margin:12px 12px 20px 0;'>"  .

      * ______ EVENTUALE NUMERO PROTOCOLLO _____

           display   "</span><b>Si conferma la cancellazione?</b></p>".
           display   "</div>"                                         .
       emi-htm-int-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *                                                           *
      *    * Subroutine per Responsabile                               *
      *    *-----------------------------------------------------------*
       emi-htm-int-rsp-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ute' name= 'cel_ute' align=center>" .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Operatore </h1>"                           .
           display   "<select name='rsp_doc' id='rsp_doc'>"           .
      *
           move      spaces               to   w-exe-str-dsp          .
           string    "<option selected value='"
                                delimited by   size
                     w-exe-cod-ute
                                delimited by   spaces
                     "'>"
                                delimited by   size
                     w-exe-des-ute
                                delimited by   spaces
                     "</option>"
                                delimited by   size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
       emi-htm-int-rsp-200.
      *              *-------------------------------------------------*
      *              * Test se opzione da esporre                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'CRO' Crozzoletto                        *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    "CRO"
                     go to emi-htm-int-rsp-220.
           display   "<option value='CRO'>Crozzoletto</option>"       .
       emi-htm-int-rsp-220.
      *                  *---------------------------------------------*
      *                  * Se 'LUC'                                    *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    "LUC"
                     go to emi-htm-int-rsp-230.
           display   "<option value='LUC'>Lucio</option>"             .
       emi-htm-int-rsp-230.
      *                  *---------------------------------------------*
      *                  * Se 'PIN'                                    *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    "PIN"
                     go to emi-htm-int-rsp-240.
           display   "<option value='PIN'>Pin</option>"               .
       emi-htm-int-rsp-240.
      *                  *---------------------------------------------*
      *                  * Se 'RIC'                                    *
      *                  *---------------------------------------------*
           if        w-exe-cod-ute        =    "RIC"
                     go to emi-htm-int-rsp-300.
           display   "<option value='RIC'>Riccardo</option>"          .
       emi-htm-int-rsp-300.
      *              *-------------------------------------------------*
      *              * Chiusura accettazione                           *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-htm-int-rsp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-int-rsp-999.
       emi-htm-int-rsp-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *                                                           *
      *    * Subroutine per Protocollo documento                       *
      *    *-----------------------------------------------------------*
       emi-htm-int-prt-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_prt' align=center>"                 .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Protocollo </h1>"                          .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           move      spaces               to   w-exe-str-dsp          .
           string    "<input type='number' id='num_prt' name='num_prt' "
                                delimited by   size
                     "value='"
                                delimited by   size
                     w-exe-prt-bfo
                                delimited by   spaces
                     "'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-htm-int-prt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-int-prt-999.
       emi-htm-int-prt-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *                                                           *
      *    * Subroutine per Codice prodotto                            *
      *    *-----------------------------------------------------------*
       emi-htm-int-pro-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_pro' align=center>"                 .
       emi-htm-int-pro-100.
      *              *-------------------------------------------------*
      *              * Ricerca del codice numerico prodotto            *
      *              *-------------------------------------------------*
           if        w-exe-alf-pro        =    spaces
                     go to emi-htm-int-pro-200.
           move      w-exe-alf-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
           move      w-det-num-pro-num    to   w-exe-num-pro          .
           move      w-det-num-pro-alf    to   w-exe-alf-pro          .
       emi-htm-int-pro-200.
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           display   "<h1> Prodotto </h1>"                            .
      *
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='alf_pro' name='alf_pro'"
                                delimited by   size
                     "value='"
                                delimited by   size
                     w-det-num-pro-alf
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='20'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       emi-htm-int-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-int-pro-999.
       emi-htm-int-pro-999.
           exit.

      *    *===========================================================*
      *    * Intestazione                                              *
      *    *                                                           *
      *    * Subroutine per 'header'                                   *
      *    *-----------------------------------------------------------*
       emi-htm-int-hea-000.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<head>"                                         .
           display   "<title>[***[ELETTRA]***] Entrata merce </title>".
      *              *-------------------------------------------------*
      *              * Jquery UI CSS                                   *
      *              *-------------------------------------------------*
           display   "<link rel='stylesheet' type='text/css' href='https
      -              "://ajax.googleapis.com/ajax/libs/jqueryui/1.13.2/t
      -              "hemes/smoothness/jquery-ui.css'>"               .
      *              *-------------------------------------------------*
      *              * Css                                             *
      *              *-------------------------------------------------*
           display   "<link rel='stylesheet' type='text/css' href='../cs
      -              "s/ele.css'>"                                    .
      *              *-------------------------------------------------*
      *              * Jquery                                          *
      *              *-------------------------------------------------*
           display   "<script src='https://ajax.googleapis.com/ajax/libs
      -              "/jquery/3.6.3/jquery.min.js'></script>"         .
      *              *-------------------------------------------------*
      *              * Jquery UI                                       *
      *              *-------------------------------------------------*
           display   "<script src='https://ajax.googleapis.com/ajax/libs
      -              "/jqueryui/1.13.2/jquery-ui.min.js'></script>"   .
      *              *-------------------------------------------------*
      *              * Javascript per il documento                     *
      *              *-------------------------------------------------*
           display   "<script src=""../jsc/elebfo21.js""></script>"   .
      *              *-------------------------------------------------*
      *              * Head - fine                                     *
      *              *-------------------------------------------------*
           display   "</head>"                                        .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           display   "<body>"                                         .
       emi-htm-int-hea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-int-hea-999.
       emi-htm-int-hea-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag generica                                    *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-exe-tag-tag = nome del tag                     *
      *    *          w-exe-tag-flg = 'O' o 'C' (Open or Close)        *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa completa                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-htm-tag-000.
      *              *-------------------------------------------------*
      *              * Se testo a spazi                                *
      *              *-------------------------------------------------*
           if        w-exe-tag-tag        =    spaces
                     go to emi-htm-tag-900.
       emi-htm-tag-100.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "<"                  to   w-all-str-cat (1)      .
      *
           if        w-exe-tag-flg        =    "O"
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
      *
           move      w-exe-tag-tag        to   w-all-str-cat (3)      .
           move      ">"                  to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
       emi-htm-tag-300.
      *              *-------------------------------------------------*
      *              * Output                                          *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-htm-tag-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-tag-999.
       emi-htm-tag-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag 'td' o 'th'                                 *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-exe-ttd-txt = Testo da inserire nel Tag        *
      *    *          w-exe-ttd-col = Numero di colonne occupate       *
      *    *          w-exe-ttd-snh = Si/no header (th o td)           *
      *    *          w-exe-ttd-all = Allineamento (L, C, R)           *
      *    *          w-exe-ttd-wdt = Larghezza (S, M, B)              *
      *    *          w-exe-ttd-stl = Stile (B, N, x)                  *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa compbfta                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-htm-ttd-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione contenuto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * TAG                                         *
      *                  *---------------------------------------------*
           if        w-exe-ttd-snh        =    spaces or
                     w-exe-ttd-snh        =    "N"
                     move  "<td"          to   w-exe-ttd-tg1
                     move  "</td>"        to   w-exe-ttd-tg2
           else      move  "<th"          to   w-exe-ttd-tg1
                     move  "</th>"        to   w-exe-ttd-tg2          .
      *                  *---------------------------------------------*
      *                  * Testo                                       *
      *                  *---------------------------------------------*
           if        w-exe-ttd-txt        =    spaces
                     move  "&nbsp;"       to   w-exe-ttd-txt          .
      *                  *---------------------------------------------*
      *                  * Larghezza                                   *
      *                  *---------------------------------------------*
           if        w-exe-ttd-wdt        =    "S"
                     move  "width='20%'" 
                                          to   w-exe-ttd-wdw
           else if   w-exe-ttd-all        =    "M"
                     move  "width='40%'" 
                                          to   w-exe-ttd-wdw
           else if   w-exe-ttd-all        =    "B"
                     move  "width='80%'" 
                                          to   w-exe-ttd-wdw
           else      move  spaces         to   w-exe-ttd-wdw          .
      *                      *-----------------------------------------*
      *                      * Attualmente forzata a spazi             *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-ttd-wdw          .
      *                  *---------------------------------------------*
      *                  * Allineamento                                *
      *                  *---------------------------------------------*
           if        w-exe-ttd-all        =    "L"
                     move  "align='left'" 
                                          to   w-exe-ttd-alw
           else if   w-exe-ttd-all        =    "R"
                     move  "align='right'"
                                          to   w-exe-ttd-alw
           else if   w-exe-ttd-all        =    "C"
                     move  "align='center'"
                                          to   w-exe-ttd-alw
           else      move  spaces         to   w-exe-ttd-alw          .
      *                  *---------------------------------------------*
      *                  * Colonne                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-exe-ttd-col        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      20                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "colspan='"          to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           move      "'"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-ttd-cow          .
      *                  *---------------------------------------------*
      *                  * Stile                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bold                                    *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        =    "B"
                     move  "<B>"          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2
           else      move  spaces         to   w-exe-ttd-st1
                     move  spaces         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Sfondo rosso                            *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        =    "r"
                     move  "<B style='color:red'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
      *                      *-----------------------------------------*
      *                      * Sfondo grigio                           *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        =    "g"
                     move  "<B style='color:grey'>"
                                          to   w-exe-ttd-st1
                     move  "</B>"         to   w-exe-ttd-st2          .
       emi-htm-ttd-800.
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      10                   to   w-all-str-num          .
           move      w-exe-ttd-tg1        to   w-all-str-cat (1)      .
           move      w-exe-ttd-cow        to   w-all-str-cat (2)      .
           move      w-exe-ttd-alw        to   w-all-str-cat (3)      .
           move      w-exe-ttd-wdw        to   w-all-str-cat (4)      .
           move      ">"                  to   w-all-str-cat (5)      .
           move      w-exe-ttd-st1        to   w-all-str-cat (6)      .
           move      w-exe-ttd-txt        to   w-all-str-cat (7)      .
           move      w-exe-ttd-txt
                    (81 : 40)             to   w-all-str-cat (8)      .
           move      w-exe-ttd-st2        to   w-all-str-cat (9)      .
           move      w-exe-ttd-tg2        to   w-all-str-cat (10)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG 'td' o 'th'                       *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-htm-ttd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-ttd-999.
       emi-htm-ttd-999.
           exit.

      *    *===========================================================*
      *    * Emissione Tag 'input' 'hidden'                            *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * Input  : w-exe-hid-nam = Nome del campo                   *
      *    *          w-exe-hid-val = Valore del campo                 *
      *    *                                                           *
      *    * Output : w-all-str-alf = Stringa compbfta                 *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       emi-htm-hid-000.
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "<input type='hidden' name='"
                                          to   w-all-str-cat (1)      .
           move      w-exe-hid-nam        to   w-all-str-cat (2)      .
           move      "' id='"             to   w-all-str-cat (3)      .
           move      w-exe-hid-nam        to   w-all-str-cat (4)      .
           move      "' value='"          to   w-all-str-cat (5)      .
           move      w-exe-hid-val        to   w-all-str-cat (6)      .
           move      "'>"                 to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Emissione TAG                                   *
      *              *-------------------------------------------------*
           display   w-all-str-alf                                    .
       emi-htm-hid-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-hid-999.
       emi-htm-hid-999.
           exit.

      *    *===========================================================*
      *    * Emissione chiusura HTML                                   *
      *    *-----------------------------------------------------------*
       emi-htm-cls-000.
      *              *-------------------------------------------------*
      *              * Linea di fondo                                  *
      *              *-------------------------------------------------*
           display   "<hr>"                                           .
           display   "<center>"                                       .
      *              *-------------------------------------------------*
      *              * Bottone di ritorno alla home                    *
      *              *-------------------------------------------------*
           display   "<div id='back'>"                                .
           display   "<a href='../ele.html' id='next-blue'>"          .
           display   "<img src='../icons/next-blue-1.png' width='50'>".
           display   "</a>"                                           .
           display   "</div>"                                         .
      *              *-------------------------------------------------*
      *              * Copyright                                       *
      *              *-------------------------------------------------*
           display   "<div id='copy'>"                                .
           display   "<address>"                                      .
           display   "<font size=3>&copy wip - TANGRAM"               .
           display   "</address>"                                     .
           display   "</div>"                                         .
           display   "</center>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura HTML                                   *
      *              *-------------------------------------------------*
           display   "</body>"                                        .
           display   "</html>"                                        .
       emi-htm-cls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     emi-htm-cls-999.
       emi-htm-cls-999.
           exit.

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
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
      *    * Determinazione del componente numerica contrassegno       *
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
      *    * Subroutines per determinazione ubicazione di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione saldo di magazzino         *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
