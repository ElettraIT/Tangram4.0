       Identification Division.
       Program-Id.                                 elebfoa0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cgi                 *
      *                                Settore:                        *
      *                                   Fase:    elebfo              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 22/03/23    *
      *                       Ultima revisione:    NdK del 03/05/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Accettazione campi riga                     *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      *                    ___ NON UTILIZZATO ___                      *
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
      *        * [zub]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfzub"                          .
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
           05  w-exe-str-chd              pic  x(15)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-rig              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Parametri in input estratti                           *
      *        *-------------------------------------------------------*
           05  w-exe-tip-ope              pic  x(03)                  .
           05  w-exe-rsp_doc              pic  x(03)                  .
           05  w-exe-prt-bfo              pic  x(11)                  .
           05  w-exe-prg-bfo              pic  x(05)                  .
           05  w-exe-pri-bfo              pic  x(03)                  .
           05  w-exe-col-bfo              pic  x(03)                  .
           05  w-exe-qta-bfo              pic  x(12)                  .
           05  w-exe-alf-pro              pic  x(20)                  .
           05  w-exe-acc-pro              pic  x(20)                  .
           05  w-exe-acc-pnu              pic  9(07)                  .
           05  w-exe-ubi-bfo              pic  x(07)                  .
           05  w-exe-cod-ncf              pic  x(03)                  .
           05  w-exe-cnv-ncf              pic  9(03)                  .
           05  w-exe-des-ncf              pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-fld  occurs 20   pic  x(90)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qta-edt              pic  9(11)                  .
           05  w-exe-snx-pcr              pic  9(03)                  .
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
               10  w-exe-hid-nam          pic  x(07)                  .
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
               10  w-exe-ttd-cki          pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per messaggi di output                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-msg              pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-exe-qta-cnv              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione riga tabella               *
      *        *-------------------------------------------------------*
           05  w-exe-rig.
               10  w-exe-flg-spn          pic  x(01)                  .
               10  w-exe-rig-col          pic  9(03)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella ubicazioni                 *
      *        *-------------------------------------------------------*
           05  w-det-tbl-ubi.
      *            *---------------------------------------------------*
      *            * Comodo per rottura su codice ubicazione           *
      *            *---------------------------------------------------*
               10  w-det-tbl-ubi-rot      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella pre-codici                                *
      *            *---------------------------------------------------*
               10  w-det-tbl-pre-ele  occurs  200.
                   15  w-det-tbl-pre-cod  pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella codici                                    *
      *            *---------------------------------------------------*
               10  w-det-tbl-cod-ele  occurs  2000.
                   15  w-det-tbl-cod-ubi  pic  x(07)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-ubi-c01      pic  9(04)                  .
               10  w-det-tbl-ubi-c02      pic  9(04)                  .
               10  w-det-tbl-ubi-inx      pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Per determinazione tabella non conformita'            *
      *        *-------------------------------------------------------*
           05  w-det-tbl-ncf.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-tbl-ncf-max      pic  9(02) value 4          .
               10  w-det-tbl-ncf-inx      pic  9(02)                  .
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
               10  w-slc-num-bft-pri      pic  9(03)                  .

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
      *        * Numero delle coppie estratte                          *
      *        *-------------------------------------------------------*
           05  w-cgi-str-num              pic  9(02)                  .
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
           05  w-cgi-str-max              pic  9(02) value 20         .

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
           move      spaces               to   w-exe-prm-fld (01)     .
           move      spaces               to   w-exe-prm-fld (02)     .
           move      spaces               to   w-exe-prm-fld (03)     .
           move      spaces               to   w-exe-prm-fld (04)     .
           move      spaces               to   w-exe-prm-fld (05)     .
           move      spaces               to   w-exe-prm-fld (06)     .
           move      spaces               to   w-exe-prm-fld (07)     .
           move      spaces               to   w-exe-prm-fld (08)     .
           move      spaces               to   w-exe-prm-fld (09)     .
           move      spaces               to   w-exe-prm-fld (10)     .
           move      spaces               to   w-exe-prm-fld (11)     .
           move      spaces               to   w-exe-prm-fld (12)     .
           move      spaces               to   w-exe-prm-fld (13)     .
           move      spaces               to   w-exe-prm-fld (14)     .
           move      spaces               to   w-exe-prm-fld (15)     .
           move      spaces               to   w-exe-prm-fld (16)     .
           move      spaces               to   w-exe-prm-fld (17)     .
           move      spaces               to   w-exe-prm-fld (18)     .
           move      spaces               to   w-exe-prm-fld (19)     .
           move      spaces               to   w-exe-prm-fld (20)     .
      *
           move      spaces               to   w-exe-tip-ope          .
           move      spaces               to   w-exe-rsp_doc          .
           move      spaces               to   w-exe-prt-bfo          .
           move      spaces               to   w-exe-prg-bfo          .
           move      spaces               to   w-exe-pri-bfo          .
           move      spaces               to   w-exe-col-bfo          .
           move      spaces               to   w-exe-qta-bfo          .
           move      spaces               to   w-exe-ubi-bfo          .
           move      spaces               to   w-exe-alf-pro          .
           move      spaces               to   w-exe-acc-pro          .
           move      zero                 to   w-exe-acc-pnu          .
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
      *
           move      w-cgi-str-fld (01)   to   w-exe-prm-fld (01)     .
           move      w-cgi-str-fld (02)   to   w-exe-prm-fld (02)     .
           move      w-cgi-str-fld (03)   to   w-exe-prm-fld (03)     .
           move      w-cgi-str-fld (04)   to   w-exe-prm-fld (04)     .
           move      w-cgi-str-fld (05)   to   w-exe-prm-fld (05)     .
           move      w-cgi-str-fld (06)   to   w-exe-prm-fld (06)     .
           move      w-cgi-str-fld (07)   to   w-exe-prm-fld (07)     .
           move      w-cgi-str-fld (08)   to   w-exe-prm-fld (08)     .
           move      w-cgi-str-fld (09)   to   w-exe-prm-fld (09)     .
           move      w-cgi-str-fld (10)   to   w-exe-prm-fld (10)     .
           move      w-cgi-str-fld (11)   to   w-exe-prm-fld (11)     .
           move      w-cgi-str-fld (12)   to   w-exe-prm-fld (12)     .
           move      w-cgi-str-fld (13)   to   w-exe-prm-fld (13)     .
           move      w-cgi-str-fld (14)   to   w-exe-prm-fld (14)     .
           move      w-cgi-str-fld (15)   to   w-exe-prm-fld (15)     .
           move      w-cgi-str-fld (16)   to   w-exe-prm-fld (16)     .
           move      w-cgi-str-fld (17)   to   w-exe-prm-fld (17)     .
           move      w-cgi-str-fld (18)   to   w-exe-prm-fld (18)     .
           move      w-cgi-str-fld (19)   to   w-exe-prm-fld (19)     .
           move      w-cgi-str-fld (20)   to   w-exe-prm-fld (20)     .
       ext-prm-300.
      *              *-------------------------------------------------*
      *              * Assegnazione componenti                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (01)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-tip-ope          .
      *                  *---------------------------------------------*
      *                  * Subroutine di estrazione parametri in       *
      *                  * funzione del tipo operazione                *
      *                  *---------------------------------------------*
           if        w-exe-tip-ope        =    "AC"
                     perform ext-prm-acc-000
                                          thru ext-prm-acc-999        .


           go to ext-prm-900.
      *              *-------------------------------------------------*
      *              * DEBUG                                           *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   o-pst                                            .
           display   "<br>"                                           .
           display   "<br>"                                           .
           display   w-exe-rsp-doc                                    .
           display   "<br>"                                           .
           display   w-exe-prt-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-prg-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-pri-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-col-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-qta-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-ubi-bfo                                    .
           display   "<br>"                                           .
           display   w-exe-alf-pro                                    .
           display   "<br>"                                           .
           display   w-exe-cod-ncf                                    .
           display   "<br>"                                           .
           display   w-exe-flg-spn                                    .
           display   "<br>"                                           .







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
      *    * Subroutine di estrazione parametri per accettazione riga  *
      *    *                                                           *
      *    * Parametri previsti                                        *
      *    *                                                           *
      *    *  - rsp_doc : responsabile di magazzino                    *
      *    *  - prt_bfo : protocollo documento                         *
      *    *  - prg_bfo : progressivo riga interno documento           *
      *    *  - prr_bfo : progressivo riga documento                   *
      *    *  - nco_bfo : numero collo                                 *
      *    *  - qta_bfo : quantita'                                    *
      *    *  - ubi_rig : ubicazione                                   *
      *    *  - alf_pro : codice alfanumerico prodotto                 *
      *    *  - cod_ncf : codice di non conformita'                    *
      *    *  - flg_spn : flag di spunta                               *
      *    *-----------------------------------------------------------*
       ext-prm-acc-000.
      *              *-------------------------------------------------*
      *              * Estrazione parametri                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Operatore                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (02)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-rsp-doc          .
      *                  *---------------------------------------------*
      *                  * Protocollo documento                        *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (03)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prt-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo interno riga documento          *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (04)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-prg-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga di comodo                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (05)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-pri-bfo          .
      *                  *---------------------------------------------*
      *                  * Progressivo collo                           *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (06)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-col-bfo          .
      *                  *---------------------------------------------*
      *                  * Quantita'                                   *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (07)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-qta-bfo          .
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (08)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-ubi-bfo          .
      *                  *---------------------------------------------*
      *                  * Prodotto                                    *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (09)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-alf-pro          .
      *                  *---------------------------------------------*
      *                  * Non conformita'                             *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (10)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-cod-ncf          .
      *                  *---------------------------------------------*
      *                  * Spunta                                      *
      *                  *---------------------------------------------*
           move      w-exe-prm-fld (11)   to   w-all-str-alf          .
           move      "="                  to   w-all-str-del          .
           perform   all-str-ext-000      thru all-str-ext-999        . 
           move      w-all-str-cat (2)    to   w-exe-flg-spn          .
       ext-prm-acc-500.
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
      *                  * Contatore righe                             *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      03                   to   p-car                  .
           move      w-exe-pri-bfo        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-slc-num-bft-pri      .
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
      *                  * Ubicazione in uppercase                     *
      *                  *---------------------------------------------*
           move      w-exe-ubi-bfo        to   w-all-str-alf          .
           move      07                   to   w-all-str-lun          .
           perform   all-str-upp-000      thru all-str-upp-999        .
           move      w-all-str-alf        to   w-exe-ubi-bfo          .
      *                  *---------------------------------------------*
      *                  * Non conformita'                             *
      *                  *---------------------------------------------*
           move      "CV"                 to   p-ope                  .
           move      03                   to   p-car                  .
           move      w-exe-cod-ncf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-num                to   w-exe-cnv-ncf          .
       ext-prm-acc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ext-prm-acc-999.
       ext-prm-acc-999.
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
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
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
      *              * [pdk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdk                 .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *-----------------------------------------------------------*
       exe-cph-000.
      *              *-------------------------------------------------*
      *              * Emissione 'header'                              *
      *              *-------------------------------------------------*
           perform   exe-cph-hea-000      thru exe-cph-hea-999        .
       exe-cph-100.
      *              *-------------------------------------------------*
      *              * Subroutine di accettazione                      *
      *              *-------------------------------------------------*
           perform   exe-cph-acc-000      thru exe-cph-acc-999        .
       exe-cph-900.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           go to     exe-cph-999.
       exe-cph-999.
           exit.


      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per 'header'                                   *
      *    *-----------------------------------------------------------*
       exe-cph-hea-000.
      *              *-------------------------------------------------*
      *              * Emissione testata documento                     *
      *              *-------------------------------------------------*
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<head>"                                         .
      *              *-------------------------------------------------*
      *              * Css                                             *
      *              *-------------------------------------------------*
           display   "<link rel='stylesheet' type='text/css' href='../cs
      -              "s/ele.css'>"                                    .
      *              *-------------------------------------------------*
      *              * Head - fine                                     *
      *              *-------------------------------------------------*
           display   "</head>"                                        .
      *              *-------------------------------------------------*
      *              * Corpo                                           *
      *              *-------------------------------------------------*
           display   "<body>"                                         .
       exe-cph-hea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-hea-999.
       exe-cph-hea-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine per i campi 'hidden'                           *
      *    *-----------------------------------------------------------*
       exe-cph-hid-000.
      *              *-------------------------------------------------*
      *              * Protocollo 'hidden'                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "acc_prt"            to   w-exe-hid-nam          .
           move      w-exe-prt-bfo        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       exe-cph-hid-100.
      *              *-------------------------------------------------*
      *              * Progressivo 'hidden'                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "acc_prg"            to   w-exe-hid-nam          .
           move      w-exe-prg-bfo        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       exe-cph-hid-200.
      *              *-------------------------------------------------*
      *              * Progressivo interno 'hidden'                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "acc_pri"            to   w-exe-hid-nam          .
           move      w-exe-pri-bfo        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       exe-cph-hid-300.
      *              *-------------------------------------------------*
      *              * Numero collo 'hidden'                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "acc_col"            to   w-exe-hid-nam          .
           move      w-exe-col-bfo        to   w-exe-hid-val          .
           perform   emi-htm-hid-000      thru emi-htm-hid-999        .
       exe-cph-hid-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-hid-999.
       exe-cph-hid-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per accettazione                               *
      *    *-----------------------------------------------------------*
       exe-cph-acc-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione tabella ubicazioni             *
      *                  *---------------------------------------------*
           perform   det-tbl-ubi-000      thru det-tbl-ubi-999        .
       exe-cph-acc-050.
      *              *-------------------------------------------------*
      *              * Campi 'hidden' per l'accettazione               *
      *              *-------------------------------------------------*
           perform   exe-cph-hid-000      thru exe-cph-hid-999        .
      *              *-------------------------------------------------*
      *              * Premessa corpo                                  *
      *              *-------------------------------------------------*
           display   "<table border=1 cellspacing=0 cellpadding=1 class=
      -              "bordotab width=100% >"                          .
       exe-cph-acc-100.
      *              *-------------------------------------------------*
      *              * Numero riga                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Numero riga"        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-bft-pri    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      02                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-150.
      *              *-------------------------------------------------*
      *              * Numero collo                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Collo"              to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-slc-num-bft-col    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      02                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-200.
      *              *-------------------------------------------------*
      *              * Prodotto                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Prodotto"           to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      w-exe-alf-pro        to   w-exe-ttd-txt          .
           move      02                   to   w-exe-ttd-col          .
           move      "S"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "N"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-300.
      *              *-------------------------------------------------*
      *              * Quantita'                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Quantita'"          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "B"            to   w-exe-ttd-stl
           else      move  "h"            to   w-exe-ttd-stl          .
      *
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           perform   exe-cph-acc-qta-000  thru exe-cph-acc-qta-999    .
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      w-exe-qta-cnv        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione valore in ordine            *
      *                  *---------------------------------------------*
           move      p-edt                to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-400.
      *              *-------------------------------------------------*
      *              * Ubicazione                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Ubicazione"         to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "B"            to   w-exe-ttd-stl
           else      move  "h"            to   w-exe-ttd-stl          .
      *
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Accettazione ubicazione                     *
      *                  *---------------------------------------------*
           perform   exe-cph-acc-ubi-000  thru exe-cph-acc-ubi-999    .
      *                  *---------------------------------------------*
      *                  * Visualizzazione ubicazione in anagrafica    *
      *                  *---------------------------------------------*
           move      w-exe-ubi-bfo        to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
           move      "B"                  to   w-exe-ttd-stl          .
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-500.
      *              *-------------------------------------------------*
      *              * Non conformita'                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Non conformita'"    to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "B"            to   w-exe-ttd-stl
           else      move  "h"            to   w-exe-ttd-stl          .
      *
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           perform   exe-cph-acc-ncf-000  thru exe-cph-acc-ncf-999    .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-600.
      *              *-------------------------------------------------*
      *              * Prodotto non conforme                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura riga                               *
      *                  *---------------------------------------------*
           display   "<tr>"                                           .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "Prodotto non conforme"
                                          to   w-exe-ttd-txt          .
           move      01                   to   w-exe-ttd-col          .
           move      "N"                  to   w-exe-ttd-snh          .
           move      "L"                  to   w-exe-ttd-all          .
           move      "S"                  to   w-exe-ttd-wdt          .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "B"            to   w-exe-ttd-stl
           else      move  "h"            to   w-exe-ttd-stl          .
      *
           perform   emi-htm-ttd-000      thru emi-htm-ttd-999        .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           perform   exe-cph-acc-pro-000  thru exe-cph-acc-pro-999    .
      *                  *---------------------------------------------*
      *                  * Chiusura riga                               *
      *                  *---------------------------------------------*
           display   "</tr>"                                          .
       exe-cph-acc-800.
      *              *-------------------------------------------------*
      *              * Fine corpo                                      *
      *              *-------------------------------------------------*
           display   "</table>"                                       .
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           display   "</body>"                                        .
           display   "</html>"                                        .
       exe-cph-acc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-999.
       exe-cph-acc-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione quantita'                      *
      *    *-----------------------------------------------------------*
       exe-cph-acc-qta-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td>"                                           .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      02                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BGD"               to   p-edm                  .
           move      w-exe-qta-cnv        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           display   "<input type='number' "                          .
           display   "id='acc_qta' name ='acc_qta' "                  .
           display   "class='numbersOnly' "                           .
           display   "style='text-align:right;' "                     .
      *
           move      spaces               to   w-exe-str-dsp          .
           string    "value='"  delimited by size
                     p-edt      delimited by spaces
                     "'"        delimited by size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
           if        w-exe-flg-spn        not  = "S"
                     go to exe-cph-acc-qta-600.
           display   "disabled "                                      .
       exe-cph-acc-qta-600.
      *              *-------------------------------------------------*
      *              * Chiusura input                                  *
      *              *-------------------------------------------------*
           display   "/>"                                             .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       exe-cph-acc-qta-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-qta-999.
       exe-cph-acc-qta-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine di accettazione ubicazione                     *
      *    *-----------------------------------------------------------*
       exe-cph-acc-ubi-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td>"                                            .
       exe-cph-acc-ubi-050.
      *              *-------------------------------------------------*
      *              * Script di supporto ai campi ubicazione          *
      *              *-------------------------------------------------*
           display   "<script>"                                       .
           display   "var $select1 = $('#pre_ubi'),"                  .
           display   "    $select2 = $('#cod_ubi'),"                  .
           display   "    $options = $select2.find('option');"        .
           display   "$select1.on('change', function() {"             .
           display   "$select2.html($options.filter('[value=""' + this.v
      -              "alue + '""]'));"                                .
           display   "}).trigger('change'); "                         .
      *              *-------------------------------------------------*
      *              * Script di supporto ad accettazione quantita'    *
      *              *-------------------------------------------------*
           display   "$('#acc_qta').change(function() {"              .
           display   "$('#acc_ncf').val(2);"               .
           display   "$('#acc_ncf').prop('disabled',true);"           .
           display   "$('#acc_pro').prop('disabled',true);"           .
           display   "});                             "               .
      *              *-------------------------------------------------*
      *              * Script di supporto ad accettazione Non confor-  *
      *              * mita'                                           *
      *              *-------------------------------------------------*
           display   "$('#acc_ncf').change(function() {"              .
______*    display   " var ncf = $('#acc_ncf').val();"                .
           display   "if (ncf == 4) {"                                .
           display   "$('#acc_pro').prop('enable',true);}"            .
           display   "else {$('#acc_pro').prop('disabled',true);}"    .
           display   "});                             "               .
      *              *-------------------------------------------------*
      *              * Fine script                                     *
      *              *-------------------------------------------------*
           display   "</script>"                                      .
      *              *-------------------------------------------------*
      *              * Apertura campo Select                           *
      *              *-------------------------------------------------*
           display   "<select name='pre_ubi' id='pre_ubi'>"           .
       exe-cph-acc-ubi-100.
      *              *-------------------------------------------------*
      *              * Costruzione tabella ubicazione 1                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-ubi-inx      .
       exe-cph-acc-ubi-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ubi-inx      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-ubi-inx    >    w-det-tbl-ubi-c01
                     go to exe-cph-acc-ubi-500.
      *                  *---------------------------------------------*
      *                  * Valore in campo di comodo                   *
      *                  *---------------------------------------------*
           move      w-det-tbl-pre-cod
                    (w-det-tbl-ubi-inx)   to   w-all-str-alf          .
      *                  *---------------------------------------------*
      *                  * Test se elemento selezionato                *
      *                  *---------------------------------------------*
           if        w-all-str-alf        =    w-exe-ubi-bfo
                                              (01 : 01)
                     move  "selected"     to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "disabled"     to   w-all-str-cat (2)
           else      move  spaces         to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Emissione elemento                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<option "
                                delimited by   size
                     w-all-str-cat (1)
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-all-str-cat (2)
                                delimited by   spaces
                     " value='"
                                delimited by   size
                     w-all-str-alf
                                delimited by   spaces
                     "'>"
                                delimited by   size
                     w-all-str-alf
                                delimited by   spaces
                     "</option>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to exe-cph-acc-ubi-200.
       exe-cph-acc-ubi-500.
      *              *-------------------------------------------------*
      *              * Chiusura accettazione ubicazione 1              *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
           display   "<select name='cod_ubi' id='cod_ubi'>"           .
       exe-cph-acc-ubi-550.
      *              *-------------------------------------------------*
      *              * Costruzione tabella ubicazione 2                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-ubi-inx      .
       exe-cph-acc-ubi-600.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ubi-inx      .
      *                  *---------------------------------------------*
      *                  * Test su contatore                           *
      *                  *---------------------------------------------*
           if        w-det-tbl-ubi-inx    >    w-det-tbl-ubi-c02
                     go to exe-cph-acc-ubi-800.
      *                  *---------------------------------------------*
      *                  * Valore in campo di comodo                   *
      *                  *---------------------------------------------*
           move      w-det-tbl-cod-ubi
                    (w-det-tbl-ubi-inx)   to   w-all-str-alf          .
      *                  *---------------------------------------------*
      *                  * Test se elemento selezionato                *
      *                  *---------------------------------------------*
           if        w-all-str-alf        =    w-exe-ubi-bfo
                     move  "selected"     to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "disabled"     to   w-all-str-cat (2)
           else      move  spaces         to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Emissione elemento                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<option "
                                delimited by   size
                     w-all-str-cat (1)
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-all-str-cat (2)
                                delimited by   spaces
                     " value='"
                                delimited by   size
                     w-all-str-alf
                    (01 : 01)
                                delimited by   spaces
                     "'>"
                                delimited by   size
                     w-all-str-alf
                                delimited by   spaces
                     "</option>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to exe-cph-acc-ubi-600.
       exe-cph-acc-ubi-800.
      *              *-------------------------------------------------*
      *              * Chiusura input                                  *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           move      "C"                  to   w-exe-tag-flg          .
           move      "td"                 to   w-exe-tag-tag          .
           perform   emi-htm-tag-000      thru emi-htm-tag-999        .
       exe-cph-acc-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-ubi-999.
       exe-cph-acc-ubi-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per accettazione Non conformita'               *
      *    *-----------------------------------------------------------*
       exe-cph-acc-ncf-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_ncf' name= 'cel_ncf' colspan=2>"    .
      *              *-------------------------------------------------*
      *              * Apertura campo Select                           *
      *              *-------------------------------------------------*
           display   "<select name='acc_ncf' id='acc_ncf'>"           .
       exe-cph-acc-ncf-100.
      *              *-------------------------------------------------*
      *              * Ciclo per 'n' elementi                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tbl-ncf-inx      .
       exe-cph-acc-ncf-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ncf-inx      .
           if        w-det-tbl-ncf-inx    >    w-det-tbl-ncf-max
                     go to exe-cph-acc-ncf-800.
      *                  *---------------------------------------------*
      *                  * Editing valore                              *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-tbl-ncf-inx    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Non conformita'                 *
      *                  *---------------------------------------------*
           if        w-det-tbl-ncf-inx    =    01
                     move  "(nessuna)"
                                          to   w-exe-des-ncf
           else if   w-det-tbl-ncf-inx    =    02
                     move  "Quantita' diversa"
                                          to   w-exe-des-ncf
           else if   w-det-tbl-ncf-inx    =    03
                     move  "Prod. danneggiato"
                                          to   w-exe-des-ncf
           else if   w-det-tbl-ncf-inx    =    04
                     move  "Prodotto difforme"
                                          to   w-exe-des-ncf
           else      move  "(nessuna)"    to   w-exe-des-ncf          .
      *                  *---------------------------------------------*
      *                  * Test se elemento selezionato                *
      *                  *---------------------------------------------*
           if        w-exe-cnv-ncf        =    w-det-tbl-ncf-inx
                     move  "selected"     to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Eventuale disabilitazione                   *
      *                  *---------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "disabled"     to   w-all-str-cat (2)
           else      move  spaces         to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Emissione elemento                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<option "
                                delimited by   size
                     w-all-str-cat (1)
                                delimited by   spaces
                     " "
                                delimited by   size
                     w-all-str-cat (2)
                                delimited by   spaces
                     " value='"
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     "'>"
                     w-exe-des-ncf
                                delimited by   size
                     "</option>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
       exe-cph-acc-ncf-700.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-ncf-200.
       exe-cph-acc-ncf-800.
      *              *-------------------------------------------------*
      *              * Chiusura accettazione                           *
      *              *-------------------------------------------------*
           display   "</select>"                                      .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       exe-cph-acc-ncf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-ncf-999.
       exe-cph-acc-ncf-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di lettura e preparazione html                      *
      *    *                                                           *
      *    * Subroutine per accettazione Codice prodotto               *
      *    *-----------------------------------------------------------*
       exe-cph-acc-pro-000.
      *              *-------------------------------------------------*
      *              * Apertura cella                                  *
      *              *-------------------------------------------------*
           display   "<td id='cel_pro' name='cel_pro' colspan=2>"     .
       exe-cph-acc-pro-100.
      *              *-------------------------------------------------*
      *              * Ricerca del codice numerico prodotto            *
      *              *-------------------------------------------------*
           if        w-exe-acc-pro        =    spaces
                     go to exe-cph-acc-pro-200.
           move      w-exe-acc-pro        to   w-det-num-pro-alf      .
           perform   det-num-pro-000      thru det-num-pro-999        .
           move      w-det-num-pro-num    to   w-exe-acc-pnu          .
           move      w-det-num-pro-alf    to   w-exe-acc-pro          .
       exe-cph-acc-pro-200.
      *              *-------------------------------------------------*
      *              * Eventuale disabilitazione                       *
      *              *-------------------------------------------------*
           if        w-exe-flg-spn        =    "S"
                     move  "disabled"     to   w-all-str-cat (2)
           else      move  spaces         to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-exe-str-dsp          .
      *
           string    "<input type='text' id='acc_pro' name='acc_pro' "
                                delimited by   size
                     w-all-str-cat (2)
                                delimited by   spaces
                     " value='"
                                delimited by   size
                     w-exe-acc-pro
                                delimited by   spaces
                     "' style='text-transform: uppercase;' "
                                delimited by   size
                     "maxlength='18'>"
                                delimited by   size
                                          into w-exe-str-dsp          .
      *
           display   w-exe-str-dsp                                    .
      *              *-------------------------------------------------*
      *              * Chiusura cella                                  *
      *              *-------------------------------------------------*
           display   "</td>"                                          .
       exe-cph-acc-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-cph-acc-pro-999.
       exe-cph-acc-pro-999.
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
      *                      * Eventuale 'rosso'                       *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        not  = "r"
                     go to emi-htm-ttd-700.
           move      "<B style='color:red'>"
                                         to   w-exe-ttd-st1          .
           move      "</B>"              to   w-exe-ttd-st2          .
       emi-htm-ttd-700.
      *                      *-----------------------------------------*
      *                      * Eventuale 'h2'                          *
      *                      *-----------------------------------------*
           if        w-exe-ttd-stl        not  = "h"
                     go to emi-htm-ttd-800.
           move      "<h2>"               to   w-exe-ttd-st1          .
           move      "</h2>"              to   w-exe-ttd-st2          .
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
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-tbl-ubi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-tbl-ubi-rot      .
           move      zero                 to   w-det-tbl-ubi-c01      .
           move      zero                 to   w-det-tbl-ubi-c02      .
       det-tbl-ubi-050.
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
       det-tbl-ubi-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [zub]                         *
      *              *-------------------------------------------------*
           move      "ST"                 to   f-ope                  .
           move      "CODUBI    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-zub-cod-dpz         .
           move      spaces               to   rf-zub-cod-ubi         .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata: a close                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-ubi-900.
       det-tbl-ubi-200.
      *              *-------------------------------------------------*
      *              * Read-next su archivio [zub]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofzub"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zub                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tbl-ubi-900.
       det-tbl-ubi-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [zub]                      *
      *              *-------------------------------------------------*
           if        rf-zub-cod-dpz       not  = 01
                     go to det-tbl-ubi-900.
       det-tbl-ubi-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-tbl-ubi-500.
      *              *-------------------------------------------------*
      *              * Rottura su prima parte codice ubicazione        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-det-tbl-ubi-rot    =    spaces
                     move  rf-zub-cod-ubi to   w-det-tbl-ubi-rot
                     go to det-tbl-ubi-600.
           if        rf-zub-cod-ubi
                    (01 : 01)             not  = w-det-tbl-ubi-rot
                     move  rf-zub-cod-ubi to   w-det-tbl-ubi-rot
                     go to det-tbl-ubi-600.
           go to     det-tbl-ubi-700.
       det-tbl-ubi-600.
      *                  *---------------------------------------------*
      *                  * Incremento pre-tabella                      *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ubi-c01      .
           move      w-det-tbl-ubi-rot    to   w-det-tbl-pre-cod
                                              (w-det-tbl-ubi-c01)     .
       det-tbl-ubi-700.
      *                  *---------------------------------------------*
      *                  * Incremento tabella                          *
      *                  *---------------------------------------------*
           add       1                    to   w-det-tbl-ubi-c02      .
           move      rf-zub-cod-ubi       to   w-det-tbl-cod-ubi
                                              (w-det-tbl-ubi-c02)     .
       det-tbl-ubi-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-tbl-ubi-200.
       det-tbl-ubi-900.
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
           go to     det-tbl-ubi-999.
       det-tbl-ubi-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
