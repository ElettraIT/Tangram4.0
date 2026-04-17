       Identification Division.
       Program-Id.                                 psvfgrh0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:    cgi                 *
      *                                   Fase:    svfgrp              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/03/04    *
      *                       Ultima revisione:    NdK del 14/05/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   CGI per generare grafici AddGraph 1.0       *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *              Input  : w-exe-dat-min : data iniziale (POST)     *
      *                                                                *
      *                       w-exe-dat-max : data finale   (POST)     *
      *                                                                *
      *                                                                *
      *              Output : dati_fat_reg.js nella cartella           *
      *                                                                *
      *                       /abd/web/htm/add_graph                   *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************
      
      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

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
      *            * Codice regione                                    *
      *            *---------------------------------------------------*
               10  srt-cod-rgn            pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Descrizione regione                               *
      *            *---------------------------------------------------*
               10  srt-des-rgn            pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Importo fatturato                                 *
      *            *---------------------------------------------------*
               10  srt-sta-val            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Incidenza sulla statistica                        *
      *            *   - P : In positivo                               *
      *            *   - N : In negativo                               *
      *            *---------------------------------------------------*
               10  srt-sta-pon            pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*
      
      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [gxr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxr"                          .
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      
      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'mfatfit0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mfatfit0.mdl"                   .

      *    *===========================================================*
      *    * Area di comodo                                            *
      *    *-----------------------------------------------------------*
       01  w-exe.
      *        *-------------------------------------------------------*
      *        * Data di sistema                                       *
      *        *-------------------------------------------------------*
           05  w-exe-dat-exe              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Date in input                                         *
      *        *-------------------------------------------------------*
           05  w-exe-dat-min              pic  9(07)                  .
           05  w-exe-dat-max              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per date in input                              *
      *        *-------------------------------------------------------*
           05  w-exe-ann-inp              pic  x(04)                  .
           05  w-exe-mes-inp              pic  x(02)                  .
           05  w-exe-ann-ric              pic  9(04)                  .
           05  w-exe-mes-ric              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per editing date                               *
      *        *-------------------------------------------------------*
           05  w-exe-edt-min              pic  x(12)                  .
           05  w-exe-edt-max              pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-prm-x01              pic  x(80)                  .
           05  w-exe-prm-x02              pic  x(80)                  .
           05  w-exe-prm-x03              pic  x(80)                  .
           05  w-exe-prm-xxx              pic  x(80)                  .
           05  w-exe-prm-001.
               10  w-exe-prm-p01          pic  x(06)                  .
               10  w-exe-prm-v01          pic  x(14)                  .
           05  w-exe-prm-002.
               10  w-exe-prm-p02          pic  x(11)                  .
               10  w-exe-prm-v02          pic  x(40)                  .
           05  w-exe-prm-003.
               10  w-exe-prm-p03          pic  x(01)                  .
               10  w-exe-prm-v03          pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-ctr-001              pic  9(03)                  .
           05  w-exe-ctr-002              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per rottura su codice regione                  *
      *        *-------------------------------------------------------*
           05  w-exe-cod-rgn              pic  x(03)                  .
           05  w-exe-des-rgn              pic  x(25)                  .
           05  w-exe-sta-val              pic s9(13)                  .

      *    *===========================================================*
      *    * File area generica                                        *
      *    *-----------------------------------------------------------*
       01  f-xxx.
           05  f-xxx-pat                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxr]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxr.
               10  w-let-arc-gxr-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxr-cod      pic  x(03)     value spaces .
               10  w-let-arc-gxr-des      pic  x(25)     value spaces .
               10  w-let-arc-gxr-exc      pic  x(03)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxp.
               10  w-let-arc-gxp-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxp-cod      pic  x(03)     value spaces .
               10  w-let-arc-gxp-des      pic  x(25)     value spaces .
               10  w-let-arc-gxp-rgn      pic  x(03)     value spaces .
               10  w-let-arc-gxp-exc      pic  x(03)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxc-cod      pic  9(05)     value zero   .
               10  w-let-arc-gxc-des      pic  x(30)     value spaces .
               10  w-let-arc-gxc-prv      pic  x(03)     value spaces .
               10  w-let-arc-gxc-exc      pic  9(05)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli] e [dcc]                *
      *        *-------------------------------------------------------*
           05  w-let-cli-dcc.
               10  w-let-cli-dcc-cli      pic  9(07)     value zero   .
               10  w-let-cli-dcc-dpz      pic  x(04)     value spaces .
               10  w-let-cli-dcc-rag      pic  x(40)     value spaces .
               10  w-let-cli-dcc-mne      pic  x(10)     value spaces .
               10  w-let-cli-dcc-naz      pic  x(03)     value spaces .
               10  w-let-cli-dcc-cmn      pic  9(05)     value zero   .
               10  w-let-cli-dcc-rgn      pic  x(03)     value spaces .
               10  w-let-cli-dcc-prv      pic  x(03)     value spaces .
               10  w-let-cli-dcc-exc      pic  9(07)     value zero   .
               10  w-let-cli-dcc-exd      pic  x(04)     value spaces .

      *    *===========================================================*
      *    * Work per selezione su record [fit] in esame               *
      *    *-----------------------------------------------------------*
       01  w-sel-fit.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *   - Spaces : Selezione superata                       *
      *        *   - #      : Selezione non superata                   *
      *        *-------------------------------------------------------*
           05  w-sel-fit-flg-exi          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo su dipendenze selezionate         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-ctr-dpz          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cod-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza per il cliente                      *
      *        *-------------------------------------------------------*
           05  w-sel-fit-dpz-cli          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico per il cliente                              *
      *        *-------------------------------------------------------*
           05  w-sel-fit-mne-cli          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Ragione sociale per il cliente                        *
      *        *-------------------------------------------------------*
           05  w-sel-fit-rag-cli          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice nazione per il cliente                         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-naz-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice comune per il cliente                          *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cmn-cli          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice regione per il cliente                         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-rgn-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice provincia per il cliente                       *
      *        *-------------------------------------------------------*
           05  w-sel-fit-prv-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice tipo classe geografica cliente                 *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cod-cgc          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione tipo classe geografica cliente            *
      *        *-------------------------------------------------------*
           05  w-sel-fit-des-cgc          pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tipo classe geografica cliente esistente      *
      *        *-------------------------------------------------------*
           05  w-sel-fit-flg-cgc          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work per tabella colori                                   *
      *    *-----------------------------------------------------------*
       01  w-col.
      *        *-------------------------------------------------------*
      *        * Tabella dei colori                                    *
      *        *-------------------------------------------------------*
           05  w-col-tbl-col.
      *            *---------------------------------------------------*
      *            * Tabella                                           *
      *            *---------------------------------------------------*
               10  w-col-tbl-col-tbl.
                   15  filler             pic  x(06) value "FF00BB"   .
                   15  filler             pic  x(06) value "FF0000"   .
                   15  filler             pic  x(06) value "00FF00"   .
                   15  filler             pic  x(06) value "FF6600"   .
                   15  filler             pic  x(06) value "0000FF"   .
                   15  filler             pic  x(06) value "FFFF00"   .
                   15  filler             pic  x(06) value "00FFFF"   .
                   15  filler             pic  x(06) value "FF00FF"   .
                   15  filler             pic  x(06) value "8A2BE2"   .
                   15  filler             pic  x(06) value "A52A2A"   .
                   15  filler             pic  x(06) value "00CC00"   .
                   15  filler             pic  x(06) value "FF6666"   .
                   15  filler             pic  x(06) value "3399FF"   .
                   15  filler             pic  x(06) value "AA99FF"   .
                   15  filler             pic  x(06) value "009966"   .
                   15  filler             pic  x(06) value "CC3399"   .
                   15  filler             pic  x(06) value "FFCC33"   .
                   15  filler             pic  x(06) value "F23456"   .
                   15  filler             pic  x(06) value "33FF66"   .
                   15  filler             pic  x(06) value "FF0066"   .
                   15  filler             pic  x(06) value "FF9933"   .
      *            *---------------------------------------------------*
      *            * Ridefinizione tabella colori                      *
      *            *---------------------------------------------------*
               10  w-col-tbl-col-tbl-r redefines
                   w-col-tbl-col-tbl.
                   15  w-col-tbl-col-ele occurs 21.
                       20  w-col-tbl-col-rgb
                                          pic  x(06)                  .
               10  w-col-tbl-col-ctr      pic  9(03)                  .

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
      *              * Ciclo di scansione                              *
      *              *-------------------------------------------------*
           perform   exe-scn-000          thru exe-scn-999            .
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
           move      spaces               to   w-exe-prm-001          .
           move      spaces               to   w-exe-prm-002          .
           move      spaces               to   w-exe-prm-003          .
           move      spaces               to   w-exe-prm-x01          .
           move      spaces               to   w-exe-prm-x02          .
           move      spaces               to   w-exe-prm-x03          .
           move      spaces               to   w-exe-prm-xxx          .
      *              *-------------------------------------------------*
      *              * Lettura della variabile di environment          *
      *              *-------------------------------------------------*
           move      "I2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Estrazione primi 2 parametri                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  o-pat
                                delimited by "&"
                                          into w-exe-prm-x01
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  o-pat                into w-exe-prm-x02
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Estrazione 1. parametro - componenti            *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  w-exe-prm-x01
                                delimited by "="
                                          into w-exe-prm-p01
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  w-exe-prm-x01        into w-exe-prm-v01
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Estrazione 2. parametro - componenti            *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-ctr-001          .
           unstring  w-exe-prm-x02
                                delimited by "="
                                          into w-exe-prm-p02
                                    count in   w-exe-ctr-001          .
           add       2                    to   w-exe-ctr-001          .
           unstring  w-exe-prm-x02        into w-exe-prm-v02
                                  with pointer w-exe-ctr-001          .
      *              *-------------------------------------------------*
      *              * Assegnazione valori letti                       *
      *              *-------------------------------------------------*
           move      w-exe-prm-v01        to   w-exe-ann-inp          .
           move      w-exe-prm-v02        to   w-exe-mes-inp          .
       ext-prm-400.
      *              *-------------------------------------------------*
      *              * Conversione valori letti                        *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      04                   to   v-car                  .
           move      w-exe-ann-inp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      v-num                to   w-exe-ann-ric          .
           move      "CV"                 to   v-ope                  .
           move      02                   to   v-car                  .
           move      w-exe-mes-inp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-exe-mes-ric          .
       ext-prm-420.
      *              *-------------------------------------------------*
      *              * Normalizzazione date                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-dat-min          .
           move      9999999              to   w-exe-dat-max          .
      *                  *---------------------------------------------*
      *                  * Se tutti gli anni                           *
      *                  *---------------------------------------------*
           if        w-exe-ann-ric        =    9999 or
                     w-exe-ann-ric        =    zero or
                     w-exe-ann-ric        <    1990
                     go to ext-prm-500.
      *                  *---------------------------------------------*
      *                  * Se un anno e tutti i mesi                   *
      *                  *---------------------------------------------*
           if        w-exe-mes-ric        <    13   and
                     w-exe-mes-ric        >    01
                     go to ext-prm-450.
      *                      *-----------------------------------------*
      *                      * Data minima                             *
      *                      *-----------------------------------------*
           subtract  1900                 from w-exe-ann-ric
                                        giving s-ann                  .
           move      01                   to   s-mes                  .
           move      01                   to   s-gio                  .
      *
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-min          .
      *                      *-----------------------------------------*
      *                      * Data massima                            *
      *                      *-----------------------------------------*
           subtract  1900                 from w-exe-ann-ric
                                        giving s-ann                  .
           move      12                   to   s-mes                  .
           move      31                   to   s-gio                  .
      *
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-max          .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to ext-prm-500.
       ext-prm-450.
      *                  *---------------------------------------------*
      *                  * Se un anno e un mese                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data minima                             *
      *                      *-----------------------------------------*
           subtract  1900                 from w-exe-ann-ric
                                        giving s-ann                  .
           move      w-exe-mes-ric        to   s-mes                  .
           move      01                   to   s-gio                  .
      *
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-exe-dat-min          .
      *                      *-----------------------------------------*
      *                      * Data massima                            *
      *                      *-----------------------------------------*
           subtract  1900                 from w-exe-ann-ric
                                        giving s-ann                  .
           move      w-exe-mes-ric        to   s-mes                  .
           move      31                   to   s-gio                  .
      *
           move      "NS"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ext-prm-460.
           move      s-dat                to   w-exe-dat-max          .
      *
           move      "CD"                 to   s-ope                  .
           move      w-exe-dat-max        to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to ext-prm-460.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to ext-prm-500.
       ext-prm-500.
      *              *-------------------------------------------------*
      *              * Cancellazione preventiva file [dati_fat_reg.js] *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione di un comando 'Unix' da shell    *
      *                  *---------------------------------------------*
           move      "chmod 0777 /abd/web/htm/add_graph/dati_fat_reg.js"
                                          to   o-shs                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo 'mopsys'                *
      *                  *---------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Esecuzione di un comando 'Unix' da shell    *
      *                  *---------------------------------------------*
           move      "rm /abd/web/htm/add_graph/dati_fat_reg.js"
                                          to   o-shs                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo 'mopsys'                *
      *                  *---------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       ext-prm-999.
           exit.

      *    *===========================================================*
      *    * Open files                                                *
      *    *-----------------------------------------------------------*
       opn-fls-000.
      *              *-------------------------------------------------*
      *              * Open modulo per la determinazione della voce    *
      *              * 'Fatturato' sulle statistiche di vendita sul    *
      *              * fatturato da Testate Documenti                  *
      *              *-------------------------------------------------*
           move      "svfgrh"             to   w-mod-fat-fit-fas      .
           perform   mod-fat-fit-opn-000  thru mod-fat-fit-opn-999    .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
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
      *              * [gxr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Open generica file sequenziale di output        *
      *              *-------------------------------------------------*
           perform   opn-seq-out-000      thru opn-seq-out-999        .
       opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files                                               *
      *    *-----------------------------------------------------------*
       cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo per la determinazione della voce   *
      *              * 'Fatturato' sulle statistiche di vendita sul    *
      *              * fatturato da Testata Documenti                  *
      *              *-------------------------------------------------*
           perform   mod-fat-fit-cls-000  thru mod-fat-fit-cls-999    .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
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
      *              * [gxr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * Chiusura del file in output                     *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvout"                         .
      *              *-------------------------------------------------*
      *              * Rename file generato                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esecuzione di un comando 'Unix' da shell    *
      *                  *---------------------------------------------*
           move      "chmod 0777 /abd/web/htm/add_graph/001.wrk"
                                          to   o-shs                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo 'mopsys'                *
      *                  *---------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Esecuzione di un comando 'Unix' da shell    *
      *                  *---------------------------------------------*
           move      "mv /abd/web/htm/add_graph/001.wrk /abd/web/htm/add
      -              "_graph/dati_fat_reg.js"
                                          to   o-shs                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo 'mopsys'                *
      *                  *---------------------------------------------*
           move      "SH"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di scansione                                        *
      *    *-----------------------------------------------------------*
       exe-scn-000.
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   exe-scn-inp-000
                                          thru exe-scn-inp-999
                     output procedure     is   exe-scn-out-000
                                          thru exe-scn-out-999        .
       exe-scn-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di scansione                                        *
      *    *                                                           *
      *    * Fase di input                                             *
      *    *-----------------------------------------------------------*
       exe-scn-inp-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       exe-scn-inp-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione lettura sequenziale [fit]      *
      *              *-------------------------------------------------*
           perform   fil-fit-ing-000      thru fil-fit-ing-999        .
       exe-scn-inp-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [fit]              *
      *              *-------------------------------------------------*
           perform   fil-fit-get-000      thru fil-fit-get-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        w-mod-fat-fit-flg    not  = spaces
                     go to exe-scn-inp-900.
       exe-scn-inp-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [fit]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il valore fatturato e' a zero : si igno- *
      *                  * ra il record                                *
      *                  *---------------------------------------------*
           if        w-mod-fat-fit-val    =    zero
                     go to exe-scn-inp-200.
      *                  *---------------------------------------------*
      *                  * Se il valore fatturato non incide  ne' in   *
      *                  * positivo ne' in negativo sulla statistica : *
      *                  * si ignora il record                         *
      *                  *---------------------------------------------*
           if        w-mod-fat-fit-pon    not  = "P" and
                     w-mod-fat-fit-pon    not  = "N"
                     go to exe-scn-inp-200.
      *                  *---------------------------------------------*
      *                  * Se il valore fatturato non incide su nessu- *
      *                  * no dei periodi della statistica : si igno-  *
      *                  * ra il record                                *
      *                  *---------------------------------------------*
           if        w-mod-fat-fit-sn1    not  = "S" and
                     w-mod-fat-fit-sn2    not  = "S" and
                     w-mod-fat-fit-sn3    not  = "S"
                     go to exe-scn-inp-200.
      *                  *---------------------------------------------*
      *                  * Selezione sul record [fit] in esame         *
      *                  *---------------------------------------------*
           perform   sel-fit-rec-000      thru sel-fit-rec-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si  ignora il   *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-sel-fit-flg-exi    not  = spaces
                     go to exe-scn-inp-200.
       exe-scn-inp-600.
      *              *-------------------------------------------------*
      *              * Composizione del record di sort                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Codice regione                              *
      *                  *---------------------------------------------*
           move      w-sel-fit-cod-cgc    to   srt-cod-rgn            .
      *                  *---------------------------------------------*
      *                  * Descrizione regione                         *
      *                  *---------------------------------------------*
           move      w-sel-fit-des-cgc    to   srt-des-rgn            .
      *                  *---------------------------------------------*
      *                  * Importo fatturato                           *
      *                  *---------------------------------------------*
           move      w-mod-fat-fit-val    to   srt-sta-val            .
      *                  *---------------------------------------------*
      *                  * Incidenza sulla statistica                  *
      *                  *---------------------------------------------*
           move      w-mod-fat-fit-pon    to   srt-sta-pon            .
       exe-scn-inp-700.
      *              *-------------------------------------------------*
      *              * Rilascio del record                             *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       exe-scn-inp-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-scn-inp-200.
       exe-scn-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-scn-inp-999.
       exe-scn-inp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di scansione                                        *
      *    *                                                           *
      *    * Fase di output                                            *
      *    *-----------------------------------------------------------*
       exe-scn-out-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore di comodo         *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Inizializzazione area di rottura            *
      *                  *---------------------------------------------*
           move      spaces               to   w-exe-cod-rgn          .
           move      spaces               to   w-exe-des-rgn          .
           move      zero                 to   w-exe-sta-val          .
           move      zero                 to   w-col-tbl-col-ctr      .
       exe-scn-out-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            go to exe-scn-out-900.
       exe-scn-out-400.
      *              *-------------------------------------------------*
      *              * Test se rottura                                 *
      *              *-------------------------------------------------*
           if        srt-cod-rgn          not  = w-exe-cod-rgn
                     go to exe-scn-out-500
           else      go to exe-scn-out-600.
       exe-scn-out-500.
      *              *-------------------------------------------------*
      *              * Se rottura                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se primo elemento                      *
      *                  *---------------------------------------------*
           if        w-exe-cod-rgn        =    spaces
                     go to exe-scn-out-550.
      *                  *---------------------------------------------*
      *                  * Incremento contatore colori                 *
      *                  *---------------------------------------------*
           add       1                    to   w-col-tbl-col-ctr      .
      *                  *---------------------------------------------*
      *                  * Incremento contatore di comodo              *
      *                  *---------------------------------------------*
           add       1                    to   w-exe-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Composizione record [xml] dall' area record *
      *                  * [fit] e dall'area 'w-sel-fit'               *
      *                  *---------------------------------------------*
           perform   cmp-xml-000          thru cmp-xml-999            .
       exe-scn-out-550.
      *                  *---------------------------------------------*
      *                  * Preparazione rottura                        *
      *                  *---------------------------------------------*
           move      srt-cod-rgn          to   w-exe-cod-rgn          .
           move      srt-des-rgn          to   w-exe-des-rgn          .
           move      zero                 to   w-exe-sta-val          .
       exe-scn-out-600.
      *              *-------------------------------------------------*
      *              * Se non rottura                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Non rottura                                 *
      *                  *---------------------------------------------*
           if        srt-sta-pon          not  = "N"
                     add      srt-sta-val to   w-exe-sta-val
           else      subtract srt-sta-val from w-exe-sta-val          .
       exe-scn-out-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     exe-scn-out-200.
       exe-scn-out-900.
      *              *-------------------------------------------------*
      *              * Elemento in sospeso                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore colori                 *
      *                  *---------------------------------------------*
           add       1                    to   w-col-tbl-col-ctr      .
      *                  *---------------------------------------------*
      *                  * Incremento contatore di comodo              *
      *                  *---------------------------------------------*
           add       1                    to   w-exe-ctr-002          .
      *                  *---------------------------------------------*
      *                  * Composizione record [xml] dall' area record *
      *                  * [fit] e dall'area 'w-sel-fit'               *
      *                  *---------------------------------------------*
           perform   cmp-xml-000          thru cmp-xml-999            .
       exe-scn-out-920.
      *              *-------------------------------------------------*
      *              * Chiusura grafico                                *
      *              *-------------------------------------------------*
           perform   cmp-xml-ftr-000      thru cmp-xml-ftr-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-scn-out-999.
       exe-scn-out-999.
           exit.

      *    *===========================================================*
      *    * Selezioni su record [fit] in esame                        *
      *    *-----------------------------------------------------------*
       sel-fit-rec-000.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *                                                 *
      *              * Attuale forzatura                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se una sola dipendenza selezionata          *
      *                  *---------------------------------------------*
           if        rf-fit-cod-dpz       =    01
                     go to sel-fit-rec-100
           else      go to sel-fit-rec-900.
       sel-fit-rec-100.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica [cli] e [dcc]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-fit-cod-cli       to   w-let-cli-dcc-cli      .
           move      rf-fit-dpz-cli       to   w-let-cli-dcc-dpz      .
           perform   let-cli-dcc-000      thru let-cli-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-cli    to   w-sel-fit-cod-cli      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza per il cliente        *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-dpz    to   w-sel-fit-dpz-cli      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale per il cliente          *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rag    to   w-sel-fit-rag-cli      .
      *                      *-----------------------------------------*
      *                      * Mnemonico per il cliente                *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-mne    to   w-sel-fit-mne-cli      .
      *                      *-----------------------------------------*
      *                      * Codice nazione per il cliente           *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-naz    to   w-sel-fit-naz-cli      .
      *                      *-----------------------------------------*
      *                      * Codice comune per il cliente            *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-cmn    to   w-sel-fit-cmn-cli      .
      *                      *-----------------------------------------*
      *                      * Codice regione per il cliente           *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rgn    to   w-sel-fit-rgn-cli      .
      *                      *-----------------------------------------*
      *                      * Codice provincia per il cliente         *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-prv    to   w-sel-fit-prv-cli      .
       sel-fit-rec-200.
      *              *-------------------------------------------------*
      *              * Selezione su codice classe geografica cliente   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo di classe geografica cliente :      *
      *                  * regione                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura [gxr] per la regione            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-sel-fit-rgn-cli    to   w-let-arc-gxr-cod      .
           perform   let-arc-gxr-000      thru let-arc-gxr-999        .
      *                          *-------------------------------------*
      *                          * Dati letti in area di comodo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice regione                  *
      *                              *---------------------------------*
           move      w-let-arc-gxr-cod    to   w-sel-fit-cod-cgc      .
      *                              *---------------------------------*
      *                              * Descrizione regione             *
      *                              *---------------------------------*
           move      w-let-arc-gxr-des    to   w-sel-fit-des-cgc      .
      *                              *---------------------------------*
      *                              * Flag di regione esistente       *
      *                              *---------------------------------*
           move      w-let-arc-gxr-flg    to   w-sel-fit-flg-cgc      .
       sel-fit-rec-290.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sel-fit-rec-400.
       sel-fit-rec-400.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione superata       *
      *                  *---------------------------------------------*
           move      spaces               to   w-sel-fit-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fit-rec-999.
       sel-fit-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione non superata               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione non superata   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sel-fit-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fit-rec-999.
       sel-fit-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [xml]                                 *
      *    *-----------------------------------------------------------*
       cmp-xml-000.
      *              *-------------------------------------------------*
      *              * Emissione testata                               *
      *              *-------------------------------------------------*
           perform   cmp-xml-hdr-000      thru cmp-xml-hdr-999        .
       cmp-xml-100.
      *              *-------------------------------------------------*
      *              * Dati                                            *
      *              *-------------------------------------------------*
           perform   cmp-xml-rig-000      thru cmp-xml-rig-999        .
       cmp-xml-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-xml-999.
       cmp-xml-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [xml]                                 *
      *    *                                                           *
      *    * Testata                                                   *
      *    *-----------------------------------------------------------*
       cmp-xml-hdr-000.
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-exe-ctr-002        >    1
                     go to cmp-xml-hdr-900.
                     
           display   "Content-type: text/html"
                                          with no advancing           .
           display   ""                                               .
           display   "<head>"                                         .
           display   "<title>Prodotti "   with no advancing           .
           display   "GRAFICI</title>"                                .
           display   "</head>"                                        .

           display   "<body "             with no advancing           .
           display   "bgcolor=#ffffff "                               .
           display   "onLoad=""document.location='../add_graph/ven_reg.h
      -              "tml';"">"                                       .
           display   "</body>"                                        .
           display   "</html>"                                        .
                     
       cmp-xml-hdr-050.
      *              *-------------------------------------------------*
      *              * Variabile per Javascript                        *
      *              *-------------------------------------------------*
           move      "var starter = true;"
                                          to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "/*"                 to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       cmp-xml-hdr-100.
      *              *-------------------------------------------------*
      *              * Preparazione del titolo                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data minima                         *
      *                  *---------------------------------------------*
           if        w-exe-dat-min        =    zero
                     move  spaces         to   w-exe-edt-min
                     go to cmp-xml-hdr-120.
      *
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-exe-dat-min        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      12                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "dal_"               to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-edt-min          .
       cmp-xml-hdr-120.
      *                  *---------------------------------------------*
      *                  * Editing data massima                        *
      *                  *---------------------------------------------*
           if        w-exe-dat-max        =    9999999 or
                     w-exe-dat-max        =    zero
                     move  spaces         to   w-exe-edt-max
                     go to cmp-xml-hdr-200.
      *
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-exe-dat-max        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      12                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "_al_"               to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-exe-edt-max          .
       cmp-xml-hdr-200.
      *              *-------------------------------------------------*
      *              * Parametri di apertura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione                                *
      *                  *---------------------------------------------*
           move      spaces               to   g-rec                  .
      *
           string    "<graph bgcolor='ffffff' "
                                delimited by   size
                     "gridbgcolor='f5f5f5' "
                                delimited by   size
                     "xaxisname='Regioni' "
                                delimited by   size
                     "yaxisname='Quote' "
                                delimited by   size
                     "caption='Fatturato per Regione "
                                delimited by   size
                     w-exe-edt-min
                                delimited by   spaces
                     w-exe-edt-max
                                delimited by   spaces
                     "'"        delimited by   size
                     "showgridbg='1' "
                                delimited by   size
                     "showvalues='0' "
                                delimited by   size
                     "hovercapSepChar=':' "
                                delimited by   size
                     "formatNumber='0' >"
                                delimited by   size
                                          into g-rec                  .
      *
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       cmp-xml-hdr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-xml-hdr-999.
       cmp-xml-hdr-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [xml]                                 *
      *    *                                                           *
      *    * Riga                                                      *
      *    *-----------------------------------------------------------*
       cmp-xml-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing fatturato                           *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-exe-sta-val        to   v-num                  .
           divide    100                  into v-num
                                       rounded                        .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       cmp-xml-rig-200.
      *              *-------------------------------------------------*
      *              * Composizione della riga                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eliminazione eventuali '('                  *
      *                  *---------------------------------------------*
           inspect   w-exe-des-rgn
                                replacing all  "("
                                          by   "_"                    .
      *                  *---------------------------------------------*
      *                  * Eliminazione eventuali ')'                  *
      *                  *---------------------------------------------*
           inspect   w-exe-des-rgn
                                replacing all  ")"
                                          by   "_"                    .
      *                  *---------------------------------------------*
      *                  * Eliminazione eventuali '                    *
      *                  *---------------------------------------------*
           inspect   w-exe-des-rgn
                                replacing all  "'"
                                          by   " "                    .
      *                  *---------------------------------------------*
      *                  * Preparazione contatore colore               *
      *                  *---------------------------------------------*
           if        w-exe-cod-rgn        =    "ABR"
                     move  02             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "BAS"
                     move  03             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "CAL"
                     move  04             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "CAM"
                     move  05             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "EMI"
                     move  06             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "FRI"
                     move  07             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "LAZ"
                     move  08             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "LIG"
                     move  09             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "LOM"
                     move  10             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "MAR"
                     move  11             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "MOL"
                     move  12             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "PIE"
                     move  13             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "PUG"
                     move  14             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "SAR"
                     move  15             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "SIC"
                     move  16             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "TOS"
                     move  17             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "TRE"
                     move  18             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "UMB"
                     move  19             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "VAL"
                     move  20             to   w-col-tbl-col-ctr
           else if   w-exe-cod-rgn        =    "VEN"
                     move  21             to   w-col-tbl-col-ctr
           else      move  01             to   w-col-tbl-col-ctr      .
      *                  *---------------------------------------------*
      *                  * Composizione                                *
      *                  *---------------------------------------------*
           move      240                  to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "<set name='"        to   w-all-str-cat (1)      .
           move      w-exe-des-rgn        to   w-all-str-cat (2)      .
           move      "' value='"          to   w-all-str-cat (3)      .
           move      v-edt                to   w-all-str-cat (4)      .
           move      "' color='"          to   w-all-str-cat (5)      .
           move      w-col-tbl-col-rgb
                    (w-col-tbl-col-ctr)   to   w-all-str-cat (6)      .
           move      "' />"               to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           move      w-all-str-alf        to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       cmp-xml-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-xml-rig-999.
       cmp-xml-rig-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [xml]                                 *
      *    *                                                           *
      *    * Piede                                                     *
      *    *-----------------------------------------------------------*
       cmp-xml-ftr-000.
      *              *-------------------------------------------------*
      *              * Chiusura grafico                                *
      *              *-------------------------------------------------*
           move      "</graph>"           to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
      *
           move      "*/"                 to   g-rec                  .
           perform   put-nxt-out-000      thru put-nxt-out-999        .
       cmp-xml-ftr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-xml-ftr-999.
       cmp-xml-ftr-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxr]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxr-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-gxr-cod    =    spaces
                     go to let-arc-gxr-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxr-cod    =    w-let-arc-gxr-exc
                     go to let-arc-gxr-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxr-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxr-cod    to   rf-gxr-cod-rgn         .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxr-400.
       let-arc-gxr-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxr-des-rgn       to   w-let-arc-gxr-des      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxr-900.
       let-arc-gxr-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxr-flg      .
           move      all   "."            to   w-let-arc-gxr-des      .
           go to     let-arc-gxr-900.
       let-arc-gxr-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      "nnn"                to   w-let-arc-gxr-cod      .
           move      spaces               to   w-let-arc-gxr-flg      .
           move      "ESTERO"             to   w-let-arc-gxr-des      .
       let-arc-gxr-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxr-cod    to   w-let-arc-gxr-exc      .
       let-arc-gxr-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxp]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxp-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    spaces
                     go to let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    w-let-arc-gxp-exc
                     go to let-arc-gxp-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxp-cod    to   rf-gxp-cod-prv         .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxp-400.
       let-arc-gxp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxp-des-prv       to   w-let-arc-gxp-des      .
           move      rf-gxp-cod-rgn       to   w-let-arc-gxp-rgn      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxp-900.
       let-arc-gxp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxp-flg      .
           move      all   "."            to   w-let-arc-gxp-des      .
           move      spaces               to   w-let-arc-gxp-rgn      .
           go to     let-arc-gxp-900.
       let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
           move      spaces               to   w-let-arc-gxp-des      .
           move      spaces               to   w-let-arc-gxp-rgn      .
       let-arc-gxp-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxp-cod    to   w-let-arc-gxp-exc      .
       let-arc-gxp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-cod    =    zero
                     go to let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-cod    =    w-let-arc-gxc-exc
                     go to let-arc-gxc-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cod    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxc-400.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxc-900.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           go to     let-arc-gxc-900.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
       let-arc-gxc-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxc-cod    to   w-let-arc-gxc-exc      .
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli] e [dcc]                 *
      *    *-----------------------------------------------------------*
       let-cli-dcc-000.
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    zero
                     go to let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Test se codice e dipendenza pari ai valori pre- *
      *              * cedenti : ad uscita                             *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    w-let-cli-dcc-exc and
                     w-let-cli-dcc-dpz    =    w-let-cli-dcc-exd
                     go to let-cli-dcc-990.
       let-cli-dcc-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-mne      .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-naz      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-cli-dcc-cmn      .
      *                  *---------------------------------------------*
      *                  * Codice regione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rgn      .
      *                  *---------------------------------------------*
      *                  * Codice provincia                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-prv      .
       let-cli-dcc-200.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [cli]                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione di :                         *
      *                  *   - ragione sociale                         *
      *                  *   - mnemonico                               *
      *                  *   - codice nazione                          *
      *                  *   - codice comune                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-cli-rag-soc to   w-let-cli-dcc-rag
                     move  rf-cli-cod-mne to   w-let-cli-dcc-mne
                     move  rf-cli-cod-naz to   w-let-cli-dcc-naz
                     move  rf-cli-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-300.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [dcc] principale            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione di :                         *
      *                  *   - ragione sociale                         *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-rag-soc to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codici :                     *
      *                  *  - Nazione                                  *
      *                  *  - Comune                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-cod-naz to   w-let-cli-dcc-naz
                     move  rf-dcc-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-400.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [dcc] per la dipendenza     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non c'e' dipendenza : nessuna azione     *
      *                  *---------------------------------------------*
           if        w-let-cli-dcc-dpz    =    spaces
                     go to let-cli-dcc-500.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-cli-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice :                     *
      *                  *  - Comune                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-500.
      *              *-------------------------------------------------*
      *              * A pre-uscita                                    *
      *              *-------------------------------------------------*
           go to     let-cli-dcc-900.
       let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-mne      .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-naz      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-cli-dcc-cmn      .
       let-cli-dcc-900.
      *              *-------------------------------------------------*
      *              * Pre-uscita                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice comune                       *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-cmn    to   w-let-arc-gxc-cod      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice provincia             *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-prv    to   w-let-cli-dcc-prv      .
      *                  *---------------------------------------------*
      *                  * Lettura codice provincia                    *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-prv    to   w-let-arc-gxp-cod      .
           perform   let-arc-gxp-000      thru let-arc-gxp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice regione               *
      *                  *---------------------------------------------*
           move      w-let-arc-gxp-rgn    to   w-let-cli-dcc-rgn      .
       let-cli-dcc-990.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice e dipendenza letti su valori prece-  *
      *                  * denti                                       *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-cli    to   w-let-cli-dcc-exc      .
           move      w-let-cli-dcc-dpz    to   w-let-cli-dcc-exd      .
       let-cli-dcc-999.
           exit.

      *    *===========================================================*
      *    * Open generica file sequenziale di output                  *
      *    *-----------------------------------------------------------*
       opn-seq-out-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname                           *
      *              *-------------------------------------------------*
           move      "/abd/web/htm/add_graph/001.wrk"
                                          to   f-xxx-pat              .
      *              *-------------------------------------------------*
      *              * Apertura del file in output                     *
      *              *-------------------------------------------------*
           move      "OO"                 to   g-ope                  .
           move      "seq "               to   g-nam                  .
           move      f-xxx-pat            to   g-pat                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       opn-seq-out-999.
           exit.

      *    *===========================================================*
      *    * Put next generica per output in sequenziale               *
      *    *-----------------------------------------------------------*
       put-nxt-out-000.
      *              *-------------------------------------------------*
      *              * Scrittura su sequenziale                        *
      *              *-------------------------------------------------*
           move      "PN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvout"
                                         using g                      .
       put-nxt-out-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [fit]       *
      *    *-----------------------------------------------------------*
       fil-fit-ing-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione 'GS' del modulo 'mfatfit0'    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero periodi di riferimento           *
      *                      *-----------------------------------------*
           move      01                   to   w-mod-fat-fit-npr      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data min                    *
      *                      *-----------------------------------------*
           move      w-exe-dat-min        to   w-mod-fat-fit-p1i      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data max                    *
      *                      *-----------------------------------------*
           move      w-exe-dat-max        to   w-mod-fat-fit-p1f      .
      *                      *-----------------------------------------*
      *                      * 2. periodo, data min                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-fat-fit-p2i      .
      *                      *-----------------------------------------*
      *                      * 2. periodo, data max                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-fat-fit-p2f      .
      *                      *-----------------------------------------*
      *                      * 3. periodo, data min                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-fat-fit-p3i      .
      *                      *-----------------------------------------*
      *                      * 3. periodo, data max                    *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-fat-fit-p3f      .
      *                  *---------------------------------------------*
      *                  * Chiamata modulo                             *
      *                  *---------------------------------------------*
           perform   mod-fat-fit-gts-000  thru mod-fat-fit-gts-999    .
       fil-fit-ing-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale archivio [fit]                        *
      *    *-----------------------------------------------------------*
       fil-fit-get-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione 'GT' del modulo 'mfatfit0'    *
      *              *-------------------------------------------------*
           perform   mod-fat-fit-get-000  thru mod-fat-fit-get-999    .
       fil-fit-get-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per richiamo modulo per la determinazione     *
      *    * della voce 'Fatturato' sulle statistiche di vendita sul   *
      *    * fatturato da Testata Documenti                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mfatfit0.mds"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

