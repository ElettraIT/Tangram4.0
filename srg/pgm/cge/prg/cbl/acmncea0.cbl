       Identification Division.
       Program-Id.                                 acmncea0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cea                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/10/03    *
      *                       Ultima revisione:    NdK del 02/03/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice cespite          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      *       Metodi di ricerca :                                      *
      *                                                                *
      *       '-'  seguito da return = Ricerca per descrizione         *
      *                                                                *
      *       '-'  seguito da return = Ricerca dicotomica              *
      *            e da 'find'                                         *
      *                                                                *
      *       '-'  seguito da valore = Ricerca per iniziale mnemonico  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cea-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-cea-cod : codice cespite       *
      *                                                                *
      *                       w-cod-mne-cea-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-cea-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-cea-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-cea-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cea-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cea-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cea-cod : codice cespite       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cea-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cea-cod : codice cespite       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-cea-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-cea-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-cea-cod : codice cespite       *
      *                                                                *
      *       -------------------------------------------------------- *
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
      *    * File Control [rlt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  rlt   assign  to disk     f-rlt-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is          w-rlt-krn
                             file status   is          f-rlt-sts      .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [rlt]                                    *
      *    *-----------------------------------------------------------*
       fd  rlt           label record standard                        .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  rlt-rec.
      *        *-------------------------------------------------------*
      *        * Area per anagrafica commerciale cespite [cea]         *
      *        *-------------------------------------------------------*
           05  rlt-rec-rec-cea.
               10  filler occurs 2048     pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per file relative di appoggio [rlt]             *
      *    *-----------------------------------------------------------*
       01  f-rlt.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-rlt-nam                  pic  x(04) value "rlt "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-rlt-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-rlt-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * File area addizionale per file relative di appoggio [rlt] *
      *    *-----------------------------------------------------------*
       01  w-rlt.
      *        *-------------------------------------------------------*
      *        * Key record number                                     *
      *        *-------------------------------------------------------*
           05  w-rlt-krn                  pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Numero records memorizzati                            *
      *        *-------------------------------------------------------*
           05  w-rlt-num                  pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Numero record attualmente in trattamento              *
      *        *-------------------------------------------------------*
           05  w-rlt-att                  pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Indici di comodo 01..10                               *
      *        *-------------------------------------------------------*
           05  w-rlt-i01                  pic  9(03)                  .
           05  w-rlt-i02                  pic  9(03)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cea]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcea"                          .
      *        *-------------------------------------------------------*
      *        * [zc1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzc1"                          .

      *    *===========================================================*
      *    * Work per Let su archivio [zc1]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczc10.ltw"                   .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux-mne-cea.
           05  w-aux-mne-cea-c01          pic  9(03)                  .
           05  w-aux-mne-cea-c02          pic  9(03)                  .
           05  w-aux-mne-cea-c03          pic  9(03)                  .
           05  w-aux-mne-cea-c04          pic  9(03)                  .
           05  w-aux-mne-cea-c05          pic  9(03)                  .
           05  w-aux-mne-cea-c09          pic  9(03)                  .
           05  w-aux-mne-cea-nli          pic  9(03)                  .
           05  w-aux-mne-cea-m10          pic  x(10)                  .
           05  w-aux-mne-cea-mmi          pic  x(10)                  .
           05  w-aux-mne-cea-mma          pic  x(10)                  .
           05  w-aux-mne-cea-alf          pic  x(10)                  .
           05  w-aux-mne-cea-alf-r  redefines
               w-aux-mne-cea-alf.
               10  w-aux-mne-cea-al1      pic  x(01)                  .
               10  w-aux-mne-cea-al2      pic  x(01)                  .
               10  w-aux-mne-cea-al8      pic  x(08)                  .
           05  w-aux-mne-cea-alf-rr redefines
               w-aux-mne-cea-alf.
               10  w-aux-mne-cea-ar1      pic  x(01)                  .
               10  w-aux-mne-cea-ar9      pic  x(09)                  .
           05  w-aux-mne-cea-tpf          pic  x(01)                  .
           05  w-aux-mne-cea-crb          pic  9(03)                  .
           05  w-aux-mne-cea-cpb          pic  9(03)                  .
           05  w-aux-mne-cea-cpa          pic  9(03)                  .
           05  w-aux-mne-cea-buf
                               occurs 120.
               10  w-aux-mne-cea-cbu      pic  9(07)                  .
               10  w-aux-mne-cea-mbu      pic  x(10)                  .
               10  w-aux-mne-cea-dbu.
                   20  w-aux-mne-cea-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
               10  w-aux-mne-cea-tbu      pic  x(40)                  .
               10  w-aux-mne-cea-lbu      pic  x(40)                  .
           05  w-aux-mne-cea-edp.
               10  w-aux-mne-cea-ep1      pic  x(03)                  .
               10  w-aux-mne-cea-ep2      pic  x(03)                  .
           05  w-aux-mne-cea-ltp.
               10  filler                 pic  x(17)                  .
           05  w-aux-mne-cea-dup          pic  x(40)                  .
           05  w-aux-mne-cea-r01 redefines
               w-aux-mne-cea-dup.
               10  w-aux-mne-cea-p00      pic  9(11)                  .
               10  w-aux-mne-cea-p99      pic  x(29)                  .
           05  w-aux-mne-cea-r02 redefines
               w-aux-mne-cea-dup.
               10  w-aux-mne-cea-f00.
                   15  w-aux-mne-cea-f01  pic  x(03)                  .
                   15  w-aux-mne-cea-f02  pic  x(03)                  .
                   15  w-aux-mne-cea-f03  pic  9(02)                  .
                   15  w-aux-mne-cea-f04  pic  x(01)                  .
                   15  w-aux-mne-cea-f05  pic  9(02)                  .
                   15  w-aux-mne-cea-f06  pic  x(01)                  .
                   15  w-aux-mne-cea-f07  pic  9(03)                  .
                   15  w-aux-mne-cea-f08  pic  x(01)                  .
               10  w-aux-mne-cea-f99      pic  x(24)                  .
           05  w-aux-mne-cea-r03 redefines
               w-aux-mne-cea-dup.
               10  w-aux-mne-cea-e00.
                   15  w-aux-mne-cea-e01  pic  x(02)                  .
                   15  w-aux-mne-cea-e02  pic  x(01)                  .
                   15  w-aux-mne-cea-e03  pic  9(08)                  .
                   15  w-aux-mne-cea-e04  pic  x(05)                  .
               10  w-aux-mne-cea-e99      pic  x(24)                  .
           05  w-aux-mne-cea-dmx.
               10  w-aux-mne-cea-rch
                               occurs 40  pic  x(01)                  .
           05  w-aux-mne-cea-fes          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutine di ricerca totale                     *
      *    *-----------------------------------------------------------*
       01  w-rcr-tot-cea.
      *        *-------------------------------------------------------*
      *        * Valore della personalizzazione sul tipo di ricerca    *
      *        * totale da eseguire                                    *
      *        *  - A : Solo su anagrafica cespite                     *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-pf1          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore da ricercare, normalizzato                     *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-vnr          pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records esaminati                           *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-cre          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Flag di fine ricerca                                  *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-ffr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di selezione effettuata                          *
      *        *  - Spaces : No                                        *
      *        *  - S      : Si                                        *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-fsl          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records selezionati                         *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-crs          pic  9(08)                  .
      *        *-------------------------------------------------------*
      *        * Indice di match                                       *
      *        *  - 00 : No match                                      *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-mch          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per test su resto pari a 10 o 100                *
      *        *-------------------------------------------------------*
           05  w-rcr-tot-cea-wr1          pic  9(01)                  .
           05  w-rcr-tot-cea-wr2          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice cespite                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmncea0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-cea
                                               v
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-cod-mne-cea-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-cea-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-cea-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-cea-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-cea-ope    =    "A+" or
                     w-cod-mne-cea-ope    =    "I+" or
                     w-cod-mne-cea-ope    =    "F+"
                     perform   aco-000    thru aco-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' la prima Open per il modulo si *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    1
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
       opn-200.
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione relativa al tipo  *
      *                  * di ricerca totale                           *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[trt-pf1]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-rcr-tot-cea-pf1
           else      move  "A"            to   w-rcr-tot-cea-pf1      .
           if        w-rcr-tot-cea-pf1    not  = "T"
                     move  "A"            to   w-rcr-tot-cea-pf1      .
       opn-300.
      *                  *---------------------------------------------*
      *                  * Open files                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Open file [cea]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                      *-----------------------------------------*
      *                      * Open file [zc1]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzc1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zc1                 .
      *                      *-----------------------------------------*
      *                      * Open file relative di appoggio [rlt]    *
      *                      *-----------------------------------------*
           perform   rlt-opn-000          thru rlt-opn-999            .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' l'ultima Close per il modulo   *
      *              * si esce                                         *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    zero
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
       cls-300.
      *                  *---------------------------------------------*
      *                  * Close files                                 *
      *                  *---------------------------------------------*
       cls-310.
      *                      *-----------------------------------------*
      *                      * Close file [cea]                        *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                      *-----------------------------------------*
      *                      * Close file [zc1]                        *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzc1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zc1                 .
      *                      *-----------------------------------------*
      *                      * Close file relative di appoggio [rlt]   *
      *                      *-----------------------------------------*
           perform   rlt-cls-000          thru rlt-cls-999            .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle Open in corso per il mo-  *
      *              * dulo e' pari a zero si dichiara che e' cancel-  *
      *              * labile, altrimento che non lo e'                *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-cod-mne-cea-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge6010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge6000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-200.
           move      "P?"                 to   s-ope                  .
           move      "pcge6000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-200.
           if        v-pfk (04)           =    "INSR"
                     move  spaces         to   v-pfk (04)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-cod-mne-cea-cod    to   w-cod-mne-cea-num      .
           move      v-edm                to   w-cod-mne-cea-edm      .
           move      v-ufk                to   w-cod-mne-cea-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-cea-edm    to   v-edm                  .
           move      w-cod-mne-cea-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-cea-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cea-ufk    to   v-ufk                  .
           move      w-cod-mne-cea-lin    to   v-lin                  .
           move      w-cod-mne-cea-pos    to   v-pos                  .
           move      w-cod-mne-cea-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-cea-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-cea-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find o Insr                  *
      *              *-------------------------------------------------*
           if        w-cod-mne-cea-ope    =    "F+" or 
                     w-cod-mne-cea-ope    =    "I+"
                     go to aco-100.
       aco-025.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cea-ufk    to   v-ufk                  .
           move      w-cod-mne-cea-lin    to   v-lin                  .
           move      w-cod-mne-cea-pos    to   v-pos                  .
           move      w-cod-mne-cea-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-cea-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-cea-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-cea-alf      .
      *              *-------------------------------------------------*
      *              * Test se Exit o Delt                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-175.
       aco-050.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione campo editato                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-cea-lin    to   v-lin                  .
           move      w-cod-mne-cea-pos    to   v-pos                  .
           move      w-cod-mne-cea-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-cea-num    to   w-cod-mne-cea-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-cea-dln      .
           move      zero                 to   w-cod-mne-cea-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find o da Insr                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-csp"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata                 *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Se selezione effettuata                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio valore numerico             *
      *                      *-----------------------------------------*
           move      s-num                to   w-cod-mne-cea-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-cea-edm    to   v-edm                  .
           move      w-cod-mne-cea-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-cea-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-cea-ope      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore numerico editato *
      *                      * e preparazione valore in uscita         *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-125.
      *              *-------------------------------------------------*
      *              * Preparazione uscita per Find precablato         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : esecuzione Find         *
      *                  *---------------------------------------------*
           move      "F+"                 to   w-cod-mne-cea-ope      .
      *                  *---------------------------------------------*
      *                  * Simulazione tasto Find                      *
      *                  *---------------------------------------------*
           move      "FIND"               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-175.
      *              *-------------------------------------------------*
      *              * Test se blanks embedded                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-cea-alf    =    spaces
                     move  zero           to   w-cod-mne-cea-num
                     go to aco-075.
           if        w-cod-mne-cea-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-cea-c01      .
       aco-176.
           add       1                    to   w-aux-mne-cea-c01      .
           if        w-aux-mne-cea-c01    >    10
                     go to aco-180.
           if        w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-cea-c01      .
           if        w-aux-mne-cea-c01    >    10
                     go to aco-180.
           if        w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-cea-num      .
           move      zero                 to   w-aux-mne-cea-c01      .
       aco-181.
           add       1                    to   w-aux-mne-cea-c01      .
           if        w-aux-mne-cea-c01    >    10
                     go to aco-200.
           if        w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   <    "0" or
                     w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   >    "9"
                     go to aco-210.
           multiply  10                   by   w-cod-mne-cea-num      .
           move      w-cod-mne-cea-cha
                    (w-aux-mne-cea-c01)   to   w-cod-mne-cea-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-mne-cea-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-cea-edm    to   v-edm                  .
           move      w-cod-mne-cea-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-cea-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-cea-alf
                     go to aco-050.
       aco-210.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * In base al contenuto del campo si desume    *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Campo alfanumerico accettato in comodo  *
      *                      * di trattamento                          *
      *                      *-----------------------------------------*
           move      w-cod-mne-cea-alf    to   w-aux-mne-cea-alf      .
      *                      *-----------------------------------------*
      *                      * Se il primo carattere e' '-' e tutto il *
      *                      * resto a spazi : ricerca per ragione so- *
      *                      * ciale commerciale                       *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-al1    =    "-"    and
                     w-aux-mne-cea-al2    =    spaces and
                     w-aux-mne-cea-al8    =    spaces and
                     w-cod-mne-cea-dln    not  = zero
                     go to aco-500.
      *                      *-----------------------------------------*
      *                      * Se il primo ed il secondo carattere     *
      *                      * sono '-' ed il resto e' a spazi : ri-   *
      *                      * cerca per ragione sociale contabile     *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-al1    =    "-"    and
                     w-aux-mne-cea-al2    =    "-"    and
                     w-aux-mne-cea-al8    =    spaces and
                     w-cod-mne-cea-dln    not  = zero
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * Se il primo carattere e' '-' ed il res- *
      *                      * to e' diverso da spazi : ricerca per    *
      *                      * mnemonico multipla                      *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-al1    =    "-"  and
                     w-aux-mne-cea-ar9    not  = spaces
                     go to aco-224.
       aco-220.
      *                      *-----------------------------------------*
      *                      * Preparazione min-max per ricerca per    *
      *                      * mnemonico singola                       *
      *                      *-----------------------------------------*
           move      w-cod-mne-cea-alf    to   w-aux-mne-cea-mmi      .
           move      w-cod-mne-cea-alf    to   w-aux-mne-cea-mma      .
      *                          *-------------------------------------*
      *                          * A ricerca per mnemonico             *
      *                          *-------------------------------------*
           go to     aco-230.
       aco-224.
      *                      *-----------------------------------------*
      *                      * Preparazione min-max per ricerca per    *
      *                      * mnemonico multipla                      *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-ar9    to   w-aux-mne-cea-mmi      .
           move      w-aux-mne-cea-mmi    to   w-aux-mne-cea-mma      .
           move      w-aux-mne-cea-mma    to   w-all-str-alf          .
           move      10                   to   w-all-str-lun          .
           perform   all-str-pad-000      thru all-str-pad-999        .
           move      w-all-str-alf        to   w-aux-mne-cea-mma      .
      *                          *-------------------------------------*
      *                          * A ricerca per mnemonico             *
      *                          *-------------------------------------*
           go to     aco-230.
       aco-230.
      *                  *=============================================*
      *                  * Ricerca per mnemonico                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-cea-tpf      .
           move      zero                 to   w-aux-mne-cea-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-mne-cea-mmi    to   rf-cea-cod-mne         .
           move      zero                 to   rf-cea-cod-csp         .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-240.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per mnemonico       *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 10 *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-cea-cod-mne       to   w-aux-mne-cea-m10      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-m10    >    w-aux-mne-cea-mma
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Selezione sul record                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-mne-cea-fes      .
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-cea-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di 120 records letti con   *
      *                      * lo stesso mnemonico                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-crb    >    120
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-cea-cod-csp       to   w-aux-mne-cea-cbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-cod-mne       to   w-aux-mne-cea-mbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-des-csp       to   w-aux-mne-cea-dbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-cla-csp       to   w-let-arc-zc1-cod      .
           perform   let-arc-zc1-000      thru let-arc-zc1-999        .
           move      w-let-arc-zc1-des    to   w-aux-mne-cea-tbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-mtr-csp       to   w-aux-mne-cea-lbu
                                              (w-aux-mne-cea-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-240.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-cea-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-cea-crb    =    1
                     go to aco-300
           else      go to aco-325.
       aco-275.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il mnemonico impostato *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del segnale di record   *
      *                      * non trovato                             *
      *                      *-----------------------------------------*
           if        w-cod-mne-cea-dln    =    zero
                     go to aco-276.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-276.
       aco-278.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-025.
       aco-300.
      *                  *---------------------------------------------*
      *                  * Se un record con il mnemonico impostato     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se in ricerca totale, ma non effettuata *
      *                      * la selezione : al box di scelta         *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-tpf    =    "T"    and
                     w-rcr-tot-cea-fsl    =    spaces
                     go to aco-325.
      *                      *-----------------------------------------*
      *                      * Codice numerico in valore numerico      *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-cbu (1)
                                          to   w-cod-mne-cea-num      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione ed uscita             *
      *                      *-----------------------------------------*
           go to     aco-200.
       aco-325.
      *                  *---------------------------------------------*
      *                  * Se non piu' di centoventi records con lo    *
      *                  * stesso valore                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-crb    to   w-aux-mne-cea-cpb      .
           subtract  1                    from w-aux-mne-cea-cpb      .
           divide    6                    into w-aux-mne-cea-cpb      .
           add       1                    to   w-aux-mne-cea-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-cea-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      " Selezionare il cespite desiderato "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Codice cespite   :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Descrizione      :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Tipo cespite     :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Matricola        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Mnemonico        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   aco-950              thru aco-959                .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-350.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-c01    to   w-aux-mne-cea-nli      .
       aco-355.
           if        w-aux-mne-cea-nli    >    6
                     subtract  6          from w-aux-mne-cea-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-cea-nli      .
       aco-360.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-cea-cbu
                    (w-aux-mne-cea-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-cea-dbu
                    (w-aux-mne-cea-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-cea-tbu
                    (w-aux-mne-cea-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-cea-lbu
                    (w-aux-mne-cea-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-cea-mbu
                    (w-aux-mne-cea-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-cea-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-cea-c01    <    w-aux-mne-cea-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-cea-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-cea-cpa    <    w-aux-mne-cea-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-cea-nli    to   v-lin                  .
           move      26                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-380.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-382
           else if   v-key                =    "UP  "
                     go to aco-384
           else if   v-key                =    "DOWN"
                     go to aco-386
           else if   v-key                =    "EXIT"
                     go to aco-398
           else if   v-key                =    "NXSC"
                     go to aco-392
           else if   v-key                =    "PRSC"
                     go to aco-394
           else      go to aco-375.
       aco-382.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      w-aux-mne-cea-cbu
                    (w-aux-mne-cea-c01)   to   w-cod-mne-cea-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-cea-c01      .
           if        w-aux-mne-cea-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-cea-c01    <    w-aux-mne-cea-crb
                     add   1              to   w-aux-mne-cea-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-cea-nli    =    12
                     go to aco-390
           else      go to aco-350.
       aco-390.
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-cea-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-cea-cpa      .
       aco-396.
           move      w-aux-mne-cea-cpa    to   w-aux-mne-cea-c01      .
           multiply  6                    by   w-aux-mne-cea-c01      .
           subtract  5                    from w-aux-mne-cea-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-cea-tpf    not  = "M"
                     go to aco-510
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di centoventi record con lo stesso  *
      *                  * mnemonico impostato                         *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge6010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-025.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "M"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-mne"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-aux-mne-cea-mmi    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-500.
      *              *=================================================*
      *              * Se ricerca per ragione sociale commerciale      *
      *              *-------------------------------------------------*
       aco-502.
      *                  *---------------------------------------------*
      *                  * Spaces in ragione sociale di comodo         *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-cea-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-510.
      *                  *---------------------------------------------*
      *                  * Accettazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      w-aux-mne-cea-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-cea-dup      .
       aco-512.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-514.
      *                      *-----------------------------------------*
      *                      * Visualizzazione ragione sociale a spa-  *
      *                      * ces                                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Rientro ad accettazione codice          *
      *                      *-----------------------------------------*
           go to     aco-025.
       aco-514.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-516.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-075.
       aco-516.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
       aco-517.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-700.
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione ragione sociale in upper-  *
      *                      * case                                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-dup    =    spaces
                     go to aco-510.
       aco-520.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-dup    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-rcr-tot-cea-vnr      .
      *                      *-----------------------------------------*
      *                      * Se il valore normalizzato e' a spaces : *
      *                      * rientro ad accettazione ragione sociale *
      *                      * in uppercase                            *
      *                      *-----------------------------------------*
           if        w-rcr-tot-cea-vnr    =    spaces
                     go to aco-510.
       aco-530.
      *                      *-----------------------------------------*
      *                      * Esecuzione ricerca totale               *
      *                      *-----------------------------------------*
       aco-532.
      *                          *-------------------------------------*
      *                          * Tipo di ricerca                     *
      *                          *-------------------------------------*
           move      "T"                  to   w-aux-mne-cea-tpf      .
       aco-535.
      *                          *-------------------------------------*
      *                          * Salvataggio immagine video          *
      *                          *-------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box superiore                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      06                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "                        Ricerca cespiti in esecuzi
      -              "one                         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box inferiore                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      77                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Nr. cespiti esaminati ......:             Cespiti 
      -              "selezionati.....:          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Literal nel box                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      77                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "           Per interrompere la ricerca premere il 
      -              "tasto di uscita            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box centrale, parte sinistra        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      44                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Box centrale, parte destra          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Box vuoto                       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-540.
      *                          *-------------------------------------*
      *                          * Numero records memorizzati in file  *
      *                          * relative di appoggio [rlt] : zero   *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-num              .
      *                          *-------------------------------------*
      *                          * Numero record attualmente in trat-  *
      *                          * tamento del file relative di appog- *
      *                          * gio [rlt] : zero                    *
      *                          *-------------------------------------*
           move      zero                 to   w-rlt-att              .
      *                          *-------------------------------------*
      *                          * Flag di fine ricerca : No           *
      *                          *-------------------------------------*
           move      spaces               to   w-rcr-tot-cea-ffr      .
      *                          *-------------------------------------*
      *                          * Flag di selezione effettuata : No   *
      *                          *-------------------------------------*
           move      spaces               to   w-rcr-tot-cea-fsl      .
      *                          *-------------------------------------*
      *                          * Contatore records esaminati : a ze- *
      *                          * ro                                  *
      *                          *-------------------------------------*
           move      zero                 to   w-rcr-tot-cea-cre      .
      *                          *-------------------------------------*
      *                          * Contatore records selezionati : a   *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-rcr-tot-cea-crs      .
      *                          *-------------------------------------*
      *                          * Contatore records nel buffer : a    *
      *                          * zero                                *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-mne-cea-crb      .
       aco-550.
      *                          *-------------------------------------*
      *                          * Start su [cea] per descriaione      *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      spaces               to   rf-cea-des-key         .
           move      zero                 to   rf-cea-cod-csp         .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : a fine trattamen- *
      *                          * to                                  *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-660.
       aco-555.
      *                          *-------------------------------------*
      *                          * Test se l'utente ha premuto uno dei *
      *                          * seguenti tasti :                    *
      *                          * - Exit : sempre ammesso             *
      *                          * - Slct o                            *
      *                          *   Rtrn : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt]          *
      *                          * - Up   o                            *
      *                          *   Prsc : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt], che non *
      *                          *          e' il primo                *
      *                          * - Down o                            *
      *                          *   Nxsc : solo se in questo momento  *
      *                          *          c'e' un record in tratta-  *
      *                          *          mento del file relative    *
      *                          *          di appoggio [rlt], che non *
      *                          *          e' l'ultimo                *
      *                          *-------------------------------------*
           move      "AA"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      08                   to   v-pos                  .
           if        w-rlt-att            >    1
                     move  "UP  "         to   v-pfk (01)
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-rlt-att            <    w-rlt-num
                     move  "DOWN"         to   v-pfk (02)
                     move  "NXSC"         to   v-pfk (08)             .
           if        w-rlt-att            not  = zero
                     move  "SLCT"         to   v-pfk (10)
                     move  "RTRN"         to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-key                not  = spaces
                     go to aco-670.
       aco-560.
      *                          *-------------------------------------*
      *                          * Se flag di fine ricerca in On : in- *
      *                          * vece di eseguire la lettura si ri-  *
      *                          * cicla per vedere quale tasto fun-   *
      *                          * zione digita l'utente               *
      *                          *-------------------------------------*
           if        w-rcr-tot-cea-ffr    =    "S"
                     go to aco-555.
      *                          *-------------------------------------*
      *                          * Read Next su [cea] per descrizione  *
      *                          * ciale                               *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                          *-------------------------------------*
      *                          * Se At End : a fine trattamento      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-660.
       aco-565.
       aco-570.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records esaminati            *
      *                          *-------------------------------------*
       aco-571.
           add       1                    to   w-rcr-tot-cea-cre      .
       aco-572.
           if        w-rcr-tot-cea-cre    not  < 100
                     move  w-rcr-tot-cea-cre
                                          to   w-rcr-tot-cea-wr2
                     if    w-rcr-tot-cea-wr2
                                          =    zero
                           go to aco-573
                     else  go to aco-575.
           if        w-rcr-tot-cea-cre    not  < 10
                     move  w-rcr-tot-cea-cre
                                          to   w-rcr-tot-cea-wr1
                     if    w-rcr-tot-cea-wr1
                                          =    zero
                           go to aco-573
                     else  go to aco-575.
       aco-573.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-rcr-tot-cea-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-575.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * descrizione                         *
      *                          *-------------------------------------*
      
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-cea-des-csp       =    spaces
                     go to aco-580.
      *                              *---------------------------------*
      *                              * Test sulle 10 linee bufferizza- *
      *                              * te                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Indice 01..10 : a zero      *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-577.
      *                                  *-----------------------------*
      *                                  * Incremento indice 01..10    *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-i01              .
      *                                  *-----------------------------*
      *                                  * Se oltre il massimo : no    *
      *                                  * bufferizzazione             *
      *                                  *-----------------------------*
           if        w-rlt-i01            >    10
                     go to aco-580.
      *                                  *-----------------------------*
      *                                  * Se linea a spaces : no buf- *
      *                                  * ferizzazione                *
      *                                  *-----------------------------*
           if        rf-cea-des-rig
                    (w-rlt-i01)           =    spaces
                     go to aco-580.
      *                                  *-----------------------------*
      *                                  * Normalizzazione A..Z - 0..9 *
      *                                  * e preparazione 2. valore    *
      *                                  * per il match                *
      *                                  *-----------------------------*
           move      rf-cea-des-rig
                    (w-rlt-i01)           to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                                  *-----------------------------*
      *                                  * Preparazione 1. valore per  *
      *                                  * il match                    *
      *                                  *-----------------------------*
           move      w-rcr-tot-cea-vnr    to   w-all-str-cat (1)      .
      *                                  *-----------------------------*
      *                                  * Match tra i due valori      *
      *                                  *-----------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                                  *-----------------------------*
      *                                  * Se c'e' stato un match : a  *
      *                                  * bufferizzazione con indice  *
      *                                  * match a 11                  *
      *                                  *-----------------------------*
           if        w-all-str-flg        =    spaces
                     move  01             to   w-rcr-tot-cea-mch
                     go to aco-625.
      *                                  *-----------------------------*
      *                                  * Altrimenti riciclo su linea *
      *                                  * successiva                  *
      *                                  *-----------------------------*
           go to     aco-577.
       aco-580.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per la    *
      *                          * matricola                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-cea-mtr-csp       =    spaces
                     go to aco-585.
      *                              *---------------------------------*
      *                              * Normalizzazione A..Z - 0..9, e  *
      *                              * preparazione 2. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      rf-cea-mtr-csp       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                              *---------------------------------*
      *                              * Preparazione 1. valore per il   *
      *                              * match                           *
      *                              *---------------------------------*
           move      w-rcr-tot-cea-vnr    to   w-all-str-cat (1)      .
      *                              *---------------------------------*
      *                              * Match tra i due valori          *
      *                              *---------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                              *---------------------------------*
      *                              * Se c'e' stato un match : a buf- *
      *                              * ferizzazione con indice match a *
      *                              * 11                              *
      *                              *---------------------------------*
           if        w-all-str-flg        =    spaces
                     move  11             to   w-rcr-tot-cea-mch
                     go to aco-625.
       aco-585.
      *                          *-------------------------------------*
      *                          * Eventuale bufferizzazione per note  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test che non sia a Spaces       *
      *                              *---------------------------------*
           if        rf-cea-ann-csp       =    spaces
                     go to aco-600.
      *                              *---------------------------------*
      *                              * Test sulle 10 linee bufferizza- *
      *                              * te                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Indice 01..10 : a zero      *
      *                                  *-----------------------------*
           move      zero                 to   w-rlt-i01              .
       aco-590.
      *                                  *-----------------------------*
      *                                  * Incremento indice 01..10    *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-i01              .
      *                                  *-----------------------------*
      *                                  * Se oltre il massimo : no    *
      *                                  * bufferizzazione             *
      *                                  *-----------------------------*
           if        w-rlt-i01            >    10
                     go to aco-600.
      *                                  *-----------------------------*
      *                                  * Se linea a spaces : no buf- *
      *                                  * ferizzazione                *
      *                                  *-----------------------------*
           if        rf-cea-ann-rig
                    (w-rlt-i01)           =    spaces
                     go to aco-600.
      *                                  *-----------------------------*
      *                                  * Normalizzazione A..Z - 0..9 *
      *                                  * e preparazione 2. valore    *
      *                                  * per il match                *
      *                                  *-----------------------------*
           move      rf-cea-ann-rig
                    (w-rlt-i01)           to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                                  *-----------------------------*
      *                                  * Preparazione 1. valore per  *
      *                                  * il match                    *
      *                                  *-----------------------------*
           move      w-rcr-tot-cea-vnr    to   w-all-str-cat (1)      .
      *                                  *-----------------------------*
      *                                  * Match tra i due valori      *
      *                                  *-----------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                                  *-----------------------------*
      *                                  * Se c'e' stato un match : a  *
      *                                  * bufferizzazione con indice  *
      *                                  * match a 11                  *
      *                                  *-----------------------------*
           if        w-all-str-flg        =    spaces
                     move  11             to   w-rcr-tot-cea-mch
                     go to aco-625.
      *                                  *-----------------------------*
      *                                  * Altrimenti riciclo su linea *
      *                                  * successiva                  *
      *                                  *-----------------------------*
           go to     aco-590.
       aco-600.
       aco-620.
      *                          *-------------------------------------*
      *                          * Se non c'e' stato alcun match : si  *
      *                          * ricicla su anagrafica cespite com-  *
      *                          * merciale successiva                 *
      *                          *-------------------------------------*
           go to     aco-555.
       aco-625.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record              *
      *                          *-------------------------------------*
       aco-630.
      *                              *---------------------------------*
      *                              * Visualizzazione, se non gia' e- *
      *                              * seguita, del numero records se- *
      *                              * lezionati                       *
      *                              *---------------------------------*
       aco-631.
           if        w-rcr-tot-cea-cre    not  < 100
                     move  w-rcr-tot-cea-cre
                                          to   w-rcr-tot-cea-wr2
                     if    w-rcr-tot-cea-wr2
                                          =    zero
                           go to aco-633
                     else  go to aco-632.
           if        w-rcr-tot-cea-cre    not  < 10
                     move  w-rcr-tot-cea-cre
                                          to   w-rcr-tot-cea-wr1
                     if    w-rcr-tot-cea-wr1
                                          =    zero
                           go to aco-633
                     else  go to aco-632.
           go to     aco-633.
       aco-632.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-rcr-tot-cea-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-633.
      *                          *-------------------------------------*
      *                          * Incremento e visualizzazione del    *
      *                          * numero records selezionati          *
      *                          *-------------------------------------*
           add       1                    to   w-rcr-tot-cea-crs      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-rcr-tot-cea-crs    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-635.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer, a meno di non aver raggiunto   *
      *                          * il massimo                          *
      *                          *-------------------------------------*
           if        w-aux-mne-cea-crb    not  < 120
                     go to aco-640.
           add       1                    to   w-aux-mne-cea-crb      .
       aco-640.
      *                          *-------------------------------------*
      *                          * Bufferizzazione vera e propria, con *
      *                          * indice di tipo match, a meno di non *
      *                          * aver raggiunto il massimo           *
      *                          *-------------------------------------*
           if        w-aux-mne-cea-crb    not  < 120
                     go to aco-645.
           move      rf-cea-cod-csp       to   w-aux-mne-cea-cbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-cod-mne       to   w-aux-mne-cea-mbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-des-csp       to   w-aux-mne-cea-dbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-cla-csp       to   w-let-arc-zc1-cod      .
           perform   let-arc-zc1-000      thru let-arc-zc1-999        .
           move      w-let-arc-zc1-des    to   w-aux-mne-cea-tbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-mtr-csp       to   w-aux-mne-cea-lbu
                                              (w-aux-mne-cea-crb)     .
       aco-645.
      *                          *-------------------------------------*
      *                          * Incremento numero di records memo-  *
      *                          * rizzati nel file relative di appog- *
      *                          * gio [rlt]                           *
      *                          *-------------------------------------*
           add       1                    to   w-rlt-num              .
      *                          *-------------------------------------*
      *                          * Scrittura file relative di appoggio *
      *                          * [rlt]                               *
      *                          *-------------------------------------*
           move      w-rlt-num            to   w-rlt-krn              .
           move      spaces               to   rlt-rec                .
           move      rf-cea               to   rlt-rec-rec-cea        .
           perform   rlt-put-000          thru rlt-put-999            .
      *                          *-------------------------------------*
      *                          * Se e' il primo record scritto, lo   *
      *                          * si visualizza                       *
      *                          *-------------------------------------*
           if        w-rlt-num            >    1
                     go to aco-650.
           move      1                    to   w-rlt-att              .
           perform   aco-680              thru aco-689                .
       aco-650.
      *                          *-------------------------------------*
      *                          * Riciclo a lettura anagrafica com-   *
      *                          * merciale successiva                 *
      *                          *-------------------------------------*
           go to     aco-555.
       aco-660.
      *                          *-------------------------------------*
      *                          * Fine lettura                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione del numero re-  *
      *                              * cords esaminati                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-rcr-tot-cea-cre    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Flag di fine ricerca : Si       *
      *                              *---------------------------------*
           move      "S"                  to   w-rcr-tot-cea-ffr      .
      *                              *---------------------------------*
      *                              * Literal nel box inferiore per   *
      *                              * fine ricerca                    *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "                                 FINE RICERCA     
      -              "                            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * funzione per dar modo all'ope-  *
      *                              * ratore di effettuare una scel-  *
      *                              * ta ben precisa                  *
      *                              *---------------------------------*
           go to     aco-555.
       aco-665.
      *                          *-------------------------------------*
      *                          * Fine ricerca totale                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione function key    *
      *                              *---------------------------------*
           move      spaces               to   v-key                  .
      *                              *---------------------------------*
      *                              * Ripristino immagine video       *
      *                              *---------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * A controllo su numero records   *
      *                              * trovati                         *
      *                              *---------------------------------*
           go to     aco-250.
       aco-670.
      *                          *-------------------------------------*
      *                          * Trattamento tasti funzione asin-    *
      *                          * croni                               *
      *                          *-------------------------------------*
       aco-671.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tasto  *
      *                              * funzione premuto                *
      *                              *---------------------------------*
           if        v-key                =    "UP  " or
                     v-key                =    "PRSC"
                     go to aco-672
           else if   v-key                =    "DOWN" or
                     v-key                =    "NXSC"
                     go to aco-673
           else if   v-key                =    "SLCT" or
                     v-key                =    "RTRN"
                     go to aco-674
           else if   v-key                =    "EXIT"
                     go to aco-675
           else      go to aco-560.
       aco-672.
      *                              *---------------------------------*
      *                              * Se Up o Prsc                    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero record    *
      *                                  * del file relative di ap-    *
      *                                  * poggio [rlt] attualmente    *
      *                                  * in trattamento              *
      *                                  *-----------------------------*
           subtract  1                    from   w-rlt-att            .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-cea      to   rf-cea                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-680              thru aco-689                .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-560.
       aco-673.
      *                              *---------------------------------*
      *                              * Se Down o Nxsc                  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero record    *
      *                                  * del file relative di ap-    *
      *                                  * poggio [rlt] attualmente    *
      *                                  * in trattamento              *
      *                                  *-----------------------------*
           add       1                    to   w-rlt-att              .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-cea      to   rf-cea                 .
      *                                  *-----------------------------*
      *                                  * Visualizzazione record del  *
      *                                  * file relative di appoggio   *
      *                                  * [rlt] attualmente in trat-  *
      *                                  * tamento                     *
      *                                  *-----------------------------*
           perform   aco-680              thru aco-689                .
      *                                  *-----------------------------*
      *                                  * Continuazione lettura       *
      *                                  *-----------------------------*
           go to     aco-560.
       aco-674.
      *                              *---------------------------------*
      *                              * Se Slct o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Flag di selezione effettua- *
      *                                  * ta : Si                     *
      *                                  *-----------------------------*
           move      "S"                  to   w-rcr-tot-cea-fsl      .
      *                                  *-----------------------------*
      *                                  * Lettura record del file re- *
      *                                  * lative di appoggio [rlt]    *
      *                                  *-----------------------------*
           move      w-rlt-att            to   w-rlt-krn              .
           perform   rlt-get-000          thru rlt-get-999            .
           move      rlt-rec-rec-cea      to   rf-cea                 .
      *                                  *-----------------------------*
      *                                  * Si esegue una forzatura co- *
      *                                  * me se si fosse rintracciato *
      *                                  * un solo record, pari al re- *
      *                                  * cord selezionato            *
      *                                  *-----------------------------*
           move      1                    to   w-aux-mne-cea-crb      .
           move      rf-cea-cod-csp       to   w-aux-mne-cea-cbu (1)  .
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-665.
       aco-675.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * A fine ricerca totale       *
      *                                  *-----------------------------*
           go to     aco-665.
       aco-680.
      *                          *-------------------------------------*
      *                          * Visualizzazione record del file     *
      *                          * relative di appoggio [rlt] attual-  *
      *                          * mente in trattamento                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Titolo                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero record, literal      *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Nr:"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Numero record, valore       *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      05                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      w-rlt-att            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Anagrafica [cea]                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Codice cespite              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      07                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-cod-csp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 1.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (01)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 2.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (02)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 3.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (03)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 4.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (04)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 5.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (05)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 6.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (06)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 7.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (07)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Descrizione 8.              *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-cea-des-rig (08)  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-689.
           exit.
       aco-700.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione codice                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-dup    =    spaces or
                     w-aux-mne-cea-dup    =    "*"
                     go to aco-025.
       aco-705.
      *                  *---------------------------------------------*
      *                  * Determinazione se impostazione pari a :     *
      *                  * - Descrizione                               *
      *                  *---------------------------------------------*
       aco-708.
      *                  *---------------------------------------------*
      *                  * Se Descrizione                              *
      *                  *---------------------------------------------*
           move      "D"                  to   w-aux-mne-cea-tpf      .
       aco-710.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per :                    *
      *                  * - Descrizione                               *
      *                  * - Partita iva                               *
      *                  * - Codice fiscale                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione limite massimo             *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-dup    to   w-aux-mne-cea-dmx      .
           move      40                   to   w-aux-mne-cea-c01      .
       aco-712.
           if        w-aux-mne-cea-c01    >    zero
                     if    w-aux-mne-cea-rch
                          (w-aux-mne-cea-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-cea-rch
                                              (w-aux-mne-cea-c01)
                           subtract 1     from w-aux-mne-cea-c01
                           go to    aco-712.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-cea-crb      .
       aco-715.
      *                      *-----------------------------------------*
      *                      * Start su archivio                       *
      *                      *-----------------------------------------*
       aco-719.
      *                          *-------------------------------------*
      *                          * Start su file [cea] per descrizione *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DESKEY    "         to   f-key                  .
           move      w-aux-mne-cea-dup    to   rf-cea-des-key         .
           move      zero                 to   rf-cea-cod-csp         .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
           go to     aco-720.
       aco-720.
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-725.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale archivio            *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     aco-728.
       aco-728.
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-730.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su descrizione                 *
      *                          *-------------------------------------*
           if        rf-cea-des-key       >    w-aux-mne-cea-dmx
                     go to aco-250.
       aco-734.
      *                      *-----------------------------------------*
      *                      * Selezione sul record                    *
      *                      *-----------------------------------------*
       aco-740.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-cea-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di 120 records letti con   *
      *                      * lo stesso valore                        *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-crb    >    120
                     go to aco-745.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-cea-cod-csp       to   w-aux-mne-cea-cbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-cod-mne       to   w-aux-mne-cea-mbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-des-csp       to   w-aux-mne-cea-dbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-cla-csp       to   w-let-arc-zc1-cod      .
           perform   let-arc-zc1-000      thru let-arc-zc1-999        .
           move      w-let-arc-zc1-des    to   w-aux-mne-cea-tbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-mtr-csp       to   w-aux-mne-cea-lbu
                                              (w-aux-mne-cea-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il centoventesimo record   *
      *                      * in interrogazione per partita iva o per *
      *                      * codice fiscale : come per fine file,    *
      *                      * altrimenti : riciclo in lettura         *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-tpf    not  = "P" and
                     w-aux-mne-cea-tpf    not  = "F"
                     go to aco-725.
           if        w-aux-mne-cea-crb    =    120
                     go to aco-250
           else      go to aco-725.
       aco-745.
      *                      *-----------------------------------------*
      *                      * Se piu' di centoventi record con la     *
      *                      * stessa ragione sociale impostata        *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge6010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-510.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-cea-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-800.
      *              *=================================================*
      *              * Se ricerca per ragione sociale contabile        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo di ricerca                             *
      *                  *---------------------------------------------*
           move      "C"                  to   w-aux-mne-cea-tpf      .
      *                  *---------------------------------------------*
      *                  * Spaces in ragione sociale di comodo         *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-cea-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-815.
      *                  *---------------------------------------------*
      *                  * Accettazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      w-aux-mne-cea-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-cea-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-820.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-cea-dln    to   v-lin                  .
           move      w-cod-mne-cea-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-025.
       aco-820.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     go to aco-075.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : come Up      *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-dup    =    spaces
                     go to aco-025.
       aco-825.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica contabile per :          *
      *                  * - Ragione sociale                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione limite massimo             *
      *                      *-----------------------------------------*
           move      w-aux-mne-cea-dup    to   w-aux-mne-cea-dmx      .
           move      40                   to   w-aux-mne-cea-c01      .
       aco-830.
           if        w-aux-mne-cea-c01    >    zero
                     if    w-aux-mne-cea-rch
                          (w-aux-mne-cea-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-cea-rch
                                              (w-aux-mne-cea-c01)
                           subtract 1     from w-aux-mne-cea-c01
                           go to    aco-830.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-cea-crb      .
       aco-835.
      *                      *-----------------------------------------*
      *                      * Start su archivio                       *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      w-aux-mne-cea-dup    to   rf-cea-des-key         .
           move      zero                 to   rf-cea-cod-csp         .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                          *-------------------------------------*
      *                          * Se start errata                     *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-840.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale archivio            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura sequenziale file [cea]      *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcea"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cea                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-845.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-cea-des-key       >    w-aux-mne-cea-dmx
                     go to aco-250.
       aco-850.
      *                      *-----------------------------------------*
      *                      * Flag esito operazione                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-aux-mne-cea-fes      .
       aco-855.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-cea-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di 120 records letti con   *
      *                      * lo stesso valore                        *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-crb    >    120
                     go to aco-860.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-cea-cod-csp       to   w-aux-mne-cea-cbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-cod-mne       to   w-aux-mne-cea-mbu
                                              (w-aux-mne-cea-crb)     .
           move      rf-cea-des-csp       to   w-aux-mne-cea-dbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-cla-csp       to   w-let-arc-zc1-cod      .
           perform   let-arc-zc1-000      thru let-arc-zc1-999        .
           move      w-let-arc-zc1-des    to   w-aux-mne-cea-tbu
                                              (w-aux-mne-cea-crb)     .
      *
           move      rf-cea-mtr-csp       to   w-aux-mne-cea-lbu
                                              (w-aux-mne-cea-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunto il centoventesimo record   *
      *                      * in interrogazione per partita iva o per *
      *                      * codice fiscale : come per fine file,    *
      *                      * altrimenti : riciclo in lettura         *
      *                      *-----------------------------------------*
           if        w-aux-mne-cea-tpf    not  = "P" and
                     w-aux-mne-cea-tpf    not  = "F"
                     go to aco-840.
           if        w-aux-mne-cea-crb    =    120
                     go to aco-250
           else      go to aco-840.
       aco-860.
      *                      *-----------------------------------------*
      *                      * Se piu' di centoventi record con la     *
      *                      * stessa ragione sociale impostata        *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge6010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-815.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-cea-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione contatori                    *
      *                  *---------------------------------------------*
           move      w-aux-mne-cea-c01    to   w-aux-mne-cea-c02      .
           add       5                    to   w-aux-mne-cea-c02      .
           divide    6                    into w-aux-mne-cea-c02      .
           move      w-aux-mne-cea-c02    to   w-aux-mne-cea-cpa      .
           subtract  1                    from w-aux-mne-cea-c02      .
           multiply  6                    by   w-aux-mne-cea-c02      .
           add       1                    to   w-aux-mne-cea-c02      .
           add       5
                     w-aux-mne-cea-c02  giving w-aux-mne-cea-c03      .
           move      w-aux-mne-cea-c03    to   w-aux-mne-cea-c04      .
           if        w-aux-mne-cea-c03    >    w-aux-mne-cea-crb
                     move  w-aux-mne-cea-crb
                                          to   w-aux-mne-cea-c03      .
           move      07                   to   w-aux-mne-cea-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Codice cespite                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-cea-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-aux-mne-cea-cbu
                    (w-aux-mne-cea-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-952.
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-cea-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-mne-cea-dbu
                    (w-aux-mne-cea-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Pulizia area rimanente                      *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-mne-cea-c02      .
           add       1                    to   w-aux-mne-cea-c05      .
           if        w-aux-mne-cea-c02    not  > w-aux-mne-cea-c03
                     go to aco-951.
       aco-953.
           if        w-aux-mne-cea-c02    >    w-aux-mne-cea-c04
                     go to aco-955.
           if        w-aux-mne-cea-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-cea-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-cea-c02      .
           add       1                    to   w-aux-mne-cea-c05      .
           go to     aco-953.
       aco-955.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-cea-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-cea-ep1      .
      *
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-mne-cea-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-mne-cea-ep2      .
      *
           move      spaces               to   w-aux-mne-cea-ltp      .
           string    "Pagina "
                                delimited by   size
                     w-aux-mne-cea-ep1
                                delimited by   spaces
                     " di "
                                delimited by   size
                     w-aux-mne-cea-ep2
                                delimited by   spaces
                                          into w-aux-mne-cea-ltp      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-aux-mne-cea-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Routines per la gestione del file relative di appoggio    *
      *    * [rlt]                                                     *
      *    *-----------------------------------------------------------*

      *    *-----------------------------------------------------------*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rlt-opn-000.
      *              *-------------------------------------------------*
      *              * Richiesta pathname al modulo segreteria         *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   f-rlt-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di open                              *
      *              *-------------------------------------------------*
           open      i-o   rlt                                        .
       rlt-opn-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rlt-cls-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     rlt                                              .
      *              *-------------------------------------------------*
      *              * Delete file                                     *
      *              *-------------------------------------------------*
           delete    file    rlt                                      .
       rlt-cls-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Put                                                       *
      *    *-----------------------------------------------------------*
       rlt-put-000.
      *              *-------------------------------------------------*
      *              * 1. tentativo : rewrite                          *
      *              *-------------------------------------------------*
           rewrite   rlt-rec invalid key
                             go to   rlt-put-200.
           go to     rlt-put-999.
       rlt-put-200.
      *              *-------------------------------------------------*
      *              * 2. tentativo : write                            *
      *              *-------------------------------------------------*
           write     rlt-rec invalid key
                             go to   rlt-put-000.
           go to     rlt-put-999.
       rlt-put-999.
           exit.

      *    *-----------------------------------------------------------*
      *    * Get                                                       *
      *    *-----------------------------------------------------------*
       rlt-get-000.
      *              *-------------------------------------------------*
      *              * Read                                            *
      *              *-------------------------------------------------*
           read      rlt   with no lock
                           invalid key
                           move    spaces to   rlt-rec                .
       rlt-get-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zc1]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczc10.lts"                   .

