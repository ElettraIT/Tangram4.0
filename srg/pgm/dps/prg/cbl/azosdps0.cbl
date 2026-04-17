       Identification Division.
       Program-Id.                                 azosdps0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dps                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/09/93    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice filtro di ordi-  *
      *                    namento e selezione per file [dps]          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "AC"                 *
      *                                                                *
      *                       w-cod-zos-dps-cod : codice filtro        *
      *                                                                *
      *                       w-cod-zos-dps-lin : linea codice         *
      *                                                                *
      *                       w-cod-zos-dps-pos : posizione codice     *
      *                                                                *
      *                       w-cod-zos-dps-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-zos-dps-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dps-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dps-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dps-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "LF"  Lettura filtro per ottenimento descrizione               *
      *                                                                *
      *              Input  : w-cod-zos-dps-ope : "LF"                 *
      *                                                                *
      *                       w-cod-zos-dps-cod : codice filtro        *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dps-flg : Flag di esistenza    *
      *                                            - Spaces : Si       *
      *                                            - #      : No       *
      *                                                                *
      *                       w-cod-zos-dps-des : Descrizione filtro   *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zos]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfzos"                          .
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .
      *        *-------------------------------------------------------*
      *        * [zs1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfzs1"                          .

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
       01  w-aux-zos-dps.
           05  w-aux-zos-dps-flt          pic  x(04) value     "dps " .
           05  w-aux-zos-dps-c01          pic  9(02)                  .
           05  w-aux-zos-dps-c02          pic  9(02)                  .
           05  w-aux-zos-dps-c03          pic  9(02)                  .
           05  w-aux-zos-dps-c04          pic  9(02)                  .
           05  w-aux-zos-dps-c05          pic  9(02)                  .
           05  w-aux-zos-dps-nli          pic  9(02)                  .
           05  w-aux-zos-dps-m10          pic  x(10)                  .
           05  w-aux-zos-dps-tpf          pic  x(01)                  .
           05  w-aux-zos-dps-crb          pic  9(02)                  .
           05  w-aux-zos-dps-cpb          pic  9(02)                  .
           05  w-aux-zos-dps-cpa          pic  9(02)                  .
           05  w-aux-zos-dps-buf
                               occurs 30.
               10  w-aux-zos-dps-cbu      pic  9(07)                  .
               10  w-aux-zos-dps-mbu      pic  x(10)                  .
               10  w-aux-zos-dps-dbu.
                   20  w-aux-zos-dps-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
           05  w-aux-zos-dps-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-zos-dps-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-zos-dps-lt2      pic  9(01)                  .
           05  w-aux-zos-dps-dup          pic  x(40)                  .
           05  w-aux-zos-dps-dmx.
               10  w-aux-zos-dps-dch
                               occurs 40  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per l'accettazione di un singolo codice anagrafico   *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-cod.
           05  w-acc-sng-cod-tip          pic  9(01)                  .
           05  w-acc-sng-cod-num          pic  9(07)                  .
           05  w-acc-sng-cod-alf          pic  x(14)                  .
           05  w-acc-sng-cod-lin          pic  9(02)                  .
           05  w-acc-sng-cod-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per l'accettazione di un singolo semilavorato        *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-dps.
           05  w-acc-sng-dps-num          pic  9(07)                  .
           05  w-acc-sng-dps-alf          pic  x(14)                  .
           05  w-acc-sng-dps-lin          pic  9(02)                  .
           05  w-acc-sng-dps-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per l'accettazione di una singola classe             *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-zs1.
           05  w-acc-sng-zs1-cla          pic  9(05)                  .
           05  w-acc-sng-zs1-lin          pic  9(02)                  .
           05  w-acc-sng-zs1-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-num      pic  9(07)                  .
               10  w-let-arc-dps-alf      pic  x(14)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zs1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zs1.
               10  w-let-arc-zs1-flg      pic  x(01)                  .
               10  w-let-arc-zs1-cla      pic  9(05)                  .
               10  w-let-arc-zs1-mne      pic  x(05)                  .
               10  w-let-arc-zs1-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dps'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe semilavorato     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acmnzs10.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dps]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/azosdps0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-zos-dps
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
           if        w-cod-zos-dps-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-zos-dps-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-zos-dps-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-zos-dps-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-zos-dps-ope    =    "A+" or
                     w-cod-zos-dps-ope    =    "I+" or
                     w-cod-zos-dps-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-zos-dps-ope    =    "LF"
                     perform   ltf-000    thru ltf-999                .
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
      *                  *---------------------------------------------*
      *                  * Open file [zos]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Open file [dps]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Open file [zs1]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice prodotto su *
      *                  * 'dps'                                       *
      *                  *---------------------------------------------*
           perform   cod-cod-dps-opn-000  thru cod-cod-dps-opn-999    .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice classe mer- *
      *                  * ceologica semilavorato                      *
      *                  *---------------------------------------------*
           perform   cod-mne-zs1-opn-000  thru cod-mne-zs1-opn-999    .
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
      *                  *---------------------------------------------*
      *                  * Close file [zos]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                  *---------------------------------------------*
      *                  * Close file [dps]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
      *                  *---------------------------------------------*
      *                  * Close file [zs1]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice prodotto   *
      *                  * su 'dps'                                    *
      *                  *---------------------------------------------*
           perform   cod-cod-dps-cls-000  thru cod-cod-dps-cls-999    .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice classe     *
      *                  * merceologica semilavorato                   *
      *                  *---------------------------------------------*
           perform   cod-mne-zs1-cls-000  thru cod-mne-zs1-cls-999    .
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
                     move  spaces         to   w-cod-zos-dps-ope      .
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
           move      "pdps0011"           to   s-pro                  .
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
           move      "pdps0010"           to   s-pro                  .
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
           move      w-cod-zos-dps-cod    to   w-cod-zos-dps-num      .
           move      v-edm                to   w-cod-zos-dps-edm      .
           move      v-ufk                to   w-cod-zos-dps-ufk      .
       acc-300.
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *                                                 *
      *              * Nota: Se in ingresso si ha gia' un codice ana-  *
      *              *       grafico si propone un singolo puntino     *
      *              *       come default di accettazione              *
      *              *-------------------------------------------------*
           if        w-cod-zos-dps-tco    not  = zero
                     move  "."            to   w-cod-zos-dps-alf
                     go to acc-400.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dps-edm    to   v-edm                  .
           move      w-cod-zos-dps-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-dps-alf      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-zos-dps-ufk    to   v-ufk                  .
           move      w-cod-zos-dps-lin    to   v-lin                  .
           move      w-cod-zos-dps-pos    to   v-pos                  .
           move      w-cod-zos-dps-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-dps-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-zos-dps-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find o da Insr               *
      *              *-------------------------------------------------*
           if        w-cod-zos-dps-ope    =    "F+" or
                     w-cod-zos-dps-ope    =    "I+"
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
           move      w-cod-zos-dps-ufk    to   v-ufk                  .
           move      w-cod-zos-dps-lin    to   v-lin                  .
           move      w-cod-zos-dps-pos    to   v-pos                  .
           move      w-cod-zos-dps-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-dps-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dps-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-zos-dps-alf      .
      *              *-------------------------------------------------*
      *              * Test se Exit o Delt                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-175.
       aco-050.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione campo editato                 *
      *              *                                                 *
      *              * Nota: Se in ingresso si aveva un codice ana-    *
      *              *       grafico, e l'attuale valore alfanumerico  *
      *              *       equivale ad un singolo puntino, si vi-    *
      *              *       sualizza il valore spaces.                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-zos-dps-lin    to   v-lin                  .
           move      w-cod-zos-dps-pos    to   v-pos                  .
           if        w-cod-zos-dps-tco    not  = zero and
                     w-cod-zos-dps-alf    =    "."
                     move  spaces         to   v-alf
           else      move  w-cod-zos-dps-alf
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-zos-dps-num    to   w-cod-zos-dps-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-dps-dln      .
           move      zero                 to   w-cod-zos-dps-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find o da Insr                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-flt"            to   s-var                  .
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
           move      s-num                to   w-cod-zos-dps-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dps-edm    to   v-edm                  .
           move      w-cod-zos-dps-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-dps-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-zos-dps-ope      .
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
           move      "F+"                 to   w-cod-zos-dps-ope      .
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
           if        w-cod-zos-dps-alf    =    spaces
                     move  zero           to   w-cod-zos-dps-num
                     go to aco-075.
           if        w-cod-zos-dps-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-zos-dps-c01      .
       aco-176.
           add       1                    to   w-aux-zos-dps-c01      .
           if        w-aux-zos-dps-c01    >    10
                     go to aco-180.
           if        w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-zos-dps-c01      .
           if        w-aux-zos-dps-c01    >    10
                     go to aco-180.
           if        w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-dps-num      .
           move      zero                 to   w-aux-zos-dps-c01      .
       aco-181.
           add       1                    to   w-aux-zos-dps-c01      .
           if        w-aux-zos-dps-c01    >    10
                     go to aco-200.
           if        w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   =    spaces
                     go to aco-200.
           if        w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   <    "0" or
                     w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-zos-dps-num      .
           move      w-cod-zos-dps-cha
                    (w-aux-zos-dps-c01)   to   w-cod-zos-dps-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-zos-dps-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dps-edm    to   v-edm                  .
           move      w-cod-zos-dps-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-zos-dps-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-zos-dps-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se l'impostazione corrisponde ad un singo-  *
      *                  * lo puntino, e il valore di default corri-   *
      *                  * spondeva ad un codice anagrafico, si puo'   *
      *                  * uscire con il valore di uscita pari al      *
      *                  * valore di default                           *
      *                  *---------------------------------------------*
           if        w-cod-zos-dps-tco    not  = zero and
                     w-cod-zos-dps-alf    =    "."
                     move  w-cod-zos-dps-cod
                                          to   w-cod-zos-dps-num
                     go to aco-050.
       aco-230.
      *                  *---------------------------------------------*
      *                  * Test se valore ':' per impostazione di un   *
      *                  * singolo codice anagrafico                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-dps-alf    not  = ": " and
                     w-cod-zos-dps-alf    not  = ":S" and
                     w-cod-zos-dps-alf    not  = ":C"
                     go to aco-235.
      *                  *---------------------------------------------*
      *                  * Preparazione e richiamo subroutine di ac-   *
      *                  * cettazione singolo codice anagrafico        *
      *                  *---------------------------------------------*
           if        w-cod-zos-dps-alf    =    ": " or
                     w-cod-zos-dps-alf    =    ":S"
                     move  1              to   w-acc-sng-cod-tip
           else      move  2              to   w-acc-sng-cod-tip      .
           move      zero                 to   w-acc-sng-cod-num      .
           move      spaces               to   w-acc-sng-cod-alf      .
           move      w-cod-zos-dps-lin    to   w-acc-sng-cod-lin      .
           move      w-cod-zos-dps-pos    to   w-acc-sng-cod-pos      .
           perform   acc-sng-cod-000      thru acc-sng-cod-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito dell'impo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        w-acc-sng-cod-num    not  = zero
                     go to aco-234.
       aco-232.
      *                  *---------------------------------------------*
      *                  * Se non e' stato scelto alcun codice anagra- *
      *                  * fico                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione valori impostati        *
      *                      *-----------------------------------------*
           move      zero                 to   w-cod-zos-dps-num      .
           move      spaces               to   w-cod-zos-dps-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione a spaces    *
      *                      *-----------------------------------------*
           if        w-cod-zos-dps-lin    =    zero
                     go to aco-233.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dps-dln    to   v-lin                  .
           move      w-cod-zos-dps-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-233.
      *                      *-----------------------------------------*
      *                      * Ad uscita per reimpostazione            *
      *                      *-----------------------------------------*
           go to      aco-025.
       aco-234.
      *                  *---------------------------------------------*
      *                  * Se e' stato scelto un codice anagrafico     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore numerico in uscita               *
      *                      *-----------------------------------------*
           move      w-acc-sng-cod-num    to   w-cod-zos-dps-num      .
           if        w-acc-sng-cod-tip    =    1
                     add   10000000       to   w-cod-zos-dps-num
           else      add   20000000       to   w-cod-zos-dps-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico a spaces            *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-zos-dps-alf      .
      *                      *-----------------------------------------*
      *                      * A rivisualizzazione ed uscita           *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-235.
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-dps-alf    =    "-" and
                     w-cod-zos-dps-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-zos-dps-tpf      .
           move      zero                 to   w-aux-zos-dps-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "dps "               to   rf-zos-tip-rec         .
           move      w-cod-zos-dps-alf    to   rf-zos-cod-mne         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
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
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zos-tip-rec       not  = "dps "
                     go to aco-250.
           if        rf-zos-cod-mne       not  = w-cod-zos-dps-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Se mnemonico a spaces : si ignora il    *
      *                      * record letto                            *
      *                      *-----------------------------------------*
           if        rf-zos-cod-mne       =    spaces
                     go to aco-240.
      *                      *-----------------------------------------*
      *                      * Se codice a zero : si ignora il record  *
      *                      * letto                                   *
      *                      *-----------------------------------------*
           if        rf-zos-cod-flt       =    zero
                     go to aco-240.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 10 *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-zos-cod-mne       to   w-aux-zos-dps-m10      .
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-zos-dps-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-zos-dps-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-dps-cbu
                                              (w-aux-zos-dps-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-dps-mbu
                                              (w-aux-zos-dps-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-dps-dbu
                                              (w-aux-zos-dps-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-240.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-zos-dps-crb    =    zero
                     go to aco-275
           else if   w-aux-zos-dps-crb    =    1
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
           if        w-cod-zos-dps-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dps-dln    to   v-lin                  .
           move      w-cod-zos-dps-dps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *                      * Codice numerico in valore numerico      *
      *                      *-----------------------------------------*
           move      w-aux-zos-dps-cbu (1)
                                          to   w-cod-zos-dps-num      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione ed uscita             *
      *                      *-----------------------------------------*
           go to     aco-200.
       aco-325.
      *                  *---------------------------------------------*
      *                  * Se non piu' di trenta records con lo stesso *
      *                  * valore                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-aux-zos-dps-crb    to   w-aux-zos-dps-cpb      .
           subtract  1                    from w-aux-zos-dps-cpb      .
           divide    12                   into w-aux-zos-dps-cpb      .
           add       1                    to   w-aux-zos-dps-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-zos-dps-c01      .
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
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "              Selezionare il filtro desiderato    
      -              "          "         to   v-alf                  .
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
           move      19                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
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
           move      w-aux-zos-dps-c01    to   w-aux-zos-dps-nli      .
       aco-355.
           if        w-aux-zos-dps-nli    >    12
                     subtract  12         from w-aux-zos-dps-nli
                     go to aco-355.
           add       06                   to   w-aux-zos-dps-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-zos-dps-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-zos-dps-c01    <    w-aux-zos-dps-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-zos-dps-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-zos-dps-cpa    <    w-aux-zos-dps-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-zos-dps-nli    to   v-lin                  .
           move      31                   to   v-pos                  .
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
           move      w-aux-zos-dps-cbu
                    (w-aux-zos-dps-c01)   to   w-cod-zos-dps-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-zos-dps-c01      .
           if        w-aux-zos-dps-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-zos-dps-c01    <    w-aux-zos-dps-crb
                     add   1              to   w-aux-zos-dps-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-zos-dps-nli    =    18
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-zos-dps-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-zos-dps-cpa      .
       aco-396.
           move      w-aux-zos-dps-cpa    to   w-aux-zos-dps-c01      .
           multiply  12                   by   w-aux-zos-dps-c01      .
           subtract  11                   from w-aux-zos-dps-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-zos-dps-tpf    not  = "M"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdps0011"           to   s-pro                  .
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
           move      w-cod-zos-dps-alf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-500.
      *              *-------------------------------------------------*
      *              * Se ricerca per descrizione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spaces in descrizione di comodo             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-zos-dps-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per descrizione              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dps-dln    to   v-lin                  .
           move      w-cod-zos-dps-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-525.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-zos-dps-dln    to   v-lin                  .
           move      w-cod-zos-dps-dps    to   v-pos                  .
           move      w-aux-zos-dps-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-zos-dps-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dps-dln    to   v-lin                  .
           move      w-cod-zos-dps-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           go to     aco-025.
       aco-550.
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
           if        w-aux-zos-dps-dup    =    spaces
                     go to aco-025.
       aco-555.
      *                  *---------------------------------------------*
      *                  * Tipo ricerca : 'D'                          *
      *                  *---------------------------------------------*
       aco-570.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione limite massimo             *
      *                      *-----------------------------------------*
           move      w-aux-zos-dps-dup    to   w-aux-zos-dps-dmx      .
           move      40                   to   w-aux-zos-dps-c01      .
       aco-575.
           if        w-aux-zos-dps-c01    >    zero
                     if    w-aux-zos-dps-dch
                          (w-aux-zos-dps-c01)
                                          =    spaces
                           move     "z"   to   w-aux-zos-dps-dch
                                              (w-aux-zos-dps-c01)
                           subtract 1     from w-aux-zos-dps-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-zos-dps-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "dps "               to   rf-zos-tip-rec         .
           move      w-aux-zos-dps-dup    to   rf-zos-des-key         .
           move      zero                 to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-600.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per descrizione     *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zos-tip-rec       not  = "dps "
                     go to aco-250.
           if        rf-zos-des-key       >    w-aux-zos-dps-dmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Se mnemonico a spaces : si ignora il    *
      *                      * record letto                            *
      *                      *-----------------------------------------*
           if        rf-zos-cod-mne       =    spaces
                     go to aco-600.
      *                      *-----------------------------------------*
      *                      * Se codice a zero : si ignora il record  *
      *                      * letto                                   *
      *                      *-----------------------------------------*
           if        rf-zos-cod-flt       =    zero
                     go to aco-600.
      *                      *-----------------------------------------*
      *                      * Se descrizione a spaces : si ignora il  *
      *                      * record letto                            *
      *                      *-----------------------------------------*
           if        rf-zos-des-key       =    spaces
                     go to aco-600.
       aco-625.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-zos-dps-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso valore                    *
      *                      *-----------------------------------------*
           if        w-aux-zos-dps-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-dps-cbu
                                              (w-aux-zos-dps-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-dps-mbu
                                              (w-aux-zos-dps-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-dps-dbu
                                              (w-aux-zos-dps-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a Read Next                     *
      *                      *-----------------------------------------*
           go to     aco-600.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Se piu' di trenta record con la stessa  *
      *                      * descrizione impostata                   *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdps0011"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-525.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "D"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "des-flt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-zos-dps-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-zos-dps-c01    to   w-aux-zos-dps-c02      .
           add       11                   to   w-aux-zos-dps-c02      .
           divide    12                   into w-aux-zos-dps-c02      .
           move      w-aux-zos-dps-c02    to   w-aux-zos-dps-cpa      .
           subtract  1                    from w-aux-zos-dps-c02      .
           multiply  12                   by   w-aux-zos-dps-c02      .
           add       1                    to   w-aux-zos-dps-c02      .
           add       11
                     w-aux-zos-dps-c02  giving w-aux-zos-dps-c03      .
           move      w-aux-zos-dps-c03    to   w-aux-zos-dps-c04      .
           if        w-aux-zos-dps-c03    >    w-aux-zos-dps-crb
                     move  w-aux-zos-dps-crb
                                          to   w-aux-zos-dps-c03      .
           move      07                   to   w-aux-zos-dps-c05      .
       aco-951.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-zos-dps-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-aux-zos-dps-cbu
                    (w-aux-zos-dps-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-aux-zos-dps-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-aux-zos-dps-mbu
                    (w-aux-zos-dps-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-zos-dps-c05    to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-aux-zos-dps-dbu
                    (w-aux-zos-dps-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-dps-c02      .
           add       1                    to   w-aux-zos-dps-c05      .
           if        w-aux-zos-dps-c02    not  > w-aux-zos-dps-c03
                     go to aco-951.
       aco-952.
           if        w-aux-zos-dps-c02    >    w-aux-zos-dps-c04
                     go to aco-955.
           if        w-aux-zos-dps-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-zos-dps-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-dps-c02      .
           add       1                    to   w-aux-zos-dps-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-zos-dps-cpa    to   w-aux-zos-dps-lt1      .
           move      w-aux-zos-dps-cpb    to   w-aux-zos-dps-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-zos-dps-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Lettura filtro di ordinamento e selezione per ottenimento *
      *    * della descrizione e del flag di esistenza                 *
      *    *-----------------------------------------------------------*
       ltf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri in output             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di esistenza                           *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-zos-dps-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-zos-dps-des      .
       ltf-050.
      *              *-------------------------------------------------*
      *              * Se codice filtro a zero : uscita                *
      *              *-------------------------------------------------*
           if        w-cod-zos-dps-cco    =    zero
                     go to ltf-999.
       ltf-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di codice         *
      *              *-------------------------------------------------*
           if        w-cod-zos-dps-tco    =    zero
                     go to ltf-200
           else if   w-cod-zos-dps-tco    =    1
                     go to ltf-400
           else      go to ltf-600.
       ltf-200.
      *              *-------------------------------------------------*
      *              * Se filtro vero e proprio                        *
      *              *-------------------------------------------------*
       ltf-225.
      *                  *---------------------------------------------*
      *                  * Lettura filtro                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFLT    "         to   f-key                  .
           move      "dps "               to   rf-zos-tip-rec         .
           move      w-cod-zos-dps-cco    to   rf-zos-cod-flt         .
           move      "pgm/azi/fls/ioc/obj/iofzos"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zos                 .
       ltf-250.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito lettura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ltf-300.
       ltf-275.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in area di output           *
      *                      *-----------------------------------------*
           move      rf-zos-des-flt       to   w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-300.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Puntini in area di output               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dps-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-400.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [dps]              *
      *              *-------------------------------------------------*
       ltf-425.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-cod-zos-dps-cco    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
       ltf-450.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito lettura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ltf-500.
       ltf-475.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in area di output           *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-zos-dps-des      .
           string    "Codice semilavorato = "
                                delimited by   size
                     rf-dps-alf-sem
                                delimited by   size
                                          into w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-500.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Puntini in area di output               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dps-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-600.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [zs1]              *
      *              *-------------------------------------------------*
       ltf-625.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-cod-zos-dps-cco    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
       ltf-650.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito lettura          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ltf-700.
       ltf-675.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in area di output           *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-cod-zos-dps-cco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      spaces               to   w-cod-zos-dps-des      .
           string    "Codice classe merceologica = "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-700.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Puntini in area di output               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-cod-zos-dps-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dps-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di un singolo codice anagrafico              *
      *    *-----------------------------------------------------------*
       acc-sng-cod-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di codice anagra- *
      *              * fico                                            *
      *              *-------------------------------------------------*
           if        w-acc-sng-cod-tip    =    1
                     go to acc-sng-cod-100
           else      go to acc-sng-cod-200.
       acc-sng-cod-100.
      *              *-------------------------------------------------*
      *              * Se codice [dps]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-dps-num      .
           move      w-acc-sng-cod-alf    to   w-acc-sng-dps-alf      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-dps-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-dps-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-dps-000      thru acc-sng-dps-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-dps-num    to   w-acc-sng-cod-num      .
           move      w-acc-sng-dps-alf    to   w-acc-sng-cod-alf      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-200.
      *              *-------------------------------------------------*
      *              * Se codice [zs1]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-zs1-cla      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-zs1-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-zs1-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-zs1-000      thru acc-sng-zs1-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-zs1-cla    to   w-acc-sng-cod-num      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di un singolo codice anagrafico [dps]        *
      *    *-----------------------------------------------------------*
       acc-sng-dps-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della linea di accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-acc-sng-dps-lin    >    16
                     move  16             to   w-acc-sng-dps-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-dps-pos    >    30
                     move  30             to   w-acc-sng-dps-pos      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-dps-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-dps-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           move      "        Selezione del codice semilavorato         
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           move      " Codice :                                         
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-dps-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dps-ope      .
           move      "A"                  to   w-cod-cod-dps-tac      .
           move      w-acc-sng-dps-num    to   w-cod-cod-dps-num      .
           move      w-acc-sng-dps-alf    to   w-cod-cod-dps-alf      .
           move      w-acc-sng-dps-lin    to   w-cod-cod-dps-lin      .
           add       03                   to   w-cod-cod-dps-lin      .
           move      w-acc-sng-dps-pos    to   w-cod-cod-dps-pos      .
           add       10                   to   w-cod-cod-dps-pos      .
           move      w-acc-sng-dps-lin    to   w-cod-cod-dps-dln      .
           add       05                   to   w-cod-cod-dps-dln      .
           move      w-acc-sng-dps-pos    to   w-cod-cod-dps-dps      .
           add       01                   to   w-cod-cod-dps-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
       acc-sng-dps-210.
           perform   cod-cod-dps-cll-000  thru cod-cod-dps-cll-999    .
           if        w-cod-cod-dps-ope    =    "F+"
                     go to acc-sng-dps-215.
           if        w-cod-cod-dps-ope    =    "AC"
                     go to acc-sng-dps-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-dps-215.
           perform   cod-cod-dps-foi-000  thru cod-cod-dps-foi-999    .
           go to     acc-sng-dps-210.
       acc-sng-dps-220.
           move      w-cod-cod-dps-alf    to   v-alf                  .
           move      w-cod-cod-dps-num    to   v-num                  .
       acc-sng-dps-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-dps-num
                     go to acc-sng-dps-350.
       acc-sng-dps-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-dps-num      .
           move      v-alf                to   w-acc-sng-dps-alf      .
       acc-sng-dps-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dps]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-dps-num    to   w-let-arc-dps-num      .
           move      w-acc-sng-dps-alf    to   w-let-arc-dps-alf      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-dps-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-dps-num    =    zero
                     go to acc-sng-dps-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-flg    not  = spaces
                     go to acc-sng-dps-200.
       acc-sng-dps-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione per presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "RTRN"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-dps-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dps-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-dps-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-dps-num
                     go to acc-sng-dps-350.
       acc-sng-dps-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-dps-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di una singola classe [zs1]                  *
      *    *-----------------------------------------------------------*
       acc-sng-zs1-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della linea di accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-acc-sng-zs1-lin    >    16
                     move  16             to   w-acc-sng-zs1-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-zs1-pos    >    30
                     move  30             to   w-acc-sng-zs1-pos      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-zs1-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-zs1-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           move      "       Selezione della classe merceologica        
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           move      " Codice :                                         
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-zs1-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zs1-ope      .
           move      w-acc-sng-zs1-cla    to   w-cod-mne-zs1-cla      .
           move      zero                 to   w-cod-mne-zs1-gru      .
           move      zero                 to   w-cod-mne-zs1-sgr      .
           move      w-acc-sng-zs1-lin    to   w-cod-mne-zs1-lin      .
           add       03                   to   w-cod-mne-zs1-lin      .
           move      w-acc-sng-zs1-pos    to   w-cod-mne-zs1-pos      .
           add       10                   to   w-cod-mne-zs1-pos      .
           move      w-acc-sng-zs1-lin    to   w-cod-mne-zs1-dln      .
           add       05                   to   w-cod-mne-zs1-dln      .
           move      w-acc-sng-zs1-pos    to   w-cod-mne-zs1-dps      .
           add       01                   to   w-cod-mne-zs1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-zs1-cll-000  thru cod-mne-zs1-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-mne-zs1-foi-000  thru cod-mne-zs1-foi-999    .
       acc-sng-zs1-210.
           perform   cod-mne-zs1-cll-000  thru cod-mne-zs1-cll-999    .
           if        w-cod-mne-zs1-ope    =    "F+"
                     go to acc-sng-zs1-215.
           if        w-cod-mne-zs1-ope    =    "AC"
                     go to acc-sng-zs1-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-zs1-215.
           perform   cod-mne-zs1-foi-000  thru cod-mne-zs1-foi-999    .
           go to     acc-sng-zs1-210.
       acc-sng-zs1-220.
           move      w-cod-mne-zs1-cla    to   v-num                  .
       acc-sng-zs1-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-zs1-cla
                     go to acc-sng-zs1-350.
       acc-sng-zs1-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-zs1-cla      .
       acc-sng-zs1-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [zs1]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-zs1-cla    to   w-let-arc-zs1-cla      .
           perform   let-arc-zs1-000      thru let-arc-zs1-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-zs1-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-zs1-cla    =    zero
                     go to acc-sng-zs1-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zs1-flg    not  = spaces
                     go to acc-sng-zs1-200.
       acc-sng-zs1-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione per presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "RTRN"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zs1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zs1-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-zs1-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-zs1-cla
                     go to acc-sng-zs1-350.
       acc-sng-zs1-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-zs1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dps]                            *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Semilavorato a zero              *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-num    =    zero
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM    "         to   f-key                  .
           move      w-let-arc-dps-num    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-alf-sem       to   w-let-arc-dps-alf      .
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
           move      all   "."            to   w-let-arc-dps-des      .
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-alf      .
       let-arc-dps-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zs1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zs1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zs1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zs1-cla    =    zero
                     go to let-arc-zs1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zs1-cla    to   rf-zs1-cod-cla         .
           move      "pgm/dps/fls/ioc/obj/iofzs1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zs1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zs1-400.
       let-arc-zs1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zs1-des-cla       to   w-let-arc-zs1-des      .
           move      rf-zs1-mne-cla       to   w-let-arc-zs1-mne      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zs1-999.
       let-arc-zs1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zs1-flg      .
           move      all   "."            to   w-let-arc-zs1-des      .
           go to     let-arc-zs1-600.
       let-arc-zs1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zs1-des      .
       let-arc-zs1-600.
           move      spaces               to   w-let-arc-zs1-mne      .
       let-arc-zs1-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dps'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acoddps0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dps/prg/cpy/acmnzs10.acs"                   .
