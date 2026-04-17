       Identification Division.
       Program-Id.                                 azosmtv0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    mtv                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/09/93    *
      *                       Ultima revisione:    NdK del 14/10/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice filtro di ordi-  *
      *                    namento e selezione per file [mtv]          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "AC"                 *
      *                                                                *
      *                       w-cod-zos-mtv-cod : codice filtro        *
      *                                                                *
      *                       w-cod-zos-mtv-lin : linea codice         *
      *                                                                *
      *                       w-cod-zos-mtv-pos : posizione codice     *
      *                                                                *
      *                       w-cod-zos-mtv-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-zos-mtv-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-mtv-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-mtv-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-mtv-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "LF"  Lettura filtro per ottenimento descrizione               *
      *                                                                *
      *              Input  : w-cod-zos-mtv-ope : "LF"                 *
      *                                                                *
      *                       w-cod-zos-mtv-cod : codice filtro        *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-mtv-flg : Flag di esistenza    *
      *                                            - Spaces : Si       *
      *                                            - #      : No       *
      *                                                                *
      *                       w-cod-zos-mtv-des : Descrizione filtro   *
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
      *        * [mtv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfmtv"                          .
      *        *-------------------------------------------------------*
      *        * [zv1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mtv/fls/rec/rfzv1"                          .

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
       01  w-aux-zos-mtv.
           05  w-aux-zos-mtv-flt          pic  x(04) value     "mtv " .
           05  w-aux-zos-mtv-c01          pic  9(02)                  .
           05  w-aux-zos-mtv-c02          pic  9(02)                  .
           05  w-aux-zos-mtv-c03          pic  9(02)                  .
           05  w-aux-zos-mtv-c04          pic  9(02)                  .
           05  w-aux-zos-mtv-c05          pic  9(02)                  .
           05  w-aux-zos-mtv-nli          pic  9(02)                  .
           05  w-aux-zos-mtv-m10          pic  x(10)                  .
           05  w-aux-zos-mtv-tpf          pic  x(01)                  .
           05  w-aux-zos-mtv-crb          pic  9(02)                  .
           05  w-aux-zos-mtv-cpb          pic  9(02)                  .
           05  w-aux-zos-mtv-cpa          pic  9(02)                  .
           05  w-aux-zos-mtv-buf
                               occurs 30.
               10  w-aux-zos-mtv-cbu      pic  9(07)                  .
               10  w-aux-zos-mtv-mbu      pic  x(10)                  .
               10  w-aux-zos-mtv-dbu.
                   20  w-aux-zos-mtv-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
           05  w-aux-zos-mtv-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-zos-mtv-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-zos-mtv-lt2      pic  9(01)                  .
           05  w-aux-zos-mtv-dup          pic  x(40)                  .
           05  w-aux-zos-mtv-dmx.
               10  w-aux-zos-mtv-dch
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
      *    * Work per l'accettazione di un singolo materiale vario     *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-mtv.
           05  w-acc-sng-mtv-num          pic  9(07)                  .
           05  w-acc-sng-mtv-alf          pic  x(14)                  .
           05  w-acc-sng-mtv-lin          pic  9(02)                  .
           05  w-acc-sng-mtv-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per l'accettazione di una singola classe             *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-zv1.
           05  w-acc-sng-zv1-cla          pic  9(05)                  .
           05  w-acc-sng-zv1-lin          pic  9(02)                  .
           05  w-acc-sng-zv1-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [mtv]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-mtv.
               10  w-let-arc-mtv-flg      pic  x(01)                  .
               10  w-let-arc-mtv-num      pic  9(07)                  .
               10  w-let-arc-mtv-alf      pic  x(14)                  .
               10  w-let-arc-mtv-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zv1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zv1.
               10  w-let-arc-zv1-flg      pic  x(01)                  .
               10  w-let-arc-zv1-cla      pic  9(05)                  .
               10  w-let-arc-zv1-mne      pic  x(05)                  .
               10  w-let-arc-zv1-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice materiame vario 'mtv'   *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe materiale vario  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [mtv]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/azosmtv0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-zos-mtv
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
           if        w-cod-zos-mtv-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-zos-mtv-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-zos-mtv-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-zos-mtv-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-zos-mtv-ope    =    "A+" or
                     w-cod-zos-mtv-ope    =    "I+" or
                     w-cod-zos-mtv-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-zos-mtv-ope    =    "LF"
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
      *                  * Open file [mtv]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                  *---------------------------------------------*
      *                  * Open file [zv1]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice materiale   *
      *                  * vario 'mtv'                                 *
      *                  *---------------------------------------------*
           perform   cod-cod-mtv-opn-000  thru cod-cod-mtv-opn-999    .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice classe mer- *
      *                  * ceologica materiale vario                   *
      *                  *---------------------------------------------*
           perform   cod-mne-zv1-opn-000  thru cod-mne-zv1-opn-999    .
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
      *                  * Close file [mtv]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
      *                  *---------------------------------------------*
      *                  * Close file [zv1]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice prodotto   *
      *                  * su 'mtv'                                    *
      *                  *---------------------------------------------*
           perform   cod-cod-mtv-cls-000  thru cod-cod-mtv-cls-999    .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice classe     *
      *                  * merceologica materiale vario                *
      *                  *---------------------------------------------*
           perform   cod-mne-zv1-cls-000  thru cod-mne-zv1-cls-999    .
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
                     move  spaces         to   w-cod-zos-mtv-ope      .
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
           move      "pmtv0011"           to   s-pro                  .
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
           move      "pmtv0010"           to   s-pro                  .
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
           move      w-cod-zos-mtv-cod    to   w-cod-zos-mtv-num      .
           move      v-edm                to   w-cod-zos-mtv-edm      .
           move      v-ufk                to   w-cod-zos-mtv-ufk      .
       acc-300.
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *                                                 *
      *              * Nota: Se in ingresso si ha gia' un codice ana-  *
      *              *       grafico si propone un singolo puntino     *
      *              *       come default di accettazione              *
      *              *-------------------------------------------------*
           if        w-cod-zos-mtv-tco    not  = zero
                     move  "."            to   w-cod-zos-mtv-alf
                     go to acc-400.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-mtv-edm    to   v-edm                  .
           move      w-cod-zos-mtv-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-mtv-alf      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-zos-mtv-ufk    to   v-ufk                  .
           move      w-cod-zos-mtv-lin    to   v-lin                  .
           move      w-cod-zos-mtv-pos    to   v-pos                  .
           move      w-cod-zos-mtv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-mtv-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-zos-mtv-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find o da Insr               *
      *              *-------------------------------------------------*
           if        w-cod-zos-mtv-ope    =    "F+" or
                     w-cod-zos-mtv-ope    =    "I+"
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
           move      w-cod-zos-mtv-ufk    to   v-ufk                  .
           move      w-cod-zos-mtv-lin    to   v-lin                  .
           move      w-cod-zos-mtv-pos    to   v-pos                  .
           move      w-cod-zos-mtv-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-mtv-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-mtv-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-zos-mtv-alf      .
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
           move      w-cod-zos-mtv-lin    to   v-lin                  .
           move      w-cod-zos-mtv-pos    to   v-pos                  .
           if        w-cod-zos-mtv-tco    not  = zero and
                     w-cod-zos-mtv-alf    =    "."
                     move  spaces         to   v-alf
           else      move  w-cod-zos-mtv-alf
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-zos-mtv-num    to   w-cod-zos-mtv-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-mtv-dln      .
           move      zero                 to   w-cod-zos-mtv-dps      .
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
           move      s-num                to   w-cod-zos-mtv-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-mtv-edm    to   v-edm                  .
           move      w-cod-zos-mtv-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-mtv-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-zos-mtv-ope      .
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
           move      "F+"                 to   w-cod-zos-mtv-ope      .
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
           if        w-cod-zos-mtv-alf    =    spaces
                     move  zero           to   w-cod-zos-mtv-num
                     go to aco-075.
           if        w-cod-zos-mtv-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-zos-mtv-c01      .
       aco-176.
           add       1                    to   w-aux-zos-mtv-c01      .
           if        w-aux-zos-mtv-c01    >    10
                     go to aco-180.
           if        w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-zos-mtv-c01      .
           if        w-aux-zos-mtv-c01    >    10
                     go to aco-180.
           if        w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-mtv-num      .
           move      zero                 to   w-aux-zos-mtv-c01      .
       aco-181.
           add       1                    to   w-aux-zos-mtv-c01      .
           if        w-aux-zos-mtv-c01    >    10
                     go to aco-200.
           if        w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   =    spaces
                     go to aco-200.
           if        w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   <    "0" or
                     w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-zos-mtv-num      .
           move      w-cod-zos-mtv-cha
                    (w-aux-zos-mtv-c01)   to   w-cod-zos-mtv-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-zos-mtv-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-mtv-edm    to   v-edm                  .
           move      w-cod-zos-mtv-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-zos-mtv-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-zos-mtv-alf
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
           if        w-cod-zos-mtv-tco    not  = zero and
                     w-cod-zos-mtv-alf    =    "."
                     move  w-cod-zos-mtv-cod
                                          to   w-cod-zos-mtv-num
                     go to aco-050.
       aco-230.
      *                  *---------------------------------------------*
      *                  * Test se valore ':' per impostazione di un   *
      *                  * singolo codice anagrafico                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-mtv-alf    not  = ": " and
                     w-cod-zos-mtv-alf    not  = ":V" and
                     w-cod-zos-mtv-alf    not  = ":C"
                     go to aco-235.
      *                  *---------------------------------------------*
      *                  * Preparazione e richiamo subroutine di ac-   *
      *                  * cettazione singolo codice anagrafico        *
      *                  *---------------------------------------------*
           if        w-cod-zos-mtv-alf    =    ": " or
                     w-cod-zos-mtv-alf    =    ":V"
                     move  1              to   w-acc-sng-cod-tip
           else      move  2              to   w-acc-sng-cod-tip      .
           move      zero                 to   w-acc-sng-cod-num      .
           move      spaces               to   w-acc-sng-cod-alf      .
           move      w-cod-zos-mtv-lin    to   w-acc-sng-cod-lin      .
           move      w-cod-zos-mtv-pos    to   w-acc-sng-cod-pos      .
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
           move      zero                 to   w-cod-zos-mtv-num      .
           move      spaces               to   w-cod-zos-mtv-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione a spaces    *
      *                      *-----------------------------------------*
           if        w-cod-zos-mtv-lin    =    zero
                     go to aco-233.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-mtv-dln    to   v-lin                  .
           move      w-cod-zos-mtv-dps    to   v-pos                  .
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
           move      w-acc-sng-cod-num    to   w-cod-zos-mtv-num      .
           if        w-acc-sng-cod-tip    =    1
                     add   10000000       to   w-cod-zos-mtv-num
           else      add   20000000       to   w-cod-zos-mtv-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico a spaces            *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-zos-mtv-alf      .
      *                      *-----------------------------------------*
      *                      * A rivisualizzazione ed uscita           *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-235.
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-mtv-alf    =    "-" and
                     w-cod-zos-mtv-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-zos-mtv-tpf      .
           move      zero                 to   w-aux-zos-mtv-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "mtv "               to   rf-zos-tip-rec         .
           move      w-cod-zos-mtv-alf    to   rf-zos-cod-mne         .
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
           if        rf-zos-tip-rec       not  = "mtv "
                     go to aco-250.
           if        rf-zos-cod-mne       not  = w-cod-zos-mtv-alf
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
           move      rf-zos-cod-mne       to   w-aux-zos-mtv-m10      .
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-zos-mtv-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-zos-mtv-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-mtv-cbu
                                              (w-aux-zos-mtv-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-mtv-mbu
                                              (w-aux-zos-mtv-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-mtv-dbu
                                              (w-aux-zos-mtv-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-240.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-zos-mtv-crb    =    zero
                     go to aco-275
           else if   w-aux-zos-mtv-crb    =    1
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
           if        w-cod-zos-mtv-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-mtv-dln    to   v-lin                  .
           move      w-cod-zos-mtv-dps    to   v-pos                  .
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
           move      w-aux-zos-mtv-cbu (1)
                                          to   w-cod-zos-mtv-num      .
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
           move      w-aux-zos-mtv-crb    to   w-aux-zos-mtv-cpb      .
           subtract  1                    from w-aux-zos-mtv-cpb      .
           divide    12                   into w-aux-zos-mtv-cpb      .
           add       1                    to   w-aux-zos-mtv-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-zos-mtv-c01      .
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
           move      w-aux-zos-mtv-c01    to   w-aux-zos-mtv-nli      .
       aco-355.
           if        w-aux-zos-mtv-nli    >    12
                     subtract  12         from w-aux-zos-mtv-nli
                     go to aco-355.
           add       06                   to   w-aux-zos-mtv-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-zos-mtv-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-zos-mtv-c01    <    w-aux-zos-mtv-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-zos-mtv-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-zos-mtv-cpa    <    w-aux-zos-mtv-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-zos-mtv-nli    to   v-lin                  .
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
           move      w-aux-zos-mtv-cbu
                    (w-aux-zos-mtv-c01)   to   w-cod-zos-mtv-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-zos-mtv-c01      .
           if        w-aux-zos-mtv-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-zos-mtv-c01    <    w-aux-zos-mtv-crb
                     add   1              to   w-aux-zos-mtv-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-zos-mtv-nli    =    18
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-zos-mtv-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-zos-mtv-cpa      .
       aco-396.
           move      w-aux-zos-mtv-cpa    to   w-aux-zos-mtv-c01      .
           multiply  12                   by   w-aux-zos-mtv-c01      .
           subtract  11                   from w-aux-zos-mtv-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-zos-mtv-tpf    not  = "M"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pmtv0011"           to   s-pro                  .
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
           move      w-cod-zos-mtv-alf    to   s-alf                  .
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
           move      spaces               to   w-aux-zos-mtv-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per descrizione              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-mtv-dln    to   v-lin                  .
           move      w-cod-zos-mtv-dps    to   v-pos                  .
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
           move      w-cod-zos-mtv-dln    to   v-lin                  .
           move      w-cod-zos-mtv-dps    to   v-pos                  .
           move      w-aux-zos-mtv-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-zos-mtv-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-mtv-dln    to   v-lin                  .
           move      w-cod-zos-mtv-dps    to   v-pos                  .
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
           if        w-aux-zos-mtv-dup    =    spaces
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
           move      w-aux-zos-mtv-dup    to   w-aux-zos-mtv-dmx      .
           move      40                   to   w-aux-zos-mtv-c01      .
       aco-575.
           if        w-aux-zos-mtv-c01    >    zero
                     if    w-aux-zos-mtv-dch
                          (w-aux-zos-mtv-c01)
                                          =    spaces
                           move     "z"   to   w-aux-zos-mtv-dch
                                              (w-aux-zos-mtv-c01)
                           subtract 1     from w-aux-zos-mtv-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-zos-mtv-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "mtv "               to   rf-zos-tip-rec         .
           move      w-aux-zos-mtv-dup    to   rf-zos-des-key         .
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
           if        rf-zos-tip-rec       not  = "mtv "
                     go to aco-250.
           if        rf-zos-des-key       >    w-aux-zos-mtv-dmx
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
           add       1                    to   w-aux-zos-mtv-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso valore                    *
      *                      *-----------------------------------------*
           if        w-aux-zos-mtv-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-mtv-cbu
                                              (w-aux-zos-mtv-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-mtv-mbu
                                              (w-aux-zos-mtv-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-mtv-dbu
                                              (w-aux-zos-mtv-crb)     .
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
           move      "pmtv0011"           to   s-pro                  .
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
           move      w-aux-zos-mtv-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-zos-mtv-c01    to   w-aux-zos-mtv-c02      .
           add       11                   to   w-aux-zos-mtv-c02      .
           divide    12                   into w-aux-zos-mtv-c02      .
           move      w-aux-zos-mtv-c02    to   w-aux-zos-mtv-cpa      .
           subtract  1                    from w-aux-zos-mtv-c02      .
           multiply  12                   by   w-aux-zos-mtv-c02      .
           add       1                    to   w-aux-zos-mtv-c02      .
           add       11
                     w-aux-zos-mtv-c02  giving w-aux-zos-mtv-c03      .
           move      w-aux-zos-mtv-c03    to   w-aux-zos-mtv-c04      .
           if        w-aux-zos-mtv-c03    >    w-aux-zos-mtv-crb
                     move  w-aux-zos-mtv-crb
                                          to   w-aux-zos-mtv-c03      .
           move      07                   to   w-aux-zos-mtv-c05      .
       aco-951.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-zos-mtv-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-aux-zos-mtv-cbu
                    (w-aux-zos-mtv-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-aux-zos-mtv-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-aux-zos-mtv-mbu
                    (w-aux-zos-mtv-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-zos-mtv-c05    to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-aux-zos-mtv-dbu
                    (w-aux-zos-mtv-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-mtv-c02      .
           add       1                    to   w-aux-zos-mtv-c05      .
           if        w-aux-zos-mtv-c02    not  > w-aux-zos-mtv-c03
                     go to aco-951.
       aco-952.
           if        w-aux-zos-mtv-c02    >    w-aux-zos-mtv-c04
                     go to aco-955.
           if        w-aux-zos-mtv-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-zos-mtv-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-mtv-c02      .
           add       1                    to   w-aux-zos-mtv-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-zos-mtv-cpa    to   w-aux-zos-mtv-lt1      .
           move      w-aux-zos-mtv-cpb    to   w-aux-zos-mtv-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-zos-mtv-ltp    to   v-alf                  .
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
           move      spaces               to   w-cod-zos-mtv-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-zos-mtv-des      .
       ltf-050.
      *              *-------------------------------------------------*
      *              * Se codice filtro a zero : uscita                *
      *              *-------------------------------------------------*
           if        w-cod-zos-mtv-cco    =    zero
                     go to ltf-999.
       ltf-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di codice         *
      *              *-------------------------------------------------*
           if        w-cod-zos-mtv-tco    =    zero
                     go to ltf-200
           else if   w-cod-zos-mtv-tco    =    1
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
           move      "mtv "               to   rf-zos-tip-rec         .
           move      w-cod-zos-mtv-cco    to   rf-zos-cod-flt         .
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
           move      rf-zos-des-flt       to   w-cod-zos-mtv-des      .
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
           move      all   "."            to   w-cod-zos-mtv-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-mtv-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-400.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [mtv]              *
      *              *-------------------------------------------------*
       ltf-425.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV    "         to   f-key                  .
           move      w-cod-zos-mtv-cco    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
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
           move      spaces               to   w-cod-zos-mtv-des      .
           string    "Codice materiale vario = "
                                delimited by   size
                     rf-mtv-alf-mtv
                                delimited by   size
                                          into w-cod-zos-mtv-des      .
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
           move      all   "."            to   w-cod-zos-mtv-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-mtv-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-600.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [zv1]              *
      *              *-------------------------------------------------*
       ltf-625.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-cod-zos-mtv-cco    to   rf-zv1-cod-cla         .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
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
           move      w-cod-zos-mtv-cco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      spaces               to   w-cod-zos-mtv-des      .
           string    "Codice classe merceologica = "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-cod-zos-mtv-des      .
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
           move      all   "."            to   w-cod-zos-mtv-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-mtv-flg      .
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
      *              * Se codice [mtv]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-mtv-num      .
           move      w-acc-sng-cod-alf    to   w-acc-sng-mtv-alf      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-mtv-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-mtv-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-mtv-000      thru acc-sng-mtv-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-mtv-num    to   w-acc-sng-cod-num      .
           move      w-acc-sng-mtv-alf    to   w-acc-sng-cod-alf      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-200.
      *              *-------------------------------------------------*
      *              * Se codice [zv1]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-zv1-cla      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-zv1-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-zv1-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-zv1-000      thru acc-sng-zv1-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-zv1-cla    to   w-acc-sng-cod-num      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di un singolo codice anagrafico [mtv]        *
      *    *-----------------------------------------------------------*
       acc-sng-mtv-000.
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
           if        w-acc-sng-mtv-lin    >    16
                     move  16             to   w-acc-sng-mtv-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-mtv-pos    >    30
                     move  30             to   w-acc-sng-mtv-pos      .
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
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-mtv-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-mtv-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           move      "      Selezione del codice materiale vario        
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
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
       acc-sng-mtv-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-mtv-ope      .
           move      "A"                  to   w-cod-cod-mtv-tac      .
           move      w-acc-sng-mtv-num    to   w-cod-cod-mtv-num      .
           move      w-acc-sng-mtv-alf    to   w-cod-cod-mtv-alf      .
           move      w-acc-sng-mtv-lin    to   w-cod-cod-mtv-lin      .
           add       03                   to   w-cod-cod-mtv-lin      .
           move      w-acc-sng-mtv-pos    to   w-cod-cod-mtv-pos      .
           add       10                   to   w-cod-cod-mtv-pos      .
           move      w-acc-sng-mtv-lin    to   w-cod-cod-mtv-dln      .
           add       05                   to   w-cod-cod-mtv-dln      .
           move      w-acc-sng-mtv-pos    to   w-cod-cod-mtv-dps      .
           add       01                   to   w-cod-cod-mtv-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
       acc-sng-mtv-210.
           perform   cod-cod-mtv-cll-000  thru cod-cod-mtv-cll-999    .
           if        w-cod-cod-mtv-ope    =    "F+"
                     go to acc-sng-mtv-215.
           if        w-cod-cod-mtv-ope    =    "AC"
                     go to acc-sng-mtv-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-mtv-215.
           perform   cod-cod-mtv-foi-000  thru cod-cod-mtv-foi-999    .
           go to     acc-sng-mtv-210.
       acc-sng-mtv-220.
           move      w-cod-cod-mtv-alf    to   v-alf                  .
           move      w-cod-cod-mtv-num    to   v-num                  .
       acc-sng-mtv-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-mtv-num
                     go to acc-sng-mtv-350.
       acc-sng-mtv-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-mtv-num      .
           move      v-alf                to   w-acc-sng-mtv-alf      .
       acc-sng-mtv-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [mtv]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-mtv-num    to   w-let-arc-mtv-num      .
           move      w-acc-sng-mtv-alf    to   w-let-arc-mtv-alf      .
           perform   let-arc-mtv-000      thru let-arc-mtv-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-mtv-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-mtv-num    =    zero
                     go to acc-sng-mtv-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-mtv-flg    not  = spaces
                     go to acc-sng-mtv-200.
       acc-sng-mtv-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
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
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-mtv-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-mtv-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-mtv-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-mtv-num
                     go to acc-sng-mtv-350.
       acc-sng-mtv-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-mtv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di una singola classe [zv1]                  *
      *    *-----------------------------------------------------------*
       acc-sng-zv1-000.
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
           if        w-acc-sng-zv1-lin    >    16
                     move  16             to   w-acc-sng-zv1-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-zv1-pos    >    30
                     move  30             to   w-acc-sng-zv1-pos      .
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
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-zv1-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-zv1-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           move      "       Selezione della classe merceologica        
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
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
       acc-sng-zv1-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zv1-ope      .
           move      w-acc-sng-zv1-cla    to   w-cod-mne-zv1-cla      .
           move      zero                 to   w-cod-mne-zv1-gru      .
           move      zero                 to   w-cod-mne-zv1-sgr      .
           move      w-acc-sng-zv1-lin    to   w-cod-mne-zv1-lin      .
           add       03                   to   w-cod-mne-zv1-lin      .
           move      w-acc-sng-zv1-pos    to   w-cod-mne-zv1-pos      .
           add       10                   to   w-cod-mne-zv1-pos      .
           move      w-acc-sng-zv1-lin    to   w-cod-mne-zv1-dln      .
           add       05                   to   w-cod-mne-zv1-dln      .
           move      w-acc-sng-zv1-pos    to   w-cod-mne-zv1-dps      .
           add       01                   to   w-cod-mne-zv1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
       acc-sng-zv1-210.
           perform   cod-mne-zv1-cll-000  thru cod-mne-zv1-cll-999    .
           if        w-cod-mne-zv1-ope    =    "F+"
                     go to acc-sng-zv1-215.
           if        w-cod-mne-zv1-ope    =    "AC"
                     go to acc-sng-zv1-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-zv1-215.
           perform   cod-mne-zv1-foi-000  thru cod-mne-zv1-foi-999    .
           go to     acc-sng-zv1-210.
       acc-sng-zv1-220.
           move      w-cod-mne-zv1-cla    to   v-num                  .
       acc-sng-zv1-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-zv1-cla
                     go to acc-sng-zv1-350.
       acc-sng-zv1-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-zv1-cla      .
       acc-sng-zv1-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [zv1]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-zv1-cla    to   w-let-arc-zv1-cla      .
           perform   let-arc-zv1-000      thru let-arc-zv1-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-zv1-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-zv1-cla    =    zero
                     go to acc-sng-zv1-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zv1-flg    not  = spaces
                     go to acc-sng-zv1-200.
       acc-sng-zv1-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
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
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zv1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zv1-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-zv1-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-zv1-cla
                     go to acc-sng-zv1-350.
       acc-sng-zv1-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-zv1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [mtv]                            *
      *    *-----------------------------------------------------------*
       let-arc-mtv-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Materia vario a zero             *
      *              *-------------------------------------------------*
           if        w-let-arc-mtv-num    =    zero
                     go to let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMTV    "         to   f-key                  .
           move      w-let-arc-mtv-num    to   rf-mtv-num-mtv         .
           move      "pgm/mtv/fls/ioc/obj/iofmtv"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mtv                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-mtv-400.
       let-arc-mtv-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-mtv-alf-mtv       to   w-let-arc-mtv-alf      .
           move      rf-mtv-des-mtv       to   w-let-arc-mtv-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-mtv-999.
       let-arc-mtv-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-mtv-flg      .
           move      all   "."            to   w-let-arc-mtv-des      .
           go to     let-arc-mtv-600.
       let-arc-mtv-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-mtv-des      .
       let-arc-mtv-600.
           move      spaces               to   w-let-arc-mtv-alf      .
       let-arc-mtv-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zv1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zv1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zv1-cla    =    zero
                     go to let-arc-zv1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zv1-cla    to   rf-zv1-cod-cla         .
           move      "pgm/mtv/fls/ioc/obj/iofzv1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zv1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zv1-400.
       let-arc-zv1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zv1-des-cla       to   w-let-arc-zv1-des      .
           move      rf-zv1-mne-cla       to   w-let-arc-zv1-mne      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zv1-999.
       let-arc-zv1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zv1-flg      .
           move      all   "."            to   w-let-arc-zv1-des      .
           go to     let-arc-zv1-600.
       let-arc-zv1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zv1-des      .
       let-arc-zv1-600.
           move      spaces               to   w-let-arc-zv1-mne      .
       let-arc-zv1-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'mtv'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acodmtv0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/mtv/prg/cpy/acmnzv10.acs"                   .
