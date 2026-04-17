       Identification Division.
       Program-Id.                                 azosdpm0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dpm                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 20/09/93    *
      *                       Ultima revisione:    Ndk del 04/05/00    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice filtro di ordi-  *
      *                    namento e selezione per file [dpm]          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "AC"                 *
      *                                                                *
      *                       w-cod-zos-dpm-cod : codice filtro        *
      *                                                                *
      *                       w-cod-zos-dpm-lin : linea codice         *
      *                                                                *
      *                       w-cod-zos-dpm-pos : posizione codice     *
      *                                                                *
      *                       w-cod-zos-dpm-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-zos-dpm-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dpm-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dpm-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-zos-dpm-cod : codice filtro        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "LF"  Lettura filtro per ottenimento descrizione               *
      *                                                                *
      *              Input  : w-cod-zos-dpm-ope : "LF"                 *
      *                                                                *
      *                       w-cod-zos-dpm-cod : codice filtro        *
      *                                                                *
      *                                                                *
      *              Output : w-cod-zos-dpm-flg : Flag di esistenza    *
      *                                            - Spaces : Si       *
      *                                            - #      : No       *
      *                                                                *
      *                       w-cod-zos-dpm-des : Descrizione filtro   *
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
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .
      *        *-------------------------------------------------------*
      *        * [zm1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzm1"                          .

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
       01  w-aux-zos-dpm.
           05  w-aux-zos-dpm-flt          pic  x(04) value     "dpm " .
           05  w-aux-zos-dpm-c01          pic  9(02)                  .
           05  w-aux-zos-dpm-c02          pic  9(02)                  .
           05  w-aux-zos-dpm-c03          pic  9(02)                  .
           05  w-aux-zos-dpm-c04          pic  9(02)                  .
           05  w-aux-zos-dpm-c05          pic  9(02)                  .
           05  w-aux-zos-dpm-nli          pic  9(02)                  .
           05  w-aux-zos-dpm-m10          pic  x(10)                  .
           05  w-aux-zos-dpm-tpf          pic  x(01)                  .
           05  w-aux-zos-dpm-crb          pic  9(02)                  .
           05  w-aux-zos-dpm-cpb          pic  9(02)                  .
           05  w-aux-zos-dpm-cpa          pic  9(02)                  .
           05  w-aux-zos-dpm-buf
                               occurs 30.
               10  w-aux-zos-dpm-cbu      pic  9(07)                  .
               10  w-aux-zos-dpm-mbu      pic  x(10)                  .
               10  w-aux-zos-dpm-dbu.
                   20  w-aux-zos-dpm-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
           05  w-aux-zos-dpm-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-zos-dpm-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-zos-dpm-lt2      pic  9(01)                  .
           05  w-aux-zos-dpm-dup          pic  x(40)                  .
           05  w-aux-zos-dpm-dmx.
               10  w-aux-zos-dpm-dch
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
      *    * Work per l'accettazione di una singola materia prima      *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-dpm.
           05  w-acc-sng-dpm-num          pic  9(07)                  .
           05  w-acc-sng-dpm-alf          pic  x(14)                  .
           05  w-acc-sng-dpm-lin          pic  9(02)                  .
           05  w-acc-sng-dpm-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per l'accettazione di una singola classe             *
      *    *-----------------------------------------------------------*
       01  w-acc-sng-zm1.
           05  w-acc-sng-zm1-cla          pic  9(05)                  .
           05  w-acc-sng-zm1-lin          pic  9(02)                  .
           05  w-acc-sng-zm1-pos          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-num      pic  9(07)                  .
               10  w-let-arc-dpm-alf      pic  x(14)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zm1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zm1.
               10  w-let-arc-zm1-flg      pic  x(01)                  .
               10  w-let-arc-zm1-cla      pic  9(05)                  .
               10  w-let-arc-zm1-mne      pic  x(05)                  .
               10  w-let-arc-zm1-des      pic  x(40)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dpm'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice classe materia prima    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm10.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice filtro selezione e or-  *
      *    * dinamento per file [dpm]                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/azosdpm0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-zos-dpm
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
           if        w-cod-zos-dpm-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-zos-dpm-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-zos-dpm-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-zos-dpm-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-zos-dpm-ope    =    "A+" or
                     w-cod-zos-dpm-ope    =    "I+" or
                     w-cod-zos-dpm-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-zos-dpm-ope    =    "LF"
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
      *                  * Open file [dpm]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                  *---------------------------------------------*
      *                  * Open file [zm1]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice prodotto su *
      *                  * 'dpm'                                       *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-opn-000  thru cod-cod-dpm-opn-999    .
      *                  *---------------------------------------------*
      *                  * Open modulo accettazione codice classe mer- *
      *                  * ceologica materia prima                     *
      *                  *---------------------------------------------*
           perform   cod-mne-zm1-opn-000  thru cod-mne-zm1-opn-999    .
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
      *                  * Close file [dpm]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
      *                  *---------------------------------------------*
      *                  * Close file [zm1]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice prodotto   *
      *                  * su 'dpm'                                    *
      *                  *---------------------------------------------*
           perform   cod-cod-dpm-cls-000  thru cod-cod-dpm-cls-999    .
      *                  *---------------------------------------------*
      *                  * Close modulo accettazione codice classe     *
      *                  * merceologica materia prima                  *
      *                  *---------------------------------------------*
           perform   cod-mne-zm1-cls-000  thru cod-mne-zm1-cls-999    .
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
                     move  spaces         to   w-cod-zos-dpm-ope      .
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
           move      "pdpm0011"           to   s-pro                  .
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
           move      "pdpm0010"           to   s-pro                  .
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
           move      w-cod-zos-dpm-cod    to   w-cod-zos-dpm-num      .
           move      v-edm                to   w-cod-zos-dpm-edm      .
           move      v-ufk                to   w-cod-zos-dpm-ufk      .
       acc-300.
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *                                                 *
      *              * Nota: Se in ingresso si ha gia' un codice ana-  *
      *              *       grafico si propone un singolo puntino     *
      *              *       come default di accettazione              *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-tco    not  = zero
                     move  "."            to   w-cod-zos-dpm-alf
                     go to acc-400.
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dpm-edm    to   v-edm                  .
           move      w-cod-zos-dpm-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-dpm-alf      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-zos-dpm-ufk    to   v-ufk                  .
           move      w-cod-zos-dpm-lin    to   v-lin                  .
           move      w-cod-zos-dpm-pos    to   v-pos                  .
           move      w-cod-zos-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Attivazione function-key speciali               *
      *              *-------------------------------------------------*
           move      "[1] "               to   v-pfk (14)             .
           move      "[3] "               to   v-pfk (16)             .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-dpm-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find o da Insr               *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-ope    =    "F+" or
                     w-cod-zos-dpm-ope    =    "I+"
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
           move      w-cod-zos-dpm-ufk    to   v-ufk                  .
           move      w-cod-zos-dpm-lin    to   v-lin                  .
           move      w-cod-zos-dpm-pos    to   v-pos                  .
           move      w-cod-zos-dpm-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Attivazione function-key speciali               *
      *              *-------------------------------------------------*
           move      "[1] "               to   v-pfk (14)             .
           move      "[3] "               to   v-pfk (16)             .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-zos-dpm-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-zos-dpm-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-zos-dpm-alf      .
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
           move      w-cod-zos-dpm-lin    to   v-lin                  .
           move      w-cod-zos-dpm-pos    to   v-pos                  .
           if        w-cod-zos-dpm-tco    not  = zero and
                     w-cod-zos-dpm-alf    =    "."
                     move  spaces         to   v-alf
           else      move  w-cod-zos-dpm-alf
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-zos-dpm-num    to   w-cod-zos-dpm-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-dpm-dln      .
           move      zero                 to   w-cod-zos-dpm-dps      .
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
           move      s-num                to   w-cod-zos-dpm-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dpm-edm    to   v-edm                  .
           move      w-cod-zos-dpm-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-zos-dpm-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-zos-dpm-ope      .
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
           move      "F+"                 to   w-cod-zos-dpm-ope      .
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
      *              * Test se funzioni speciali                       *
      *              *-------------------------------------------------*
           if        v-key                =    "[1] "
                     move  ":M"           to   w-cod-zos-dpm-alf
                     go to aco-230
           else if   v-key                =    "[3] "
                     move  ":C"           to   w-cod-zos-dpm-alf
                     go to aco-230
           else      go to aco-180.
       aco-180.
      *              *-------------------------------------------------*
      *              * Test se blanks embedded                         *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-alf    =    spaces
                     move  zero           to   w-cod-zos-dpm-num
                     go to aco-075.
           if        w-cod-zos-dpm-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-zos-dpm-c01      .
       aco-186.
           add       1                    to   w-aux-zos-dpm-c01      .
           if        w-aux-zos-dpm-c01    >    10
                     go to aco-190.
           if        w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   not  = spaces
                     go to aco-186.
       aco-188.
           add       1                    to   w-aux-zos-dpm-c01      .
           if        w-aux-zos-dpm-c01    >    10
                     go to aco-190.
           if        w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   =    spaces
                     go to aco-188
           else      go to aco-025.
       aco-190.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-zos-dpm-num      .
           move      zero                 to   w-aux-zos-dpm-c01      .
       aco-192.
           add       1                    to   w-aux-zos-dpm-c01      .
           if        w-aux-zos-dpm-c01    >    10
                     go to aco-200.
           if        w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   =    spaces
                     go to aco-200.
           if        w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   <    "0" or
                     w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-zos-dpm-num      .
           move      w-cod-zos-dpm-cha
                    (w-aux-zos-dpm-c01)   to   w-cod-zos-dpm-chn (10) .
           go to     aco-192.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-zos-dpm-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-zos-dpm-edm    to   v-edm                  .
           move      w-cod-zos-dpm-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-zos-dpm-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-zos-dpm-alf
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
           if        w-cod-zos-dpm-tco    not  = zero and
                     w-cod-zos-dpm-alf    =    "."
                     move  w-cod-zos-dpm-cod
                                          to   w-cod-zos-dpm-num
                     go to aco-050.
       aco-230.
      *                  *---------------------------------------------*
      *                  * Test se valore ':' per impostazione di un   *
      *                  * singolo codice anagrafico                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-dpm-alf    not  = ": " and
                     w-cod-zos-dpm-alf    not  = ":M" and
                     w-cod-zos-dpm-alf    not  = ":C"
                     go to aco-235.
      *                  *---------------------------------------------*
      *                  * Preparazione e richiamo subroutine di ac-   *
      *                  * cettazione singolo codice anagrafico        *
      *                  *---------------------------------------------*
           if        w-cod-zos-dpm-alf    =    ": " or
                     w-cod-zos-dpm-alf    =    ":M"
                     move  1              to   w-acc-sng-cod-tip
           else      move  2              to   w-acc-sng-cod-tip      .
           move      zero                 to   w-acc-sng-cod-num      .
           move      spaces               to   w-acc-sng-cod-alf      .
           move      w-cod-zos-dpm-lin    to   w-acc-sng-cod-lin      .
           move      w-cod-zos-dpm-pos    to   w-acc-sng-cod-pos      .
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
           move      zero                 to   w-cod-zos-dpm-num      .
           move      spaces               to   w-cod-zos-dpm-alf      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione a spaces    *
      *                      *-----------------------------------------*
           if        w-cod-zos-dpm-lin    =    zero
                     go to aco-233.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dpm-dln    to   v-lin                  .
           move      w-cod-zos-dpm-dps    to   v-pos                  .
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
           move      w-acc-sng-cod-num    to   w-cod-zos-dpm-num      .
           if        w-acc-sng-cod-tip    =    1
                     add   10000000       to   w-cod-zos-dpm-num
           else      add   20000000       to   w-cod-zos-dpm-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico a spaces            *
      *                      *-----------------------------------------*
           move      spaces               to   w-cod-zos-dpm-alf      .
      *                      *-----------------------------------------*
      *                      * A rivisualizzazione ed uscita           *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-235.
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-zos-dpm-alf    =    "-" and
                     w-cod-zos-dpm-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-zos-dpm-tpf      .
           move      zero                 to   w-aux-zos-dpm-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "dpm "               to   rf-zos-tip-rec         .
           move      w-cod-zos-dpm-alf    to   rf-zos-cod-mne         .
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
           if        rf-zos-tip-rec       not  = "dpm "
                     go to aco-250.
           if        rf-zos-cod-mne       not  = w-cod-zos-dpm-alf
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
           move      rf-zos-cod-mne       to   w-aux-zos-dpm-m10      .
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-zos-dpm-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-zos-dpm-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-dpm-cbu
                                              (w-aux-zos-dpm-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-dpm-mbu
                                              (w-aux-zos-dpm-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-dpm-dbu
                                              (w-aux-zos-dpm-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-240.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-zos-dpm-crb    =    zero
                     go to aco-275
           else if   w-aux-zos-dpm-crb    =    1
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
           if        w-cod-zos-dpm-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dpm-dln    to   v-lin                  .
           move      w-cod-zos-dpm-dps    to   v-pos                  .
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
           move      w-aux-zos-dpm-cbu (1)
                                          to   w-cod-zos-dpm-num      .
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
           move      w-aux-zos-dpm-crb    to   w-aux-zos-dpm-cpb      .
           subtract  1                    from w-aux-zos-dpm-cpb      .
           divide    12                   into w-aux-zos-dpm-cpb      .
           add       1                    to   w-aux-zos-dpm-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-zos-dpm-c01      .
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
           move      w-aux-zos-dpm-c01    to   w-aux-zos-dpm-nli      .
       aco-355.
           if        w-aux-zos-dpm-nli    >    12
                     subtract  12         from w-aux-zos-dpm-nli
                     go to aco-355.
           add       06                   to   w-aux-zos-dpm-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-zos-dpm-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-zos-dpm-c01    <    w-aux-zos-dpm-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-zos-dpm-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-zos-dpm-cpa    <    w-aux-zos-dpm-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-zos-dpm-nli    to   v-lin                  .
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
           move      w-aux-zos-dpm-cbu
                    (w-aux-zos-dpm-c01)   to   w-cod-zos-dpm-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-zos-dpm-c01      .
           if        w-aux-zos-dpm-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-zos-dpm-c01    <    w-aux-zos-dpm-crb
                     add   1              to   w-aux-zos-dpm-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-zos-dpm-nli    =    18
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-zos-dpm-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-zos-dpm-cpa      .
       aco-396.
           move      w-aux-zos-dpm-cpa    to   w-aux-zos-dpm-c01      .
           multiply  12                   by   w-aux-zos-dpm-c01      .
           subtract  11                   from w-aux-zos-dpm-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-zos-dpm-tpf    not  = "M"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdpm0011"           to   s-pro                  .
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
           move      w-cod-zos-dpm-alf    to   s-alf                  .
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
           move      spaces               to   w-aux-zos-dpm-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per descrizione              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dpm-dln    to   v-lin                  .
           move      w-cod-zos-dpm-dps    to   v-pos                  .
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
           move      w-cod-zos-dpm-dln    to   v-lin                  .
           move      w-cod-zos-dpm-dps    to   v-pos                  .
           move      w-aux-zos-dpm-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-zos-dpm-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-zos-dpm-dln    to   v-lin                  .
           move      w-cod-zos-dpm-dps    to   v-pos                  .
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
           if        w-aux-zos-dpm-dup    =    spaces
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
           move      w-aux-zos-dpm-dup    to   w-aux-zos-dpm-dmx      .
           move      40                   to   w-aux-zos-dpm-c01      .
       aco-575.
           if        w-aux-zos-dpm-c01    >    zero
                     if    w-aux-zos-dpm-dch
                          (w-aux-zos-dpm-c01)
                                          =    spaces
                           move     "z"   to   w-aux-zos-dpm-dch
                                              (w-aux-zos-dpm-c01)
                           subtract 1     from w-aux-zos-dpm-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-zos-dpm-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      "dpm "               to   rf-zos-tip-rec         .
           move      w-aux-zos-dpm-dup    to   rf-zos-des-key         .
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
           if        rf-zos-tip-rec       not  = "dpm "
                     go to aco-250.
           if        rf-zos-des-key       >    w-aux-zos-dpm-dmx
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
           add       1                    to   w-aux-zos-dpm-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso valore                    *
      *                      *-----------------------------------------*
           if        w-aux-zos-dpm-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zos-cod-flt       to   w-aux-zos-dpm-cbu
                                              (w-aux-zos-dpm-crb)     .
           move      rf-zos-cod-mne       to   w-aux-zos-dpm-mbu
                                              (w-aux-zos-dpm-crb)     .
           move      rf-zos-des-flt       to   w-aux-zos-dpm-dbu
                                              (w-aux-zos-dpm-crb)     .
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
           move      "pdpm0011"           to   s-pro                  .
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
           move      w-aux-zos-dpm-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-zos-dpm-c01    to   w-aux-zos-dpm-c02      .
           add       11                   to   w-aux-zos-dpm-c02      .
           divide    12                   into w-aux-zos-dpm-c02      .
           move      w-aux-zos-dpm-c02    to   w-aux-zos-dpm-cpa      .
           subtract  1                    from w-aux-zos-dpm-c02      .
           multiply  12                   by   w-aux-zos-dpm-c02      .
           add       1                    to   w-aux-zos-dpm-c02      .
           add       11
                     w-aux-zos-dpm-c02  giving w-aux-zos-dpm-c03      .
           move      w-aux-zos-dpm-c03    to   w-aux-zos-dpm-c04      .
           if        w-aux-zos-dpm-c03    >    w-aux-zos-dpm-crb
                     move  w-aux-zos-dpm-crb
                                          to   w-aux-zos-dpm-c03      .
           move      07                   to   w-aux-zos-dpm-c05      .
       aco-951.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-zos-dpm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-aux-zos-dpm-cbu
                    (w-aux-zos-dpm-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      w-aux-zos-dpm-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-aux-zos-dpm-mbu
                    (w-aux-zos-dpm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-zos-dpm-c05    to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-aux-zos-dpm-dbu
                    (w-aux-zos-dpm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-dpm-c02      .
           add       1                    to   w-aux-zos-dpm-c05      .
           if        w-aux-zos-dpm-c02    not  > w-aux-zos-dpm-c03
                     go to aco-951.
       aco-952.
           if        w-aux-zos-dpm-c02    >    w-aux-zos-dpm-c04
                     go to aco-955.
           if        w-aux-zos-dpm-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-zos-dpm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-zos-dpm-c02      .
           add       1                    to   w-aux-zos-dpm-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-zos-dpm-cpa    to   w-aux-zos-dpm-lt1      .
           move      w-aux-zos-dpm-cpb    to   w-aux-zos-dpm-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-zos-dpm-ltp    to   v-alf                  .
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
           move      spaces               to   w-cod-zos-dpm-flg      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-zos-dpm-des      .
       ltf-050.
      *              *-------------------------------------------------*
      *              * Se codice filtro a zero : uscita                *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-cco    =    zero
                     go to ltf-999.
       ltf-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di codice         *
      *              *-------------------------------------------------*
           if        w-cod-zos-dpm-tco    =    zero
                     go to ltf-200
           else if   w-cod-zos-dpm-tco    =    1
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
           move      "dpm "               to   rf-zos-tip-rec         .
           move      w-cod-zos-dpm-cco    to   rf-zos-cod-flt         .
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
           move      rf-zos-des-flt       to   w-cod-zos-dpm-des      .
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
           move      all   "."            to   w-cod-zos-dpm-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dpm-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-400.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [dpm]              *
      *              *-------------------------------------------------*
       ltf-425.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      w-cod-zos-dpm-cco    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
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
           move      spaces               to   w-cod-zos-dpm-des      .
           string    "Codice materia prima = "
                                delimited by   size
                     rf-dpm-alf-map
                                delimited by   size
                                          into w-cod-zos-dpm-des      .
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
           move      all   "."            to   w-cod-zos-dpm-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dpm-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ltf-999.
       ltf-600.
      *              *-------------------------------------------------*
      *              * Se singolo codice anagrafico [zm1]              *
      *              *-------------------------------------------------*
       ltf-625.
      *                  *---------------------------------------------*
      *                  * Lettura codice anagrafico                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-cod-zos-dpm-cco    to   rf-zm1-cod-cla         .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
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
           move      w-cod-zos-dpm-cco    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      spaces               to   w-cod-zos-dpm-des      .
           string    "Codice classe merceologica = "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                                          into w-cod-zos-dpm-des      .
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
           move      all   "."            to   w-cod-zos-dpm-des      .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza a : No                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cod-zos-dpm-flg      .
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
      *              * Se codice [dpm]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-dpm-num      .
           move      w-acc-sng-cod-alf    to   w-acc-sng-dpm-alf      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-dpm-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-dpm-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-dpm-000      thru acc-sng-dpm-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-dpm-num    to   w-acc-sng-cod-num      .
           move      w-acc-sng-dpm-alf    to   w-acc-sng-cod-alf      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-200.
      *              *-------------------------------------------------*
      *              * Se codice [zm1]                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prelievo parametri                          *
      *                  *---------------------------------------------*
           move      w-acc-sng-cod-num    to   w-acc-sng-zm1-cla      .
           move      w-acc-sng-cod-lin    to   w-acc-sng-zm1-lin      .
           move      w-acc-sng-cod-pos    to   w-acc-sng-zm1-pos      .
      *                  *---------------------------------------------*
      *                  * Richiamo specifica subroutine               *
      *                  *---------------------------------------------*
           perform   acc-sng-zm1-000      thru acc-sng-zm1-999        .
      *                  *---------------------------------------------*
      *                  * Restituzione parametri                      *
      *                  *---------------------------------------------*
           move      w-acc-sng-zm1-cla    to   w-acc-sng-cod-num      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-sng-cod-999.
       acc-sng-cod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di un singolo codice anagrafico [mtv]        *
      *    *-----------------------------------------------------------*
       acc-sng-dpm-000.
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
           if        w-acc-sng-dpm-lin    >    16
                     move  16             to   w-acc-sng-dpm-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-dpm-pos    >    30
                     move  30             to   w-acc-sng-dpm-pos      .
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
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-dpm-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-dpm-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           move      "       Selezione del codice materia prima         
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
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
       acc-sng-dpm-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpm-ope      .
           move      "A"                  to   w-cod-cod-dpm-tac      .
           move      w-acc-sng-dpm-num    to   w-cod-cod-dpm-num      .
           move      w-acc-sng-dpm-alf    to   w-cod-cod-dpm-alf      .
           move      w-acc-sng-dpm-lin    to   w-cod-cod-dpm-lin      .
           add       03                   to   w-cod-cod-dpm-lin      .
           move      w-acc-sng-dpm-pos    to   w-cod-cod-dpm-pos      .
           add       10                   to   w-cod-cod-dpm-pos      .
           move      w-acc-sng-dpm-lin    to   w-cod-cod-dpm-dln      .
           add       05                   to   w-cod-cod-dpm-dln      .
           move      w-acc-sng-dpm-pos    to   w-cod-cod-dpm-dps      .
           add       01                   to   w-cod-cod-dpm-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
       acc-sng-dpm-210.
           perform   cod-cod-dpm-cll-000  thru cod-cod-dpm-cll-999    .
           if        w-cod-cod-dpm-ope    =    "F+"
                     go to acc-sng-dpm-215.
           if        w-cod-cod-dpm-ope    =    "AC"
                     go to acc-sng-dpm-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-dpm-215.
           perform   cod-cod-dpm-foi-000  thru cod-cod-dpm-foi-999    .
           go to     acc-sng-dpm-210.
       acc-sng-dpm-220.
           move      w-cod-cod-dpm-alf    to   v-alf                  .
           move      w-cod-cod-dpm-num    to   v-num                  .
       acc-sng-dpm-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-dpm-num
                     go to acc-sng-dpm-350.
       acc-sng-dpm-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-dpm-num      .
           move      v-alf                to   w-acc-sng-dpm-alf      .
       acc-sng-dpm-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dpm]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-dpm-num    to   w-let-arc-dpm-num      .
           move      w-acc-sng-dpm-alf    to   w-let-arc-dpm-alf      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-dpm-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-dpm-num    =    zero
                     go to acc-sng-dpm-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-flg    not  = spaces
                     go to acc-sng-dpm-200.
       acc-sng-dpm-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
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
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-dpm-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-dpm-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-dpm-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-dpm-num
                     go to acc-sng-dpm-350.
       acc-sng-dpm-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-dpm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione di una singola classe [zm1]                  *
      *    *-----------------------------------------------------------*
       acc-sng-zm1-000.
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
           if        w-acc-sng-zm1-lin    >    16
                     move  16             to   w-acc-sng-zm1-lin      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della posizione di    *
      *              * accettazione                                    *
      *              *-------------------------------------------------*
           if        w-acc-sng-zm1-pos    >    30
                     move  30             to   w-acc-sng-zm1-pos      .
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
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           subtract  01                   from v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           subtract  01                   from v-pos                  .
           move      w-acc-sng-zm1-lin    to   v-lto                  .
           add       07                   to   v-lto                  .
           move      w-acc-sng-zm1-pos    to   v-pto                  .
           add       50                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           move      "       Selezione della classe merceologica        
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       01                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           move      " ------------------------------------------------ 
      -              ""                   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       03                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
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
       acc-sng-zm1-200.
      *              *-------------------------------------------------*
      *              * Accettazione del codice anagrafico              *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zm1-ope      .
           move      w-acc-sng-zm1-cla    to   w-cod-mne-zm1-cla      .
           move      zero                 to   w-cod-mne-zm1-gru      .
           move      zero                 to   w-cod-mne-zm1-sgr      .
           move      w-acc-sng-zm1-lin    to   w-cod-mne-zm1-lin      .
           add       03                   to   w-cod-mne-zm1-lin      .
           move      w-acc-sng-zm1-pos    to   w-cod-mne-zm1-pos      .
           add       10                   to   w-cod-mne-zm1-pos      .
           move      w-acc-sng-zm1-lin    to   w-cod-mne-zm1-dln      .
           add       05                   to   w-cod-mne-zm1-dln      .
           move      w-acc-sng-zm1-pos    to   w-cod-mne-zm1-dps      .
           add       01                   to   w-cod-mne-zm1-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-zm1-cll-000  thru cod-mne-zm1-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-mne-zm1-foi-000  thru cod-mne-zm1-foi-999    .
       acc-sng-zm1-210.
           perform   cod-mne-zm1-cll-000  thru cod-mne-zm1-cll-999    .
           if        w-cod-mne-zm1-ope    =    "F+"
                     go to acc-sng-zm1-215.
           if        w-cod-mne-zm1-ope    =    "AC"
                     go to acc-sng-zm1-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-sng-zm1-215.
           perform   cod-mne-zm1-foi-000  thru cod-mne-zm1-foi-999    .
           go to     acc-sng-zm1-210.
       acc-sng-zm1-220.
           move      w-cod-mne-zm1-cla    to   v-num                  .
       acc-sng-zm1-250.
      *              *-------------------------------------------------*
      *              * Se Exit o Up                                    *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT" or
                     v-key                =    "UP  "
                     move  zero           to   w-acc-sng-zm1-cla
                     go to acc-sng-zm1-350.
       acc-sng-zm1-300.
      *              *-------------------------------------------------*
      *              * Valore impostato in destinazione                *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-sng-zm1-cla      .
       acc-sng-zm1-350.
      *              *-------------------------------------------------*
      *              * Lettura archivio [zm1]                          *
      *              *-------------------------------------------------*
           move      w-acc-sng-zm1-cla    to   w-let-arc-zm1-cla      .
           perform   let-arc-zm1-000      thru let-arc-zm1-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione                     *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           add       01                   to   v-pos                  .
           move      w-let-arc-zm1-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-acc-sng-zm1-cla    =    zero
                     go to acc-sng-zm1-900.
      *              *-------------------------------------------------*
      *              * Se codice non esistente : si ricicla all'accet- *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           if        w-let-arc-zm1-flg    not  = spaces
                     go to acc-sng-zm1-200.
       acc-sng-zm1-400.
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
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
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           add       47                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione parentesi quadre di delimitazione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-acc-sng-zm1-lin    to   v-lin                  .
           add       05                   to   v-lin                  .
           move      w-acc-sng-zm1-pos    to   v-pos                  .
           add       46                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Up : a reimpostazione                        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-sng-zm1-200.
      *              *-------------------------------------------------*
      *              * Se Exit : normalizzazione ed uscita             *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  zero           to   w-acc-sng-zm1-cla
                     go to acc-sng-zm1-350.
       acc-sng-zm1-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-sng-zm1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dpm]                            *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Materia prima a zero             *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-num    =    zero
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP    "         to   f-key                  .
           move      w-let-arc-dpm-num    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-alf-map       to   w-let-arc-dpm-alf      .
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
           move      all   "."            to   w-let-arc-dpm-des      .
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-alf      .
       let-arc-dpm-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zm1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zm1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zm1-cla    =    zero
                     go to let-arc-zm1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zm1-cla    to   rf-zm1-cod-cla         .
           move      "pgm/dpm/fls/ioc/obj/iofzm1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zm1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zm1-400.
       let-arc-zm1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zm1-des-cla       to   w-let-arc-zm1-des      .
           move      rf-zm1-mne-cla       to   w-let-arc-zm1-mne      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zm1-999.
       let-arc-zm1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zm1-flg      .
           move      all   "."            to   w-let-arc-zm1-des      .
           go to     let-arc-zm1-600.
       let-arc-zm1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zm1-des      .
       let-arc-zm1-600.
           move      spaces               to   w-let-arc-zm1-mne      .
       let-arc-zm1-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dpm'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acoddpm0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice classe          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzm10.acs"                   .
