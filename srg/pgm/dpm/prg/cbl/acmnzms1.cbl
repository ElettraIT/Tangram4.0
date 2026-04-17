       Identification Division.
       Program-Id.                                 acmnzms1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dpm                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/05/91    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice statistico 1 per *
      *                    classificazioni statistiche materie prime   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-zms-001-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "AC"                 *
      *                                                                *
      *                       w-cmn-zms-001-cod : codice statistico 1  *
      *                                                                *
      *                       w-cmn-zms-001-lin : linea codice         *
      *                                                                *
      *                       w-cmn-zms-001-pos : posizione codice     *
      *                                                                *
      *                       w-cmn-zms-001-dln : linea descrizione    *
      *                                                                *
      *                       w-cmn-zms-001-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-zms-001-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-zms-001-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cmn-zms-001-cod : codice statistico 1  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-zms-001-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cmn-zms-001-cod : codice statistico 1  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cmn-zms-001-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cmn-zms-001-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cmn-zms-001-cod : codice statistico 1  *
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
      *        * [zms]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfzms"                          .

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
       01  w-aux-mne-zms.
           05  w-aux-mne-zms-c01          pic  9(02)                  .
           05  w-aux-mne-zms-c02          pic  9(02)                  .
           05  w-aux-mne-zms-c03          pic  9(02)                  .
           05  w-aux-mne-zms-c04          pic  9(02)                  .
           05  w-aux-mne-zms-c05          pic  9(02)                  .
           05  w-aux-mne-zms-nli          pic  9(02)                  .
           05  w-aux-mne-zms-m05          pic  x(05)                  .
           05  w-aux-mne-zms-tpf          pic  x(01)                  .
           05  w-aux-mne-zms-crb          pic  9(02)                  .
           05  w-aux-mne-zms-cpb          pic  9(02)                  .
           05  w-aux-mne-zms-cpa          pic  9(02)                  .
           05  w-aux-mne-zms-buf
                               occurs 30.
               10  w-aux-mne-zms-cbu      pic  9(05)                  .
               10  w-aux-mne-zms-mbu      pic  x(05)                  .
               10  w-aux-mne-zms-dbu.
                   15  w-aux-mne-zms-fbu  pic  x(01)                  .
                   15  filler             pic  x(19)                  .
               10  w-aux-mne-zms-bbu      pic  x(01)                  .
               10  w-aux-mne-zms-tbu      pic  9(01)                  .
           05  w-aux-mne-zms-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-zms-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-zms-lt2      pic  9(01)                  .
           05  w-aux-mne-zms-dup          pic  x(20)                  .
           05  w-aux-mne-zms-rmx.
               10  w-aux-mne-zms-dch
                               occurs 20  pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice statistico 1            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dpm/prg/cpy/acmnzms1.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cmn-zms-001
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
           if        w-cmn-zms-001-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cmn-zms-001-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cmn-zms-001-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cmn-zms-001-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cmn-zms-001-ope    =    "A+" or
                     w-cmn-zms-001-ope    =    "I+" or
                     w-cmn-zms-001-ope    =    "F+"
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
      *                  *---------------------------------------------*
      *                  * Open file [zms]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
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
      *                  * Close file [zms]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
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
                     move  spaces         to   w-cmn-zms-001-ope      .
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
           move      "pdpm1710"           to   s-pro                  .
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
           move      "pdpm1700"           to   s-pro                  .
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
           move      w-cmn-zms-001-cod    to   w-cmn-zms-001-num      .
           move      v-edm                to   w-cmn-zms-001-edm      .
           move      v-ufk                to   w-cmn-zms-001-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-zms-001-edm    to   v-edm                  .
           move      w-cmn-zms-001-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cmn-zms-001-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-zms-001-ufk    to   v-ufk                  .
           move      w-cmn-zms-001-lin    to   v-lin                  .
           move      w-cmn-zms-001-pos    to   v-pos                  .
           move      w-cmn-zms-001-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cmn-zms-001-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cmn-zms-001-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cmn-zms-001-ope    =    "F+"
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
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-zms-001-ufk    to   v-ufk                  .
           move      w-cmn-zms-001-lin    to   v-lin                  .
           move      w-cmn-zms-001-pos    to   v-pos                  .
           move      w-cmn-zms-001-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cmn-zms-001-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cmn-zms-001-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cmn-zms-001-alf      .
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
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cmn-zms-001-lin    to   v-lin                  .
           move      w-cmn-zms-001-pos    to   v-pos                  .
           move      w-cmn-zms-001-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cmn-zms-001-num    to   w-cmn-zms-001-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmn-zms-001-dln      .
           move      zero                 to   w-cmn-zms-001-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cls"            to   s-var                  .
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
           move      s-num                to   w-cmn-zms-001-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-zms-001-edm    to   v-edm                  .
           move      w-cmn-zms-001-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cmn-zms-001-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cmn-zms-001-ope      .
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
           move      "F+"                 to   w-cmn-zms-001-ope      .
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
           if        w-cmn-zms-001-alf    =    spaces
                     move  zero           to   w-cmn-zms-001-num
                     go to aco-075.
           if        w-cmn-zms-001-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-zms-c01      .
       aco-176.
           add       1                    to   w-aux-mne-zms-c01      .
           if        w-aux-mne-zms-c01    >    05
                     go to aco-180.
           if        w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-zms-c01      .
           if        w-aux-mne-zms-c01    >    05
                     go to aco-180.
           if        w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmn-zms-001-num      .
           move      zero                 to   w-aux-mne-zms-c01      .
       aco-181.
           add       1                    to   w-aux-mne-zms-c01      .
           if        w-aux-mne-zms-c01    >    05
                     go to aco-200.
           if        w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   =    spaces
                     go to aco-200.
           if        w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   <    "0" or
                     w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cmn-zms-001-num      .
           move      w-cmn-zms-001-cha
                    (w-aux-mne-zms-c01)   to   w-cmn-zms-001-chn (05) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cmn-zms-001-edm    to   v-edm                  .
           move      w-cmn-zms-001-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cmn-zms-001-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cmn-zms-001-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cmn-zms-001-alf    =    "-" and
                     w-cmn-zms-001-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-zms-tpf      .
           move      zero                 to   w-aux-mne-zms-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-zms-tip-cls         .
           move      w-cmn-zms-001-alf    to   rf-zms-mne-cls         .
           move      zero                 to   rf-zms-cod-cls         .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-230.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per mnemonico       *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
           if        rf-zms-tip-cls       not  = 01
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 5  *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-zms-mne-cls       to   w-aux-mne-zms-m05      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-zms-m05    not  = w-cmn-zms-001-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zms-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zms-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zms-cod-cls       to   w-aux-mne-zms-cbu
                                              (w-aux-mne-zms-crb)     .
           move      rf-zms-mne-cls       to   w-aux-mne-zms-mbu
                                              (w-aux-mne-zms-crb)     .
           move      rf-zms-des-cls       to   w-aux-mne-zms-dbu
                                              (w-aux-mne-zms-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-zms-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-zms-crb    =    1
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
           if        w-cmn-zms-001-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cmn-zms-001-dln    to   v-lin                  .
           move      w-cmn-zms-001-dps    to   v-pos                  .
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
           move      w-aux-mne-zms-cbu (1)
                                          to   w-cmn-zms-001-num      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione ed uscita             *
      *                      *-----------------------------------------*
           go to     aco-200.
       aco-325.
      *                  *---------------------------------------------*
      *                  * Se non piu' di trenta records con lo stesso *
      *                  * mnemonico                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-aux-mne-zms-crb    to   w-aux-mne-zms-cpb      .
           subtract  1                    from w-aux-mne-zms-cpb      .
           divide    6                    into w-aux-mne-zms-cpb      .
           add       1                    to   w-aux-mne-zms-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-zms-c01      .
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
           move      07                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "        Selezionare il codice statistico desiderat
      -              "o         "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      16                   to   v-lin                  .
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
           move      w-aux-mne-zms-c01    to   w-aux-mne-zms-nli      .
       aco-355.
           if        w-aux-mne-zms-nli    >    6
                     subtract  6          from w-aux-mne-zms-nli
                     go to aco-355.
           add       09                   to   w-aux-mne-zms-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           if        w-aux-mne-zms-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-zms-c01    <    w-aux-mne-zms-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-zms-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-zms-cpa    <    w-aux-mne-zms-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-zms-nli    to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-aux-mne-zms-fbu
                    (w-aux-mne-zms-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-alf                =    w-aux-mne-zms-fbu
                                              (w-aux-mne-zms-c01)
                     go to aco-380.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-aux-mne-zms-nli    to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-aux-mne-zms-fbu
                    (w-aux-mne-zms-c01)   to   v-alf                  .
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
           move      w-aux-mne-zms-cbu
                    (w-aux-mne-zms-c01)   to   w-cmn-zms-001-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-zms-c01      .
           if        w-aux-mne-zms-nli    =    10
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-zms-c01    <    w-aux-mne-zms-crb
                     add   1              to   w-aux-mne-zms-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-zms-nli    =    15
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-zms-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-zms-cpa      .
       aco-396.
           move      w-aux-mne-zms-cpa    to   w-aux-mne-zms-c01      .
           multiply  6                    by   w-aux-mne-zms-c01      .
           subtract  5                    from w-aux-mne-zms-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-zms-tpf    =    "D"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdpm1710"           to   s-pro                  .
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
           move      05                   to   s-car                  .
           move      w-cmn-zms-001-alf    to   s-alf                  .
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
           move      spaces               to   w-aux-mne-zms-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cmn-zms-001-dln    to   v-lin                  .
           move      w-cmn-zms-001-dps    to   v-pos                  .
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
           move      w-cmn-zms-001-dln    to   v-lin                  .
           move      w-cmn-zms-001-dps    to   v-pos                  .
           move      w-aux-mne-zms-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-zms-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cmn-zms-001-dln    to   v-lin                  .
           move      w-cmn-zms-001-dps    to   v-pos                  .
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
           if        w-aux-mne-zms-dup    =    spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione massima        *
      *                      *-----------------------------------------*
           move      w-aux-mne-zms-dup    to   w-aux-mne-zms-rmx      .
           move      20                   to   w-aux-mne-zms-c01      .
       aco-575.
           if        w-aux-mne-zms-c01    >    zero
                     if    w-aux-mne-zms-dch
                          (w-aux-mne-zms-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-zms-dch
                                              (w-aux-mne-zms-c01)
                           subtract 1     from w-aux-mne-zms-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con la    *
      *                      * stessa descrizione nel buffer           *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-zms-tpf      .
           move      zero                 to   w-aux-mne-zms-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      01                   to   rf-zms-tip-cls         .
           move      w-aux-mne-zms-dup    to   rf-zms-des-key         .
           move      zero                 to   rf-zms-cod-cls         .
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
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
           move      "pgm/dpm/fls/ioc/obj/iofzms"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zms                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
           if        rf-zms-tip-cls       not  = 01
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zms-des-key       >    w-aux-mne-zms-rmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zms-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zms-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zms-cod-cls       to   w-aux-mne-zms-cbu
                                              (w-aux-mne-zms-crb)     .
           move      rf-zms-mne-cls       to   w-aux-mne-zms-mbu
                                              (w-aux-mne-zms-crb)     .
           move      rf-zms-des-cls       to   w-aux-mne-zms-dbu
                                              (w-aux-mne-zms-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-600.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Se piu' di trenta record con la stessa  *
      *                      * descrizione impostata                   *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdpm1710"           to   s-pro                  .
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
           move      "des-cls"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-zms-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-zms-c01    to   w-aux-mne-zms-c02      .
           add       5                    to   w-aux-mne-zms-c02      .
           divide    6                    into w-aux-mne-zms-c02      .
           move      w-aux-mne-zms-c02    to   w-aux-mne-zms-cpa      .
           subtract  1                    from w-aux-mne-zms-c02      .
           multiply  6                    by   w-aux-mne-zms-c02      .
           add       1                    to   w-aux-mne-zms-c02      .
           add       5
                     w-aux-mne-zms-c02  giving w-aux-mne-zms-c03      .
           move      w-aux-mne-zms-c03    to   w-aux-mne-zms-c04      .
           if        w-aux-mne-zms-c03    >    w-aux-mne-zms-crb
                     move  w-aux-mne-zms-crb
                                          to   w-aux-mne-zms-c03      .
           move      10                   to   w-aux-mne-zms-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-mne-zms-c05    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-aux-mne-zms-mbu
                    (w-aux-mne-zms-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-zms-c05    to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-aux-mne-zms-cbu
                    (w-aux-mne-zms-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-aux-mne-zms-c05    to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-aux-mne-zms-dbu
                    (w-aux-mne-zms-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zms-c02      .
           add       1                    to   w-aux-mne-zms-c05      .
           if        w-aux-mne-zms-c02    not  > w-aux-mne-zms-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-zms-c02    >    w-aux-mne-zms-c04
                     go to aco-955.
           if        w-aux-mne-zms-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-zms-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zms-c02      .
           add       1                    to   w-aux-mne-zms-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-zms-cpa    to   w-aux-mne-zms-lt1      .
           move      w-aux-mne-zms-cpb    to   w-aux-mne-zms-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-zms-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.
