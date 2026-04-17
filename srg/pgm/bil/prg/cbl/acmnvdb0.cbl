       Identification Division.
       Program-Id.                                 acmnvdb0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bil                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/12/93    *
      *                       Ultima revisione:    NdK del 09/08/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice voce di bilancio *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-vdb-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-vdb-tpb : Codice tipo bilancio *
      *                                                                *
      *                       w-cod-mne-vdb-liv : Numero livello voce  *
      *                                                                *
      *                       w-cod-mne-vdb-lv1 : Num. voce livello 1  *
      *                       w-cod-mne-vdb-lv2 : Num. voce livello 2  *
      *                       w-cod-mne-vdb-lv3 : Num. voce livello 3  *
      *                       w-cod-mne-vdb-lv4 : Num. voce livello 4  *
      *                       w-cod-mne-vdb-lv5 : Num. voce livello 5  *
      *                       w-cod-mne-vdb-lv6 : Num. voce livello 6  *
      *                                                                *
      *                       w-cod-mne-vdb-lin : linea voce           *
      *                                                                *
      *                       w-cod-mne-vdb-pos : posizione voce       *
      *                                                                *
      *                       w-cod-mne-vdb-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-vdb-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-vdb-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-vdb-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-vdb-lvx : Codice numerico voce *
      *                                           di bilancio del li-  *
      *                                           vello che si sta     *
      *                                           trattando            *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-vdb-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-vdb-lvx : Codice numerico voce *
      *                                           di bilancio del li-  *
      *                                           vello che si sta     *
      *                                           trattando            *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-vdb-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-vdb-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-vdb-lvx : Codice numerico voce *
      *                                           di bilancio del li-  *
      *                                           vello che si sta     *
      *                                           trattando            *
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
      *        * [vbt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bil/fls/rec/rfvbt"                          .

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
       01  w-aux-mne-vdb.
           05  w-aux-mne-vdb-m05          pic  x(05)                  .
           05  w-aux-mne-vdb-c01          pic  9(02)                  .
           05  w-aux-mne-vdb-crb          pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice voce di bilancio        *
      *    *-----------------------------------------------------------*
           copy      "pgm/bil/prg/cpy/acmnvdb0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-vdb
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
           if        w-cod-mne-vdb-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-vdb-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-vdb-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-vdb-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-vdb-ope    =    "A+" or
                     w-cod-mne-vdb-ope    =    "I+" or
                     w-cod-mne-vdb-ope    =    "F+"
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
      *                  * Open file [vbt]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
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
      *                  * Close file [vbt]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
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
                     move  spaces         to   w-cod-mne-vdb-ope      .
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
           move      "pbil2011"           to   s-pro                  .
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
           move      "pbil2000"           to   s-pro                  .
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
           if        w-cod-mne-vdb-liv    =    1
                     move  w-cod-mne-vdb-lv1
                                          to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    2
                     move  w-cod-mne-vdb-lv2
                                          to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    3
                     move  w-cod-mne-vdb-lv3
                                          to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    4
                     move  w-cod-mne-vdb-lv4
                                          to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    5
                     move  w-cod-mne-vdb-lv5
                                          to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    6
                     move  w-cod-mne-vdb-lv6
                                          to   w-cod-mne-vdb-num      .
           move      v-edm                to   w-cod-mne-vdb-edm      .
           move      v-ufk                to   w-cod-mne-vdb-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-vdb-edm    to   v-edm                  .
           move      w-cod-mne-vdb-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-vdb-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-vdb-ufk    to   v-ufk                  .
           move      w-cod-mne-vdb-lin    to   v-lin                  .
           move      w-cod-mne-vdb-pos    to   v-pos                  .
           move      w-cod-mne-vdb-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-vdb-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-vdb-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-vdb-ope    =    "F+"
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
           move      w-cod-mne-vdb-ufk    to   v-ufk                  .
           move      w-cod-mne-vdb-lin    to   v-lin                  .
           move      w-cod-mne-vdb-pos    to   v-pos                  .
           move      w-cod-mne-vdb-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-vdb-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-vdb-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-vdb-alf      .
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
           move      w-cod-mne-vdb-lin    to   v-lin                  .
           move      w-cod-mne-vdb-pos    to   v-pos                  .
           move      w-cod-mne-vdb-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-vdb-num    to   w-cod-mne-vdb-lvx      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-vdb-dln      .
           move      zero                 to   w-cod-mne-vdb-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select per la voce di  *
      *                  * bilancio                                    *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-vdb"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-vdb-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-vdb-edm    to   v-edm                  .
           move      w-cod-mne-vdb-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-vdb-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-vdb-ope      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore numerico editato *
      *                      * e preparazione valore in uscita         *
      *                      *-----------------------------------------*
           go to     aco-050.
       aco-175.
      *              *-------------------------------------------------*
      *              * Test se blanks embedded                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-vdb-alf    =    spaces
                     move  zero           to   w-cod-mne-vdb-num
                     go to aco-075.
           if        w-cod-mne-vdb-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-vdb-c01      .
       aco-176.
           add       1                    to   w-aux-mne-vdb-c01      .
           if        w-aux-mne-vdb-c01    >    05
                     go to aco-180.
           if        w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-vdb-c01      .
           if        w-aux-mne-vdb-c01    >    05
                     go to aco-180.
           if        w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-vdb-num      .
           move      zero                 to   w-aux-mne-vdb-c01      .
       aco-181.
           add       1                    to   w-aux-mne-vdb-c01      .
           if        w-aux-mne-vdb-c01    >    05
                     go to aco-200.
           if        w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   <    "0" or
                     w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   >    "9"
                     go to aco-210.
           multiply  10                   by   w-cod-mne-vdb-num      .
           move      w-cod-mne-vdb-cha
                    (w-aux-mne-vdb-c01)   to   w-cod-mne-vdb-chn (05) .
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
           move      w-cod-mne-vdb-edm    to   v-edm                  .
           move      w-cod-mne-vdb-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-vdb-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-vdb-alf
                     go to aco-050.
       aco-210.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-vdb-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * livello in corso di trattamento     *
      *                          *-------------------------------------*
           if        w-cod-mne-vdb-liv    =    1
                     go to aco-211
           else if   w-cod-mne-vdb-liv    =    2
                     go to aco-212
           else if   w-cod-mne-vdb-liv    =    3
                     go to aco-213
           else if   w-cod-mne-vdb-liv    =    4
                     go to aco-214
           else if   w-cod-mne-vdb-liv    =    5
                     go to aco-215
           else if   w-cod-mne-vdb-liv    =    6
                     go to aco-216.
       aco-211.
      *                          *-------------------------------------*
      *                          * Se livello 1                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      zero                 to   rf-vbt-vdb-lv1         .
           move      zero                 to   rf-vbt-vdb-lv2         .
           move      zero                 to   rf-vbt-vdb-lv3         .
           move      zero                 to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-212.
      *                          *-------------------------------------*
      *                          * Se livello 2                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      w-cod-mne-vdb-lv1    to   rf-vbt-vdb-lv1         .
           move      zero                 to   rf-vbt-vdb-lv2         .
           move      zero                 to   rf-vbt-vdb-lv3         .
           move      zero                 to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-213.
      *                          *-------------------------------------*
      *                          * Se livello 3                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      w-cod-mne-vdb-lv1    to   rf-vbt-vdb-lv1         .
           move      w-cod-mne-vdb-lv2    to   rf-vbt-vdb-lv2         .
           move      zero                 to   rf-vbt-vdb-lv3         .
           move      zero                 to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-214.
      *                          *-------------------------------------*
      *                          * Se livello 4                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      w-cod-mne-vdb-lv1    to   rf-vbt-vdb-lv1         .
           move      w-cod-mne-vdb-lv2    to   rf-vbt-vdb-lv2         .
           move      w-cod-mne-vdb-lv3    to   rf-vbt-vdb-lv3         .
           move      zero                 to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-215.
      *                          *-------------------------------------*
      *                          * Se livello 5                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      w-cod-mne-vdb-lv1    to   rf-vbt-vdb-lv1         .
           move      w-cod-mne-vdb-lv2    to   rf-vbt-vdb-lv2         .
           move      w-cod-mne-vdb-lv3    to   rf-vbt-vdb-lv3         .
           move      w-cod-mne-vdb-lv4    to   rf-vbt-vdb-lv4         .
           move      zero                 to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-216.
      *                          *-------------------------------------*
      *                          * Se livello 6                        *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "LIVVDB    "         to   f-key                  .
           move      w-cod-mne-vdb-tpb    to   rf-vbt-tip-bil         .
           move      w-cod-mne-vdb-liv    to   rf-vbt-liv-vdb         .
           move      w-cod-mne-vdb-lv1    to   rf-vbt-vdb-lv1         .
           move      w-cod-mne-vdb-lv2    to   rf-vbt-vdb-lv2         .
           move      w-cod-mne-vdb-lv3    to   rf-vbt-vdb-lv3         .
           move      w-cod-mne-vdb-lv4    to   rf-vbt-vdb-lv4         .
           move      w-cod-mne-vdb-lv5    to   rf-vbt-vdb-lv5         .
           move      zero                 to   rf-vbt-vdb-lv6         .
           move      spaces               to   rf-vbt-mne-vdb         .
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
           go to     aco-220.
       aco-220.
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
           move      "pgm/bil/fls/ioc/obj/iofvbt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vbt                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * livello in corso di trattamento     *
      *                          *-------------------------------------*
           if        w-cod-mne-vdb-liv    =    1
                     go to aco-231
           else if   w-cod-mne-vdb-liv    =    2
                     go to aco-232
           else if   w-cod-mne-vdb-liv    =    3
                     go to aco-233
           else if   w-cod-mne-vdb-liv    =    4
                     go to aco-234
           else if   w-cod-mne-vdb-liv    =    5
                     go to aco-235
           else if   w-cod-mne-vdb-liv    =    6
                     go to aco-236.
       aco-231.
      *                          *-------------------------------------*
      *                          * Se livello 1                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv
                     go to aco-250.
           go to     aco-240.
       aco-232.
      *                          *-------------------------------------*
      *                          * Se livello 2                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv or
                     rf-vbt-vdb-lv1       not  = w-cod-mne-vdb-lv1
                     go to aco-250.
           go to     aco-240.
       aco-233.
      *                          *-------------------------------------*
      *                          * Se livello 3                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv or
                     rf-vbt-vdb-lv1       not  = w-cod-mne-vdb-lv1 or
                     rf-vbt-vdb-lv2       not  = w-cod-mne-vdb-lv2
                     go to aco-250.
           go to     aco-240.
       aco-234.
      *                          *-------------------------------------*
      *                          * Se livello 4                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv or
                     rf-vbt-vdb-lv1       not  = w-cod-mne-vdb-lv1 or
                     rf-vbt-vdb-lv2       not  = w-cod-mne-vdb-lv2 or
                     rf-vbt-vdb-lv3       not  = w-cod-mne-vdb-lv3
                     go to aco-250.
           go to     aco-240.
       aco-235.
      *                          *-------------------------------------*
      *                          * Se livello 5                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv or
                     rf-vbt-vdb-lv1       not  = w-cod-mne-vdb-lv1 or
                     rf-vbt-vdb-lv2       not  = w-cod-mne-vdb-lv2 or
                     rf-vbt-vdb-lv3       not  = w-cod-mne-vdb-lv3 or
                     rf-vbt-vdb-lv4       not  = w-cod-mne-vdb-lv4
                     go to aco-250.
           go to     aco-240.
       aco-236.
      *                          *-------------------------------------*
      *                          * Se livello 6                        *
      *                          *-------------------------------------*
           if        rf-vbt-tip-bil       not  = w-cod-mne-vdb-tpb or
                     rf-vbt-liv-vdb       not  = w-cod-mne-vdb-liv or
                     rf-vbt-vdb-lv1       not  = w-cod-mne-vdb-lv1 or
                     rf-vbt-vdb-lv2       not  = w-cod-mne-vdb-lv2 or
                     rf-vbt-vdb-lv3       not  = w-cod-mne-vdb-lv3 or
                     rf-vbt-vdb-lv4       not  = w-cod-mne-vdb-lv4 or
                     rf-vbt-vdb-lv5       not  = w-cod-mne-vdb-lv5
                     go to aco-250.
           go to     aco-240.
       aco-240.
      *                      *-----------------------------------------*
      *                      * Selezione sul record                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Mnemonico letto in area di lavoro   *
      *                          * di 5 caratteri                      *
      *                          *-------------------------------------*
           move      rf-vbt-mne-vdb       to   w-aux-mne-vdb-m05      .
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-aux-mne-vdb-m05    not  = w-cod-mne-vdb-alf
                     go to aco-230.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-vdb-crb      .
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-vdb-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-vdb-crb    =    1
                     go to aco-300.
       aco-275.
      *                  *---------------------------------------------*
      *                  * Se nessun record con il mnemonico impostato *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione del segnale di record   *
      *                      * non trovato                             *
      *                      *-----------------------------------------*
           if        w-cod-mne-vdb-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-cod-mne-vdb-dln    to   v-lin                  .
           move      w-cod-mne-vdb-dps    to   v-pos                  .
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
      *                      * Codice voce in valore numerico          *
      *                      *-----------------------------------------*
           if        w-cod-mne-vdb-liv    =    1
                     move  rf-vbt-vdb-lv1 to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    2
                     move  rf-vbt-vdb-lv2 to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    3
                     move  rf-vbt-vdb-lv3 to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    4
                     move  rf-vbt-vdb-lv4 to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    5
                     move  rf-vbt-vdb-lv5 to   w-cod-mne-vdb-num
           else if   w-cod-mne-vdb-liv    =    6
                     move  rf-vbt-vdb-lv6 to   w-cod-mne-vdb-num      .
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione ed uscita             *
      *                      *-----------------------------------------*
           go to     aco-200.
       aco-999.
           exit.
