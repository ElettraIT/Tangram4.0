       Identification Division.
       Program-Id.                                 acmnzp30           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 29/05/91    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice sottogruppo per  *
      *                    classificazioni gerarchiche prodotti        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zp3-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-zp3-cla : codice classe        *
      *                                                                *
      *                       w-cod-mne-zp3-dcl : descr. classe        *
      *                                                                *
      *                       w-cod-mne-zp3-gru : codice gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-dgr : descr. gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-sgr : codice sottogruppo   *
      *                                                                *
      *                       w-cod-mne-zp3-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-zp3-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-zp3-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-zp3-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zp3-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zp3-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zp3-cla : codice classe        *
      *                                                                *
      *                       w-cod-mne-zp3-dcl : descr. classe        *
      *                                                                *
      *                       w-cod-mne-zp3-gru : codice gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-dgr : descr. gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-sgr : codice sottogruppo   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zp3-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zp3-cla : codice classe        *
      *                                                                *
      *                       w-cod-mne-zp3-dcl : descr. classe        *
      *                                                                *
      *                       w-cod-mne-zp3-gru : codice gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-dgr : descr. gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-sgr : codice sottogruppo   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-zp3-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zp3-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zp3-cla : codice classe        *
      *                                                                *
      *                       w-cod-mne-zp3-dcl : descr. classe        *
      *                                                                *
      *                       w-cod-mne-zp3-gru : codice gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-dgr : descr. gruppo        *
      *                                                                *
      *                       w-cod-mne-zp3-sgr : codice sottogruppo   *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .

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
       01  w-aux-mne-sgr.
           05  w-aux-mne-sgr-c01          pic  9(02)                  .
           05  w-aux-mne-sgr-c02          pic  9(02)                  .
           05  w-aux-mne-sgr-c03          pic  9(02)                  .
           05  w-aux-mne-sgr-c04          pic  9(02)                  .
           05  w-aux-mne-sgr-c05          pic  9(02)                  .
           05  w-aux-mne-sgr-nli          pic  9(02)                  .
           05  w-aux-mne-sgr-m05          pic  x(05)                  .
           05  w-aux-mne-sgr-tpf          pic  x(01)                  .
           05  w-aux-mne-sgr-crb          pic  9(02)                  .
           05  w-aux-mne-sgr-cpb          pic  9(02)                  .
           05  w-aux-mne-sgr-cpa          pic  9(02)                  .
           05  w-aux-mne-sgr-buf
                               occurs 30.
               10  w-aux-mne-sgr-cbu      pic  9(05)                  .
               10  w-aux-mne-sgr-mbu      pic  x(05)                  .
               10  w-aux-mne-sgr-dbu.
                   15  w-aux-mne-sgr-fbu  pic  x(01)                  .
                   15  filler             pic  x(39)                  .
               10  w-aux-mne-sgr-bbu      pic  x(01)                  .
               10  w-aux-mne-sgr-tbu      pic  9(01)                  .
           05  w-aux-mne-sgr-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-sgr-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-sgr-lt2      pic  9(01)                  .
           05  w-aux-mne-sgr-dup          pic  x(40)                  .
           05  w-aux-mne-sgr-rmx.
               10  w-aux-mne-sgr-dch
                               occurs 40  pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice sottogruppo prodotto    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acmnzp30.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-zp3
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
           if        w-cod-mne-zp3-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-zp3-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-zp3-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-zp3-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-zp3-ope    =    "A+" or
                     w-cod-mne-zp3-ope    =    "I+" or
                     w-cod-mne-zp3-ope    =    "F+"
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
      *                  * Open file [zp3]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
      *                  * Close file [zp3]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
                     move  spaces         to   w-cod-mne-zp3-ope      .
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
           move      "pdcp1213"           to   s-pro                  .
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
           move      "pdcp1200"           to   s-pro                  .
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
           move      w-cod-mne-zp3-sgr    to   w-cod-mne-zp3-num      .
           move      v-edm                to   w-cod-mne-zp3-edm      .
           move      v-ufk                to   w-cod-mne-zp3-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zp3-edm    to   v-edm                  .
           move      w-cod-mne-zp3-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zp3-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-zp3-ufk    to   v-ufk                  .
           move      w-cod-mne-zp3-lin    to   v-lin                  .
           move      w-cod-mne-zp3-pos    to   v-pos                  .
           move      w-cod-mne-zp3-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zp3-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-zp3-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-zp3-ope    =    "F+"
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
           move      w-cod-mne-zp3-ufk    to   v-ufk                  .
           move      w-cod-mne-zp3-lin    to   v-lin                  .
           move      w-cod-mne-zp3-pos    to   v-pos                  .
           move      w-cod-mne-zp3-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zp3-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zp3-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-zp3-alf      .
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
           move      w-cod-mne-zp3-lin    to   v-lin                  .
           move      w-cod-mne-zp3-pos    to   v-pos                  .
           move      w-cod-mne-zp3-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-zp3-num    to   w-cod-mne-zp3-sgr      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zp3-dln      .
           move      zero                 to   w-cod-mne-zp3-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select per il sotto-   *
      *                  * gruppo                                      *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-sgr"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-zp3-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zp3-edm    to   v-edm                  .
           move      w-cod-mne-zp3-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zp3-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-zp3-ope      .
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
           move      "F+"                 to   w-cod-mne-zp3-ope      .
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
           if        w-cod-mne-zp3-alf    =    spaces
                     move  zero           to   w-cod-mne-zp3-num
                     go to aco-075.
           if        w-cod-mne-zp3-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-sgr-c01      .
       aco-176.
           add       1                    to   w-aux-mne-sgr-c01      .
           if        w-aux-mne-sgr-c01    >    05
                     go to aco-180.
           if        w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-sgr-c01      .
           if        w-aux-mne-sgr-c01    >    05
                     go to aco-180.
           if        w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zp3-num      .
           move      zero                 to   w-aux-mne-sgr-c01      .
       aco-181.
           add       1                    to   w-aux-mne-sgr-c01      .
           if        w-aux-mne-sgr-c01    >    05
                     go to aco-200.
           if        w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   <    "0" or
                     w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-mne-zp3-num      .
           move      w-cod-mne-zp3-cha
                    (w-aux-mne-sgr-c01)   to   w-cod-mne-zp3-chn (05) .
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
           move      w-cod-mne-zp3-edm    to   v-edm                  .
           move      w-cod-mne-zp3-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-zp3-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-zp3-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-mne-zp3-alf    =    "-" and
                     w-cod-mne-zp3-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-sgr-tpf      .
           move      zero                 to   w-aux-mne-sgr-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-zp3-cla    to   rf-zp3-cod-cla         .
           move      w-cod-mne-zp3-gru    to   rf-zp3-cod-gru         .
           move      w-cod-mne-zp3-alf    to   rf-zp3-mne-sgr         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
           if        rf-zp3-cod-cla       not  = w-cod-mne-zp3-cla
                     go to aco-250.
           if        rf-zp3-cod-gru       not  = w-cod-mne-zp3-gru
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 5  *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-zp3-mne-sgr       to   w-aux-mne-sgr-m05      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-sgr-m05    not  = w-cod-mne-zp3-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-sgr-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-sgr-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zp3-cod-sgr       to   w-aux-mne-sgr-cbu
                                              (w-aux-mne-sgr-crb)     .
           move      rf-zp3-mne-sgr       to   w-aux-mne-sgr-mbu
                                              (w-aux-mne-sgr-crb)     .
           move      rf-zp3-des-sgr       to   w-aux-mne-sgr-dbu
                                              (w-aux-mne-sgr-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-sgr-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-sgr-crb    =    1
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
           if        w-cod-mne-zp3-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zp3-dln    to   v-lin                  .
           move      w-cod-mne-zp3-dps    to   v-pos                  .
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
           move      w-aux-mne-sgr-cbu (1)
                                          to   w-cod-mne-zp3-num      .
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
           move      w-aux-mne-sgr-crb    to   w-aux-mne-sgr-cpb      .
           subtract  1                    from w-aux-mne-sgr-cpb      .
           divide    6                    into w-aux-mne-sgr-cpb      .
           add       1                    to   w-aux-mne-sgr-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-sgr-c01      .
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
           move      "        Selezionare il codice sottogruppo desidera
      -              "to        "         to   v-alf                  .
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
           move      w-aux-mne-sgr-c01    to   w-aux-mne-sgr-nli      .
       aco-355.
           if        w-aux-mne-sgr-nli    >    6
                     subtract  6          from w-aux-mne-sgr-nli
                     go to aco-355.
           add       09                   to   w-aux-mne-sgr-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           if        w-aux-mne-sgr-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-sgr-c01    <    w-aux-mne-sgr-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-sgr-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-sgr-cpa    <    w-aux-mne-sgr-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-sgr-nli    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-mne-sgr-fbu
                    (w-aux-mne-sgr-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        v-alf                =    w-aux-mne-sgr-fbu
                                              (w-aux-mne-sgr-c01)
                     go to aco-380.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-aux-mne-sgr-nli    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-mne-sgr-fbu
                    (w-aux-mne-sgr-c01)   to   v-alf                  .
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
           move      w-aux-mne-sgr-cbu
                    (w-aux-mne-sgr-c01)   to   w-cod-mne-zp3-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-sgr-c01      .
           if        w-aux-mne-sgr-nli    =    10
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-sgr-c01    <    w-aux-mne-sgr-crb
                     add   1              to   w-aux-mne-sgr-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-sgr-nli    =    15
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-sgr-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-sgr-cpa      .
       aco-396.
           move      w-aux-mne-sgr-cpa    to   w-aux-mne-sgr-c01      .
           multiply  6                    by   w-aux-mne-sgr-c01      .
           subtract  5                    from w-aux-mne-sgr-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-sgr-tpf    =    "D"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcp1213"           to   s-pro                  .
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
           move      w-cod-mne-zp3-alf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-cla"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      05                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zp3-cla    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-gru"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      05                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zp3-gru    to   s-num                  .
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
           move      spaces               to   w-aux-mne-sgr-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zp3-dln    to   v-lin                  .
           move      w-cod-mne-zp3-dps    to   v-pos                  .
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
           move      w-cod-mne-zp3-dln    to   v-lin                  .
           move      w-cod-mne-zp3-dps    to   v-pos                  .
           move      w-aux-mne-sgr-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-sgr-dup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zp3-dln    to   v-lin                  .
           move      w-cod-mne-zp3-dps    to   v-pos                  .
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
           if        w-aux-mne-sgr-dup    =    spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione massima        *
      *                      *-----------------------------------------*
           move      w-aux-mne-sgr-dup    to   w-aux-mne-sgr-rmx      .
           move      40                   to   w-aux-mne-sgr-c01      .
       aco-575.
           if        w-aux-mne-sgr-c01    >    zero
                     if    w-aux-mne-sgr-dch
                          (w-aux-mne-sgr-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-sgr-dch
                                              (w-aux-mne-sgr-c01)
                           subtract 1     from w-aux-mne-sgr-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con la    *
      *                      * stessa descrizione nel buffer           *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-sgr-tpf      .
           move      zero                 to   w-aux-mne-sgr-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-zp3-cla    to   rf-zp3-cod-cla         .
           move      w-cod-mne-zp3-gru    to   rf-zp3-cod-gru         .
           move      w-aux-mne-sgr-dup    to   rf-zp3-des-key         .
           move      zero                 to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
           if        rf-zp3-cod-cla       not  = w-cod-mne-zp3-cla
                     go to aco-250.
           if        rf-zp3-cod-gru       not  = w-cod-mne-zp3-gru
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zp3-des-key       >    w-aux-mne-sgr-rmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-sgr-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con la stessa descrizione               *
      *                      *-----------------------------------------*
           if        w-aux-mne-sgr-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zp3-cod-sgr       to   w-aux-mne-sgr-cbu
                                              (w-aux-mne-sgr-crb)     .
           move      rf-zp3-mne-sgr       to   w-aux-mne-sgr-mbu
                                              (w-aux-mne-sgr-crb)     .
           move      rf-zp3-des-sgr       to   w-aux-mne-sgr-dbu
                                              (w-aux-mne-sgr-crb)     .
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
           move      "pdcp1213"           to   s-pro                  .
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
           move      "des-gru"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-sgr-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-cla"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      05                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zp3-cla    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-gru"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      05                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zp3-gru    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-sgr-c01    to   w-aux-mne-sgr-c02      .
           add       5                    to   w-aux-mne-sgr-c02      .
           divide    6                    into w-aux-mne-sgr-c02      .
           move      w-aux-mne-sgr-c02    to   w-aux-mne-sgr-cpa      .
           subtract  1                    from w-aux-mne-sgr-c02      .
           multiply  6                    by   w-aux-mne-sgr-c02      .
           add       1                    to   w-aux-mne-sgr-c02      .
           add       5
                     w-aux-mne-sgr-c02  giving w-aux-mne-sgr-c03      .
           move      w-aux-mne-sgr-c03    to   w-aux-mne-sgr-c04      .
           if        w-aux-mne-sgr-c03    >    w-aux-mne-sgr-crb
                     move  w-aux-mne-sgr-crb
                                          to   w-aux-mne-sgr-c03      .
           move      10                   to   w-aux-mne-sgr-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-aux-mne-sgr-c05    to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-aux-mne-sgr-mbu
                    (w-aux-mne-sgr-c02)   to   v-alf                  .
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
           move      w-aux-mne-sgr-c05    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-aux-mne-sgr-cbu
                    (w-aux-mne-sgr-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-sgr-c05    to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-aux-mne-sgr-dbu
                    (w-aux-mne-sgr-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-sgr-c02      .
           add       1                    to   w-aux-mne-sgr-c05      .
           if        w-aux-mne-sgr-c02    not  > w-aux-mne-sgr-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-sgr-c02    >    w-aux-mne-sgr-c04
                     go to aco-955.
           if        w-aux-mne-sgr-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-sgr-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-sgr-c02      .
           add       1                    to   w-aux-mne-sgr-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-sgr-cpa    to   w-aux-mne-sgr-lt1      .
           move      w-aux-mne-sgr-cpb    to   w-aux-mne-sgr-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-sgr-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.
