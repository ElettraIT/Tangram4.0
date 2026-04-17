       Identification Division.
       Program-Id.                                 acmnage0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/06/91    *
      *                       Ultima revisione:    NdK del 14/06/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice agente           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-age-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-age-cod : codice agente        *
      *                                                                *
      *                       w-cod-mne-age-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-age-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-age-nln : linea ragione soc.   *
      *                                                                *
      *                       w-cod-mne-age-nps : posizione rag. soc.  *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-age-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-age-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-age-cod : codice agente        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-age-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-age-cod : codice agente        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-age-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-age-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-age-cod : codice agente        *
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
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

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
       01  w-aux-mne-age.
           05  w-aux-mne-age-c01          pic  9(02)                  .
           05  w-aux-mne-age-c02          pic  9(02)                  .
           05  w-aux-mne-age-c03          pic  9(02)                  .
           05  w-aux-mne-age-c04          pic  9(02)                  .
           05  w-aux-mne-age-c05          pic  9(02)                  .
           05  w-aux-mne-age-nli          pic  9(02)                  .
           05  w-aux-mne-age-m10          pic  x(10)                  .
           05  w-aux-mne-age-tpf          pic  x(01)                  .
           05  w-aux-mne-age-crb          pic  9(02)                  .
           05  w-aux-mne-age-cpb          pic  9(02)                  .
           05  w-aux-mne-age-cpa          pic  9(02)                  .
           05  w-aux-mne-age-buf
                               occurs 30.
               10  w-aux-mne-age-cbu      pic  9(07)                  .
               10  w-aux-mne-age-mbu      pic  x(10)                  .
               10  w-aux-mne-age-nbu.
                   20  w-aux-mne-age-fbu  pic  x(01)                  .
                   20  filler             pic  x(19)                  .
               10  w-aux-mne-age-rbu      pic  x(40)                  .
               10  w-aux-mne-age-vbu      pic  x(40)                  .
               10  w-aux-mne-age-lbu      pic  x(40)                  .
               10  w-aux-mne-age-pbu      pic  x(11)                  .
               10  w-aux-mne-age-sbu      pic  9(02)                  .
           05  w-aux-mne-age-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-age-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-age-lt2      pic  9(01)                  .
           05  w-aux-mne-age-nup          pic  x(20)                  .
           05  w-aux-mne-age-nmx.
               10  w-aux-mne-age-nch
                               occurs 20  pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-age
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
           if        w-cod-mne-age-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-age-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-age-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-age-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-age-ope    =    "A+" or
                     w-cod-mne-age-ope    =    "I+" or
                     w-cod-mne-age-ope    =    "F+"
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
      *                  * Open file [age]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
      *                  * Close file [age]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
                     move  spaces         to   w-cod-mne-age-ope      .
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
           move      "page2010"           to   s-pro                  .
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
           move      "page2000"           to   s-pro                  .
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
           move      w-cod-mne-age-cod    to   w-cod-mne-age-num      .
           move      v-edm                to   w-cod-mne-age-edm      .
           move      v-ufk                to   w-cod-mne-age-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-age-edm    to   v-edm                  .
           move      w-cod-mne-age-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-age-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-age-ufk    to   v-ufk                  .
           move      w-cod-mne-age-lin    to   v-lin                  .
           move      w-cod-mne-age-pos    to   v-pos                  .
           move      w-cod-mne-age-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-age-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-age-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-age-ope    =    "F+"
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
           move      w-cod-mne-age-ufk    to   v-ufk                  .
           move      w-cod-mne-age-lin    to   v-lin                  .
           move      w-cod-mne-age-pos    to   v-pos                  .
           move      w-cod-mne-age-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-age-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-age-alf      .
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
           move      w-cod-mne-age-lin    to   v-lin                  .
           move      w-cod-mne-age-pos    to   v-pos                  .
           move      w-cod-mne-age-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-age-num    to   w-cod-mne-age-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-age-nln      .
           move      zero                 to   w-cod-mne-age-nps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-age"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-age-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-age-edm    to   v-edm                  .
           move      w-cod-mne-age-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-age-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
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
           move      "F+"                 to   w-cod-mne-age-ope      .
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
           if        w-cod-mne-age-alf    =    spaces
                     move  zero           to   w-cod-mne-age-num
                     go to aco-075.
           if        w-cod-mne-age-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-age-c01      .
       aco-176.
           add       1                    to   w-aux-mne-age-c01      .
           if        w-aux-mne-age-c01    >    10
                     go to aco-180.
           if        w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-age-c01      .
           if        w-aux-mne-age-c01    >    10
                     go to aco-180.
           if        w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-age-num      .
           move      zero                 to   w-aux-mne-age-c01      .
       aco-181.
           add       1                    to   w-aux-mne-age-c01      .
           if        w-aux-mne-age-c01    >    10
                     go to aco-200.
           if        w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   <    "0" or
                     w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-mne-age-num      .
           move      w-cod-mne-age-cha
                    (w-aux-mne-age-c01)   to   w-cod-mne-age-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-mne-age-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-age-edm    to   v-edm                  .
           move      w-cod-mne-age-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-age-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-age-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '?' per Find generale        *
      *                  *---------------------------------------------*
           if        w-cod-mne-age-alf    =    "?"
                     move  spaces         to   w-aux-mne-age-nup
                     move  "?"            to   w-aux-mne-age-tpf
                     move  spaces         to   w-cod-mne-age-alf
                     go to aco-570.
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite ra-  *
      *                  * gione sociale                               *
      *                  *---------------------------------------------*
           if        w-cod-mne-age-alf    =    "-" and
                     w-cod-mne-age-nln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-age-tpf      .
           move      zero                 to   w-aux-mne-age-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-age-alf    to   rf-age-cod-mne         .
           move      zero                 to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 10 *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-aux-mne-age-m10      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-age-m10    not  = w-cod-mne-age-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-age-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-age-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-age-cod-age       to   w-aux-mne-age-cbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-cod-mne       to   w-aux-mne-age-mbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-rag-soc       to   w-aux-mne-age-rbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-nom-age       to   w-aux-mne-age-nbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-via-age       to   w-aux-mne-age-vbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-loc-age       to   w-aux-mne-age-lbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-prt-iva       to   w-aux-mne-age-pbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-sta-tus       to   w-aux-mne-age-sbu
                                              (w-aux-mne-age-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-age-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-age-crb    =    1
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
           if        w-cod-mne-age-nln    =    zero
                     go to aco-276.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cod-mne-age-nln    to   v-lin                  .
           move      w-cod-mne-age-nps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-276.
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
           move      w-aux-mne-age-cbu (1)
                                          to   w-cod-mne-age-num      .
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
           move      w-aux-mne-age-crb    to   w-aux-mne-age-cpb      .
           subtract  1                    from w-aux-mne-age-cpb      .
           divide    6                    into w-aux-mne-age-cpb      .
           add       1                    to   w-aux-mne-age-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-age-c01      .
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
           move      31                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      "Selezionare l'agente desiderato"
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
           move      "Codice agente    :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Ragione sociale  :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Indirizzo        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Localita'        :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Partita iva      :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Mnemonico :"        to   v-alf                  .
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
           move      w-aux-mne-age-c01    to   w-aux-mne-age-nli      .
       aco-355.
           if        w-aux-mne-age-nli    >    6
                     subtract  6          from w-aux-mne-age-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-age-nli      .
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-age-cbu
                    (w-aux-mne-age-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Status                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      48                   to   v-pos                  .
      *
           if        w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   not  numeric
                     move  01             to   w-aux-mne-age-sbu
                                              (w-aux-mne-age-c01)     .
      *
           if        w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    01
                     move  spaces         to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    11
                     move  "Esauriti rapp."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    21
                     move  "Sostituito    "
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    51
                     move  "Cessata attiv."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    52
                     move  "Cessata attiv."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    61
                     move  "In contenzioso"
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    62
                     move  "In contenzioso"
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    71
                     move  "Fallito       "
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c01)   =    72
                     move  "Fallito (sost)"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-age-rbu
                    (w-aux-mne-age-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-age-vbu
                    (w-aux-mne-age-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-age-lbu
                    (w-aux-mne-age-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-age-pbu
                    (w-aux-mne-age-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-aux-mne-age-mbu
                    (w-aux-mne-age-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-age-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-age-c01    <    w-aux-mne-age-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-age-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-age-cpa    <    w-aux-mne-age-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-age-nli    to   v-lin                  .
           move      36                   to   v-pos                  .
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
           move      w-aux-mne-age-cbu
                    (w-aux-mne-age-c01)   to   w-cod-mne-age-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-age-c01      .
           if        w-aux-mne-age-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-age-c01    <    w-aux-mne-age-crb
                     add   1              to   w-aux-mne-age-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-age-nli    =    12
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-age-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-age-cpa      .
       aco-396.
           move      w-aux-mne-age-cpa    to   w-aux-mne-age-c01      .
           multiply  6                    by   w-aux-mne-age-c01      .
           subtract  5                    from w-aux-mne-age-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-age-tpf    =    "N"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "page2010"           to   s-pro                  .
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
           move      w-cod-mne-age-alf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-500.
      *              *-------------------------------------------------*
      *              * Se ricerca per ragione sociale                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spaces in nome agente di comodo             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-age-nup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cod-mne-age-nln    to   v-lin                  .
           move      w-cod-mne-age-nps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-525.
      *                  *---------------------------------------------*
      *                  * Accettazione nome agente in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-age-nln    to   v-lin                  .
           move      w-cod-mne-age-nps    to   v-pos                  .
           move      w-aux-mne-age-nup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-age-nup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-cod-mne-age-nln    to   v-lin                  .
           move      w-cod-mne-age-nps    to   v-pos                  .
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
           if        w-aux-mne-age-nup    =    spaces
                     go to aco-025.
       aco-570.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per ragione sociale      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione nome agente massimo        *
      *                      *-----------------------------------------*
           move      w-aux-mne-age-nup    to   w-aux-mne-age-nmx      .
           move      20                   to   w-aux-mne-age-c01      .
       aco-575.
           if        w-aux-mne-age-c01    >    zero
                     if    w-aux-mne-age-nch
                          (w-aux-mne-age-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-age-nch
                                              (w-aux-mne-age-c01)
                           subtract 1     from w-aux-mne-age-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           if        w-aux-mne-age-tpf    =    "?"
                     go to aco-580.
           move      "N"                  to   w-aux-mne-age-tpf      .
       aco-580.
           move      zero                 to   w-aux-mne-age-crb      .
      *                      *-----------------------------------------*
      *                      * Start per nome agente                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NOMKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-mne-age-nup    to   rf-age-nom-key         .
           move      zero                 to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-600.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per rag. soc.       *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-age-nom-key       >    w-aux-mne-age-nmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-age-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-age-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-age-cod-age       to   w-aux-mne-age-cbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-cod-mne       to   w-aux-mne-age-mbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-rag-soc       to   w-aux-mne-age-rbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-nom-age       to   w-aux-mne-age-nbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-via-age       to   w-aux-mne-age-vbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-loc-age       to   w-aux-mne-age-lbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-prt-iva       to   w-aux-mne-age-pbu
                                              (w-aux-mne-age-crb)     .
           move      rf-age-sta-tus       to   w-aux-mne-age-sbu
                                              (w-aux-mne-age-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-600.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Se piu' di trenta record con lo stesso  *
      *                      * nome agente impostato                   *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "page2010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-525.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "N"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "nom-age"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-age-nup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-age-c01    to   w-aux-mne-age-c02      .
           add       5                    to   w-aux-mne-age-c02      .
           divide    6                    into w-aux-mne-age-c02      .
           move      w-aux-mne-age-c02    to   w-aux-mne-age-cpa      .
           subtract  1                    from w-aux-mne-age-c02      .
           multiply  6                    by   w-aux-mne-age-c02      .
           add       1                    to   w-aux-mne-age-c02      .
           add       5
                     w-aux-mne-age-c02  giving w-aux-mne-age-c03      .
           move      w-aux-mne-age-c03    to   w-aux-mne-age-c04      .
           if        w-aux-mne-age-c03    >    w-aux-mne-age-crb
                     move  w-aux-mne-age-crb
                                          to   w-aux-mne-age-c03      .
           move      07                   to   w-aux-mne-age-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-age-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-mne-age-cbu
                    (w-aux-mne-age-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Nominativo                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-aux-mne-age-c05    to   v-lin                  .
           move      36                   to   v-pos                  .
           move      w-aux-mne-age-nbu
                    (w-aux-mne-age-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Status                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      w-aux-mne-age-c05    to   v-lin                  .
           move      57                   to   v-pos                  .
      *
           if        w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   not  numeric
                     move  01             to   w-aux-mne-age-sbu
                                              (w-aux-mne-age-c02)     .
      *
           if        w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    01
                     move  spaces         to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    11
                     move  "Esauriti rapp."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    21
                     move  "Sostituito    "
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    51
                     move  "Cessata attiv."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    52
                     move  "Cessata attiv."
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    61
                     move  "In contenzioso"
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    62
                     move  "In contenzioso"
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    71
                     move  "Fallito       "
                                          to   v-alf
           else if   w-aux-mne-age-sbu
                    (w-aux-mne-age-c02)   =    72
                     move  "Fallito (sost)"
                                          to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Contatori                                   *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-mne-age-c02      .
           add       1                    to   w-aux-mne-age-c05      .
           if        w-aux-mne-age-c02    not  > w-aux-mne-age-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-age-c02    >    w-aux-mne-age-c04
                     go to aco-955.
           if        w-aux-mne-age-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-age-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-age-c02      .
           add       1                    to   w-aux-mne-age-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-age-cpa    to   w-aux-mne-age-lt1      .
           move      w-aux-mne-age-cpb    to   w-aux-mne-age-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-age-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.
