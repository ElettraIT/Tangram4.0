       Identification Division.
       Program-Id.                                 acmnzac0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/01/92    *
      *                       Ultima revisione:    NdK del 13/12/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione tipo di addebito o com- *
      *                    mento per la fatturazione                   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zac-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-zac-tip : tipo record          *
      *                                           01 : Addebito        *
      *                                           02 : Commento        *
      *                                                                *
      *                       w-cod-mne-zac-cod : codice addebito o    *
      *                                           commento             *
      *                                                                *
      *                       w-cod-mne-zac-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-zac-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-zac-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-zac-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zac-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zac-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zac-cod : codice addebito o    *
      *                                           commento             *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zac-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zac-cod : codice addebito o    *
      *                                           commento             *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-zac-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zac-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zac-cod : codice addebito o    *
      *                                           commento             *
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
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

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
       01  w-aux-mne-zac.
           05  w-aux-mne-zac-c01          pic  9(02)                  .
           05  w-aux-mne-zac-c02          pic  9(02)                  .
           05  w-aux-mne-zac-c03          pic  9(02)                  .
           05  w-aux-mne-zac-c04          pic  9(02)                  .
           05  w-aux-mne-zac-c05          pic  9(02)                  .
           05  w-aux-mne-zac-nli          pic  9(02)                  .
           05  w-aux-mne-zac-m03          pic  x(03)                  .
           05  w-aux-mne-zac-tpf          pic  x(01)                  .
           05  w-aux-mne-zac-crb          pic  9(02)                  .
           05  w-aux-mne-zac-cpb          pic  9(02)                  .
           05  w-aux-mne-zac-cpa          pic  9(02)                  .
           05  w-aux-mne-zac-buf
                               occurs 30.
               10  w-aux-mne-zac-cbu      pic  9(03)                  .
               10  w-aux-mne-zac-mbu      pic  x(03)                  .
               10  w-aux-mne-zac-rbu.
                   15  w-aux-mne-zac-fbu  occurs 10
                                          pic  x(40)                  .
               10  w-aux-mne-zac-ibu      pic  9(05)                  .
               10  w-aux-mne-zac-sbu      pic  9(07)                  .
           05  w-aux-mne-zac-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-zac-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-zac-lt2      pic  9(01)                  .
           05  w-aux-mne-zac-rup          pic  x(40)                  .
           05  w-aux-mne-zac-rmx.
               10  w-aux-mne-zac-rch
                               occurs 40  pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Numero livelli del piano dei conti                    *
      *        *-------------------------------------------------------*
           05  w-prs-liv-pdc              pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice sottoconto         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wkl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice addebito o commento     *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acmnzac0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-zac
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
           if        w-cod-mne-zac-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-zac-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-zac-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-zac-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-zac-ope    =    "A+" or
                     w-cod-mne-zac-ope    =    "I+" or
                     w-cod-mne-zac-ope    =    "F+"
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
      *                  * [zac]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *                  *---------------------------------------------*
      *                  * [pdc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * [zci]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
      *                  *---------------------------------------------*
      *                  * Lettura personalizzazione numero livelli    *
      *                  * del piano dei conti                         *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[liv-pdc]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-liv-pdc
           else      move  3              to   w-prs-liv-pdc          .
           if        w-prs-liv-pdc        not  = 2
                     move  3              to   w-prs-liv-pdc          .
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
      *                  * [zac]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *                  *---------------------------------------------*
      *                  * [pdc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * [zci]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
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
                     move  spaces         to   w-cod-mne-zac-ope      .
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
           move      "pfat1110"           to   s-pro                  .
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
           move      "pfat1100"           to   s-pro                  .
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
           move      w-cod-mne-zac-cod    to   w-cod-mne-zac-num      .
           move      v-edm                to   w-cod-mne-zac-edm      .
           move      v-ufk                to   w-cod-mne-zac-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zac-edm    to   v-edm                  .
           move      w-cod-mne-zac-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zac-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-zac-ufk    to   v-ufk                  .
           move      w-cod-mne-zac-lin    to   v-lin                  .
           move      w-cod-mne-zac-pos    to   v-pos                  .
           move      w-cod-mne-zac-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zac-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-zac-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-zac-ope    =    "F+"
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
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-zac-ufk    to   v-ufk                  .
           move      w-cod-mne-zac-lin    to   v-lin                  .
           move      w-cod-mne-zac-pos    to   v-pos                  .
           move      w-cod-mne-zac-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zac-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zac-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-zac-alf      .
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
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-zac-lin    to   v-lin                  .
           move      w-cod-mne-zac-pos    to   v-pos                  .
           move      w-cod-mne-zac-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-zac-num    to   w-cod-mne-zac-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zac-dln      .
           move      zero                 to   w-cod-mne-zac-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-aoc"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-zac-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zac-edm    to   v-edm                  .
           move      w-cod-mne-zac-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zac-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-zac-ope      .
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
           move      "F+"                 to   w-cod-mne-zac-ope      .
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
           if        w-cod-mne-zac-alf    =    spaces
                     move  zero           to   w-cod-mne-zac-num
                     go to aco-075.
           if        w-cod-mne-zac-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-zac-c01      .
       aco-176.
           add       1                    to   w-aux-mne-zac-c01      .
           if        w-aux-mne-zac-c01    >    03
                     go to aco-180.
           if        w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-zac-c01      .
           if        w-aux-mne-zac-c01    >    03
                     go to aco-180.
           if        w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zac-num      .
           move      zero                 to   w-aux-mne-zac-c01      .
       aco-181.
           add       1                    to   w-aux-mne-zac-c01      .
           if        w-aux-mne-zac-c01    >    03
                     go to aco-200.
           if        w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   <    "0" or
                     w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-mne-zac-num      .
           move      w-cod-mne-zac-cha
                    (w-aux-mne-zac-c01)   to   w-cod-mne-zac-chn (03) .
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
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zac-edm    to   v-edm                  .
           move      w-cod-mne-zac-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-zac-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-zac-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-mne-zac-alf    =    "-" and
                     w-cod-mne-zac-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-zac-tpf      .
           move      zero                 to   w-aux-mne-zac-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-zac-tip    to   rf-zac-tip-rec         .
           move      w-cod-mne-zac-alf    to   rf-zac-mne-aoc         .
           move      zero                 to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
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
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 3  *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-zac-mne-aoc       to   w-aux-mne-zac-m03      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zac-tip-rec       not  = w-cod-mne-zac-tip or
                     w-aux-mne-zac-m03    not  = w-cod-mne-zac-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zac-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zac-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zac-cod-aoc       to   w-aux-mne-zac-cbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-mne-aoc       to   w-aux-mne-zac-mbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-des-aoc       to   w-aux-mne-zac-rbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-cod-iva       to   w-aux-mne-zac-ibu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-cod-ctp       to   w-aux-mne-zac-sbu
                                              (w-aux-mne-zac-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-zac-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-zac-crb    =    1
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
           if        w-cod-mne-zac-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zac-dln    to   v-lin                  .
           move      w-cod-mne-zac-dps    to   v-pos                  .
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
           move      w-aux-mne-zac-cbu (1)
                                          to   w-cod-mne-zac-num      .
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
           move      w-aux-mne-zac-crb    to   w-aux-mne-zac-cpb      .
           subtract  1                    from w-aux-mne-zac-cpb      .
           divide    12                   into w-aux-mne-zac-cpb      .
           add       1                    to   w-aux-mne-zac-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-zac-c01      .
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
           move      06                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      08                   to   v-pos                  .
           if        w-cod-mne-zac-tip    =    01
                     move  "            Selezionare il tipo di addebito 
      -              "desiderato            "
                                          to   v-alf
           else      move  "            Selezionare il tipo di commento 
      -              "desiderato            "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      66                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      08                   to   v-pos                  .
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
           move      w-aux-mne-zac-c01    to   w-aux-mne-zac-nli      .
       aco-355.
           if        w-aux-mne-zac-nli    >    12
                     subtract  12         from w-aux-mne-zac-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-zac-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-zac-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-zac-c01    <    w-aux-mne-zac-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-zac-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-zac-cpa    <    w-aux-mne-zac-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXPD"               to   v-pfk (15)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-zac-nli    to   v-lin                  .
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
           else if   v-key                =    "EXPD"
                     go to aco-381
           else      go to aco-375.
       aco-381.
           perform   aco-960              thru aco-969                .
           go to     aco-375.
       aco-382.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      w-aux-mne-zac-cbu
                    (w-aux-mne-zac-c01)   to   w-cod-mne-zac-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-zac-c01      .
           if        w-aux-mne-zac-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-zac-c01    <    w-aux-mne-zac-crb
                     add   1              to   w-aux-mne-zac-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-zac-nli    =    18
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-zac-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-zac-cpa      .
       aco-396.
           move      w-aux-mne-zac-cpa    to   w-aux-mne-zac-c01      .
           multiply  12                   by   w-aux-mne-zac-c01      .
           subtract  11                   from w-aux-mne-zac-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-zac-tpf    =    "D"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pfat1110"           to   s-pro                  .
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
           move      "tip-rec"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zac-tip    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-mne"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      03                   to   s-car                  .
           move      w-cod-mne-zac-alf    to   s-alf                  .
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
           move      spaces               to   w-aux-mne-zac-rup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zac-dln    to   v-lin                  .
           move      w-cod-mne-zac-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-525.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-zac-dln    to   v-lin                  .
           move      w-cod-mne-zac-dps    to   v-pos                  .
           move      w-aux-mne-zac-rup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-zac-rup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-zac-dln    to   v-lin                  .
           move      w-cod-mne-zac-dps    to   v-pos                  .
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
           if        w-aux-mne-zac-rup    =    spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione massima        *
      *                      *-----------------------------------------*
           move      w-aux-mne-zac-rup    to   w-aux-mne-zac-rmx      .
           move      40                   to   w-aux-mne-zac-c01      .
       aco-575.
           if        w-aux-mne-zac-c01    >    zero
                     if    w-aux-mne-zac-rch
                          (w-aux-mne-zac-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-zac-rch
                                              (w-aux-mne-zac-c01)
                           subtract 1     from w-aux-mne-zac-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-zac-tpf      .
           move      zero                 to   w-aux-mne-zac-crb      .
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-zac-tip    to   rf-zac-tip-rec         .
           move      w-aux-mne-zac-rup    to   rf-zac-des-key         .
           move      zero                 to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
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
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zac-tip-rec       not  = w-cod-mne-zac-tip or
                     rf-zac-des-key       >    w-aux-mne-zac-rmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zac-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zac-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zac-cod-aoc       to   w-aux-mne-zac-cbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-mne-aoc       to   w-aux-mne-zac-mbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-des-aoc       to   w-aux-mne-zac-rbu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-cod-iva       to   w-aux-mne-zac-ibu
                                              (w-aux-mne-zac-crb)     .
           move      rf-zac-cod-ctp       to   w-aux-mne-zac-sbu
                                              (w-aux-mne-zac-crb)     .
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
           move      "pfat1110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-525.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "R"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "tip-rec"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-mne-zac-tip    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "des-aoc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      40                   to   s-car                  .
           move      w-aux-mne-zac-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-zac-c01    to   w-aux-mne-zac-c02      .
           add       11                   to   w-aux-mne-zac-c02      .
           divide    12                   into w-aux-mne-zac-c02      .
           move      w-aux-mne-zac-c02    to   w-aux-mne-zac-cpa      .
           subtract  1                    from w-aux-mne-zac-c02      .
           multiply  12                   by   w-aux-mne-zac-c02      .
           add       1                    to   w-aux-mne-zac-c02      .
           add       11
                     w-aux-mne-zac-c02  giving w-aux-mne-zac-c03      .
           move      w-aux-mne-zac-c03    to   w-aux-mne-zac-c04      .
           if        w-aux-mne-zac-c03    >    w-aux-mne-zac-crb
                     move  w-aux-mne-zac-crb
                                          to   w-aux-mne-zac-c03      .
           move      07                   to   w-aux-mne-zac-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      w-aux-mne-zac-c05    to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-aux-mne-zac-mbu
                    (w-aux-mne-zac-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice addebito o commento                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-zac-c05    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-aux-mne-zac-cbu
                    (w-aux-mne-zac-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-zac-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c02, 1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zac-c02      .
           add       1                    to   w-aux-mne-zac-c05      .
           if        w-aux-mne-zac-c02    not  > w-aux-mne-zac-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-zac-c02    >    w-aux-mne-zac-c04
                     go to aco-955.
           if        w-aux-mne-zac-crb    not  > 12
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      64                   to   v-car                  .
           move      w-aux-mne-zac-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zac-c02      .
           add       1                    to   w-aux-mne-zac-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-zac-cpa    to   w-aux-mne-zac-lt1      .
           move      w-aux-mne-zac-cpb    to   w-aux-mne-zac-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-zac-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-960.
      *              *-------------------------------------------------*
      *              * Trattamento tasto funzione Expand               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione box vuoto interno a quello  *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      74                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice addebito o commento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Codice       :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      06                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-cbu
                    (w-aux-mne-zac-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Mnemonico addebito o commento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      46                   to   v-pos                  .
           move      "Mnemonico    :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      w-aux-mne-zac-mbu
                    (w-aux-mne-zac-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione addebito o commento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Descrizione  :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 1)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 2)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 3)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 4)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 5)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 6)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 7)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 8)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 9)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-fbu
                    (w-aux-mne-zac-c01, 10)
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se tipo record commento : oltre             *
      *                  *---------------------------------------------*
           if        w-cod-mne-zac-tip    =    02
                     go to aco-967.
      *                  *---------------------------------------------*
      *                  * Codice iva                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Codice iva   :"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      17                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zac-ibu
                    (w-aux-mne-zac-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [zci]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODIVA    "         to   f-key                  .
           move      w-aux-mne-zac-ibu
                    (w-aux-mne-zac-c01)   to   rf-zci-cod-iva         .
           move      "pgm/cge/fls/ioc/obj/iofzci"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zci                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-zci-des-doc         .
           if        w-aux-mne-zac-ibu
                    (w-aux-mne-zac-c01)   <    100
                     move  spaces         to   rf-zci-des-doc         .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      rf-zci-des-doc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice contropartita                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      14                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      "Contropartita:"     to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing con appoggio a sinistra     *
      *                          *-------------------------------------*
           move      w-prs-liv-pdc        to   w-edt-cod-pdc-liv      .
           move      w-aux-mne-zac-sbu
                    (w-aux-mne-zac-c01)   to   w-edt-cod-pdc-cod      .
           move      "B"                  to   w-edt-cod-pdc-edm      .
           perform   edt-pdc-asx-000      thru edt-pdc-asx-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-edt-cod-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdc]              *
      *                          *-------------------------------------*
           if        w-aux-mne-zac-sbu
                    (w-aux-mne-zac-c01)   =    zero
                     move  spaces         to   rf-pdc-des-pdc
                     go to aco-965.
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-aux-mne-zac-sbu
                    (w-aux-mne-zac-c01)   to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-pdc-des-pdc         .
       aco-965.
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-pdc-des-pdc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-967.
      *                      *-----------------------------------------*
      *                      * Parentesi quadre di delimitazione       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Accettazione carattere di presa visione *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      72                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ripristino immagine video               *
      *                      *-----------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       aco-969.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Editing del codice sottoconto con appoggio a sx o dx      *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtpdc0.wks"                   .

