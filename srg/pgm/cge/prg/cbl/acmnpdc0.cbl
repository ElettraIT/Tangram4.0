       Identification Division.
       Program-Id.                                 acmnpdc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/05/91    *
      *                       Ultima revisione:    NdK del 26/06/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice sottoconto       *
      *                                                                *
      *                    -------------------------------------       *
      *                                                                *
      *                    Metodi di ricerca :                         *
      *                                                                *
      *                    '-'  seguito da return = Ricerca per        *
      *                                             descrizione        *
      *                                                                *
      *                    '-'  seguito da return = Ricerca dicotomica *
      *                               e da 'find'   per descrizione    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-pdc-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-pdc-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-pdc-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdc-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-pdc-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-pdc-cod : codice sottoconto    *
      *                                                                *
      *                       w-cod-mne-pdc-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-pdc-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-pdc-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-pdc-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdc-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-fnt-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-fnt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-fnt-cod : codice sottoconto    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-fnt-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-fnt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-fnt-cod : codice sottoconto    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-fnt-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-fnt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-fnt-cod : codice sottoconto    *
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
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .

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
       01  w-aux-mne-pdc.
           05  w-aux-mne-pdc-c01          pic  9(02)                  .
           05  w-aux-mne-pdc-c02          pic  9(02)                  .
           05  w-aux-mne-pdc-c03          pic  9(02)                  .
           05  w-aux-mne-pdc-c04          pic  9(02)                  .
           05  w-aux-mne-pdc-c05          pic  9(02)                  .
           05  w-aux-mne-pdc-nli          pic  9(02)                  .
           05  w-aux-mne-pdc-m10          pic  x(10)                  .
           05  w-aux-mne-pdc-tpf          pic  x(01)                  .
           05  w-aux-mne-pdc-crb          pic  9(02)                  .
           05  w-aux-mne-pdc-cpb          pic  9(02)                  .
           05  w-aux-mne-pdc-cpa          pic  9(02)                  .
           05  w-aux-mne-pdc-max          pic  9(02) value 54         .
           05  w-aux-mne-pdc-buf occurs 54.
               10  w-aux-mne-pdc-cbu      pic  9(07)                  .
               10  w-aux-mne-pdc-mbu      pic  x(10)                  .
               10  w-aux-mne-pdc-dbu      pic  x(40)                  .
           05  w-aux-mne-pdc-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-pdc-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-pdc-lt2      pic  9(01)                  .
           05  w-aux-mne-pdc-dup          pic  x(40)                  .
           05  w-aux-mne-pdc-vnr          pic  x(40)                  .
           05  w-aux-mne-pdc-dmx.
               10  w-aux-mne-pdc-dch
                               occurs 40  pic  x(01)                  .

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
      *    * Link-area per accettazione codice sottoconto              *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnpdc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-pdc
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
           if        w-cod-mne-pdc-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-pdc-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-pdc-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-pdc-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-pdc-ope    =    "A+" or
                     w-cod-mne-pdc-ope    =    "I+" or
                     w-cod-mne-pdc-ope    =    "F+"
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
      *                  * Open file [pdc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
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
      *                  * Close file [pdc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
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
                     move  spaces         to   w-cod-mne-pdc-ope      .
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
           move      "pcge2010"           to   s-pro                  .
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
           move      "pcge2000"           to   s-pro                  .
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
           move      w-cod-mne-pdc-cod    to   w-cod-mne-pdc-num      .
           move      v-edm                to   w-cod-mne-pdc-edm      .
           move      v-ufk                to   w-cod-mne-pdc-ufk      .
      *              *-------------------------------------------------*
      *              * Determinazione se clausola blank when zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-mne-pdc-c01      .
           inspect   w-cod-mne-pdc-edm
                                      tallying w-aux-mne-pdc-c01
                     for   all "B"                                    .
           if        w-aux-mne-pdc-c01    >    zero
                     move  "B"            to   w-cod-mne-pdc-bwz
           else      move  spaces         to   w-cod-mne-pdc-bwz      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           if        w-cod-mne-pdc-num    =    zero  and
                     w-cod-mne-pdc-bwz    =    "B"
                     move  spaces         to   w-cod-mne-pdc-edt
           else      move  spaces         to   w-cod-mne-pdc-edt
                     if    w-cod-mne-pdc-liv
                                          =    2
                           move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e2l
                     else  move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e3l      .
           move      w-cod-mne-pdc-edt    to   w-cod-mne-pdc-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-pdc-ufk    to   v-ufk                  .
           move      w-cod-mne-pdc-lin    to   v-lin                  .
           move      w-cod-mne-pdc-pos    to   v-pos                  .
           move      w-cod-mne-pdc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-pdc-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-pdc-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-pdc-ope    =    "F+"
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
           move      w-cod-mne-pdc-ufk    to   v-ufk                  .
           move      w-cod-mne-pdc-lin    to   v-lin                  .
           move      w-cod-mne-pdc-pos    to   v-pos                  .
           move      w-cod-mne-pdc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-pdc-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-pdc-alf      .
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
           move      w-cod-mne-pdc-lin    to   v-lin                  .
           move      w-cod-mne-pdc-pos    to   v-pos                  .
           move      w-cod-mne-pdc-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-pdc-num    to   w-cod-mne-pdc-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-pdc-dln      .
           move      zero                 to   w-cod-mne-pdc-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "select pdc"         to   s-var                  .
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
           move      s-num                to   w-cod-mne-pdc-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           if        w-cod-mne-pdc-num    =    zero  and
                     w-cod-mne-pdc-bwz    =    "B"
                     move  spaces         to   w-cod-mne-pdc-edt
           else      move  spaces         to   w-cod-mne-pdc-edt
                     if    w-cod-mne-pdc-liv
                                          =    2
                           move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e2l
                     else  move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e3l      .
           move      w-cod-mne-pdc-edt    to   w-cod-mne-pdc-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-pdc-ope      .
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
           move      "F+"                 to   w-cod-mne-pdc-ope      .
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
           if        w-cod-mne-pdc-alf    =    spaces
                     move  zero           to   w-cod-mne-pdc-num
                     go to aco-075.
           if        w-cod-mne-pdc-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-pdc-c01      .
       aco-176.
           add       1                    to   w-aux-mne-pdc-c01      .
           if        w-aux-mne-pdc-c01    >    10
                     go to aco-180.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-pdc-c01      .
           if        w-aux-mne-pdc-c01    >    10
                     go to aco-180.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-pdc-num      .
           move      zero                 to   w-aux-mne-pdc-c01      .
       aco-181.
           add       1                    to   w-aux-mne-pdc-c01      .
           if        w-aux-mne-pdc-c01    >    10
                     go to aco-200.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   <    "0" or
                     w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   >    "9"
                     go to aco-210.
           multiply  10                   by   w-cod-mne-pdc-num      .
           move      w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   to   w-cod-mne-pdc-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul valore massimo                     *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdc-liv    =    2
                     if    w-cod-mne-pdc-num
                                          <    100000
                           go to aco-205
                     else  go to aco-025
           else      if    w-cod-mne-pdc-num
                                          <    10000000
                           go to aco-205
                     else  go to aco-025.
       aco-205.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdc-num    =    zero  and
                     w-cod-mne-pdc-bwz    =    "B"
                     move  spaces         to   w-cod-mne-pdc-edt
           else      move  spaces         to   w-cod-mne-pdc-edt
                     if    w-cod-mne-pdc-liv
                                          =    2
                           move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e2l
                     else  move  w-cod-mne-pdc-num
                                          to   w-cod-mne-pdc-e3l      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdc-edt    =    w-cod-mne-pdc-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  w-cod-mne-pdc-edt
                                          to   w-cod-mne-pdc-alf
                     go to aco-050.
       aco-210.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se il valore corrisponde effettivamen- *
      *                  * te ad un codice sottoconto editato          *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdc-liv    =    2
                     go to aco-215
           else      go to aco-220.
       aco-215.
           move      w-cod-mne-pdc-alf    to   w-cod-mne-pdc-2e0      .
           if        w-cod-mne-pdc-2e1    not  numeric   or
                     w-cod-mne-pdc-2e2    not  = "."     or
                     w-cod-mne-pdc-2e3    not  numeric   or
                     w-cod-mne-pdc-2e4    not  = spaces
                     go to aco-240
           else      go to aco-225.
       aco-220.
           move      w-cod-mne-pdc-alf    to   w-cod-mne-pdc-3e0      .
           if        w-cod-mne-pdc-3e1    not  numeric   or
                     w-cod-mne-pdc-3e2    not  = "."     or
                     w-cod-mne-pdc-3e3    not  numeric   or
                     w-cod-mne-pdc-3e4    not  = "."     or
                     w-cod-mne-pdc-3e5    not  numeric   or
                     w-cod-mne-pdc-3e6    not  = spaces
                     go to aco-240.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Se il valore corrisponde effettivamente ad  *
      *                  * un codice sottoconto editato lo si converte *
      *                  * in numerico e ci si riaggancia al tratta-   *
      *                  * mento per il formato numerico               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cod-mne-pdc-num      .
           move      zero                 to   w-aux-mne-pdc-c01      .
       aco-230.
           add       1                    to   w-aux-mne-pdc-c01      .
           if        w-aux-mne-pdc-c01    >    09
                     go to aco-200.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   =    spaces
                     go to aco-230.
           if        w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   <    "0" or
                     w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   >    "9"
                     go to aco-230.
           multiply  10                   by   w-cod-mne-pdc-num      .
           move      w-cod-mne-pdc-cha
                    (w-aux-mne-pdc-c01)   to   w-cod-mne-pdc-chn (10) .
           go to     aco-230.
       aco-240.
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite de-  *
      *                  * scrizione                                   *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdc-alf    =    "-" and
                     w-cod-mne-pdc-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-pdc-tpf      .
           move      zero                 to   w-aux-mne-pdc-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-pdc-alf    to   rf-pdc-cod-mne         .
           move      zero                 to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-245.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per mnemonico       *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 9  *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-pdc-cod-mne       to   w-aux-mne-pdc-m10      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-m10    not  = w-cod-mne-pdc-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-pdc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-crb    >    w-aux-mne-pdc-max
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-pdc-cod-pdc       to   w-aux-mne-pdc-cbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-cod-mne       to   w-aux-mne-pdc-mbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-des-pdc       to   w-aux-mne-pdc-dbu
                                              (w-aux-mne-pdc-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-245.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-pdc-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-pdc-crb    =    1
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
           if        w-cod-mne-pdc-dln    =    zero
                     go to aco-276.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdc-dln    to   v-lin                  .
           move      w-cod-mne-pdc-dps    to   v-pos                  .
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
           move      w-aux-mne-pdc-cbu (1)
                                          to   w-cod-mne-pdc-num      .
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
           move      w-aux-mne-pdc-crb    to   w-aux-mne-pdc-cpb      .
           subtract  1                    from w-aux-mne-pdc-cpb      .
           divide    6                    into w-aux-mne-pdc-cpb      .
           add       1                    to   w-aux-mne-pdc-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-pdc-c01      .
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
           move      11                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      71                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "Selezionare il sottoconto desiderato"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      14                   to   v-pos                  .
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
           move      w-aux-mne-pdc-c01    to   w-aux-mne-pdc-nli      .
       aco-355.
           if        w-aux-mne-pdc-nli    >    6
                     subtract  6          from w-aux-mne-pdc-nli
                     go to aco-355.
           add       09                   to   w-aux-mne-pdc-nli      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-pdc-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-pdc-c01    <    w-aux-mne-pdc-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-pdc-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-pdc-cpa    <    w-aux-mne-pdc-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-pdc-nli    to   v-lin                  .
           move      27                   to   v-pos                  .
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
           move      w-aux-mne-pdc-cbu
                    (w-aux-mne-pdc-c01)   to   w-cod-mne-pdc-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-pdc-c01      .
           if        w-aux-mne-pdc-nli    =    10
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-pdc-c01    <    w-aux-mne-pdc-crb
                     add   1              to   w-aux-mne-pdc-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-pdc-nli    =    15
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-pdc-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-pdc-cpa      .
       aco-396.
           move      w-aux-mne-pdc-cpa    to   w-aux-mne-pdc-c01      .
           multiply  6                    by   w-aux-mne-pdc-c01      .
           subtract  5                    from w-aux-mne-pdc-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-pdc-tpf    =    "D"
                     go to aco-510
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-025.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-aux-mne-pdc-tpf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-mne"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-cod-mne-pdc-alf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * A preparazione uscita                       *
      *                  *---------------------------------------------*
           go to     aco-125.
       aco-500.
      *              *=================================================*
      *              * Se ricerca per descrizione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spaces in descrizione di comodo             *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-pdc-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per descrizione              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdc-dln    to   v-lin                  .
           move      w-cod-mne-pdc-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-510.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-pdc-dln    to   v-lin                  .
           move      w-cod-mne-pdc-dps    to   v-pos                  .
           move      w-aux-mne-pdc-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-pdc-dup      .
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
      *                      * Visualizzazione descrizione a spazi     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdc-dln    to   v-lin                  .
           move      w-cod-mne-pdc-dps    to   v-pos                  .
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
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     aco-075.
       aco-516.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to aco-700.
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : rientro ad   *
      *                      * accettazione descrizione in uppercase   *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-dup    =    spaces
                     go to aco-510.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-mne-pdc-dup    to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-aux-mne-pdc-vnr      .
      *                      *-----------------------------------------*
      *                      * Se il valore normalizzato e' a spaces : *
      *                      * rientro ad accettazione descrizione     *
      *                      * in uppercase                            *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-vnr    =    spaces
                     go to aco-510.
       aco-530.
      *                      *-----------------------------------------*
      *                      * Esecuzione ricerca totale               *
      *                      *-----------------------------------------*
           perform   aco-fnd-000          thru aco-fnd-999            .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con la stessa descrizione               *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-crb    >    w-aux-mne-pdc-max
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * A visualizzazione                       *
      *                      *-----------------------------------------*
           go to     aco-250.
       aco-700.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a Spaces : come Up      *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-dup    =    spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione massima        *
      *                      *-----------------------------------------*
           move      w-aux-mne-pdc-dup    to   w-aux-mne-pdc-dmx      .
           move      40                   to   w-aux-mne-pdc-c01      .
       aco-720.
           if        w-aux-mne-pdc-c01    >    zero
                     if    w-aux-mne-pdc-dch
                          (w-aux-mne-pdc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-pdc-dch
                                              (w-aux-mne-pdc-c01)
                           subtract 1     from w-aux-mne-pdc-c01
                           go to    aco-720.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-pdc-tpf      .
           move      zero                 to   w-aux-mne-pdc-crb      .
       aco-740.
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-mne-pdc-dup    to   rf-pdc-des-key         .
           move      zero                 to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-750.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale per descrizione     *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-pdc-des-key       >    w-aux-mne-pdc-dmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-pdc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con la stessa descrizione               *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdc-crb    >    w-aux-mne-pdc-max
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-pdc-cod-pdc       to   w-aux-mne-pdc-cbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-cod-mne       to   w-aux-mne-pdc-mbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-des-pdc       to   w-aux-mne-pdc-dbu
                                              (w-aux-mne-pdc-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-750.
       aco-800.
      *                      *-----------------------------------------*
      *                      * Se piu' di trenta record con la stessa  *
      *                      * descrizione impostata                   *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge2010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-510.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-aux-mne-pdc-tpf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "des-pdc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-pdc-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * A preparazione uscita                       *
      *                  *---------------------------------------------*
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-pdc-c01    to   w-aux-mne-pdc-c02      .
           add       5                    to   w-aux-mne-pdc-c02      .
           divide    6                    into w-aux-mne-pdc-c02      .
           move      w-aux-mne-pdc-c02    to   w-aux-mne-pdc-cpa      .
           subtract  1                    from w-aux-mne-pdc-c02      .
           multiply  6                    by   w-aux-mne-pdc-c02      .
           add       1                    to   w-aux-mne-pdc-c02      .
           add       5
                     w-aux-mne-pdc-c02  giving w-aux-mne-pdc-c03      .
           move      w-aux-mne-pdc-c03    to   w-aux-mne-pdc-c04      .
           if        w-aux-mne-pdc-c03    >    w-aux-mne-pdc-crb
                     move  w-aux-mne-pdc-crb
                                          to   w-aux-mne-pdc-c03      .
           move      10                   to   w-aux-mne-pdc-c05      .
       aco-951.

           if        w-cod-mne-pdc-num    =    zero  and
                     w-aux-mne-pdc-cbu
                    (w-aux-mne-pdc-c02)   =    "B"
                     move  spaces         to   w-cod-mne-pdc-edt
           else      move  spaces         to   w-cod-mne-pdc-edt
                     if    w-cod-mne-pdc-liv
                                          =    2
                           move  w-aux-mne-pdc-cbu
                                (w-aux-mne-pdc-c02)
                                          to   w-cod-mne-pdc-e2l
                     else  move  w-aux-mne-pdc-cbu
                                (w-aux-mne-pdc-c02)
                                          to   w-cod-mne-pdc-e3l      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      09                   to   v-car                  .
           move      w-aux-mne-pdc-c05    to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-cod-mne-pdc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-pdc-c05    to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-aux-mne-pdc-dbu
                    (w-aux-mne-pdc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-pdc-c02      .
           add       1                    to   w-aux-mne-pdc-c05      .
           if        w-aux-mne-pdc-c02    not  > w-aux-mne-pdc-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-pdc-c02    >    w-aux-mne-pdc-c04
                     go to aco-955.
           if        w-aux-mne-pdc-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      57                   to   v-car                  .
           move      w-aux-mne-pdc-c05    to   v-lin                  .
           move      13                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-pdc-c02      .
           add       1                    to   w-aux-mne-pdc-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-pdc-cpa    to   w-aux-mne-pdc-lt1      .
           move      w-aux-mne-pdc-cpb    to   w-aux-mne-pdc-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-aux-mne-pdc-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *                                                           *
      *    * Esecuzione ricerca totale                                 *
      *    *-----------------------------------------------------------*
       aco-fnd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      "d"                  to   w-aux-mne-pdc-tpf      .
           move      zero                 to   w-aux-mne-pdc-crb      .
       aco-fnd-100.
      *              *-------------------------------------------------*
      *              * Start per descrizione                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-pdc-des-key         .
           move      zero                 to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-900.
       aco-fnd-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale per descrizione             *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-fnd-900.
       aco-fnd-300.
      *              *-------------------------------------------------*
      *              * Test se oltre il max                            *
      *              *-------------------------------------------------*
       aco-fnd-400.
      *              *-------------------------------------------------*
      *              * Selezione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che la descrizione non sia a Spaces    *
      *                  *---------------------------------------------*
           if        rf-pdc-des-pdc       =    spaces
                     go to aco-fnd-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9, e preparazione *
      *                  * 2. valore per il match                      *
      *                  *---------------------------------------------*
           move      rf-pdc-des-pdc       to   w-all-str-alf          .
           move      40                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-aux-mne-pdc-vnr    to   w-all-str-cat (1)      .
      *                  *---------------------------------------------*
      *                  * Match tra i due valori                      *
      *                  *---------------------------------------------*
           perform   all-mch-atz-000      thru all-mch-atz-999        .
      *                  *---------------------------------------------*
      *                  * Se c'e' stato un match : a bufferizzazione  *
      *                  *---------------------------------------------*
           if        w-all-str-flg        =    spaces
                     go to aco-fnd-600.
       aco-fnd-500.
      *                  *---------------------------------------------*
      *                  * Se non c'e' stato alcun match : si ricicla  *
      *                  *---------------------------------------------*
           go to     aco-fnd-200.
       aco-fnd-600.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-mne-pdc-crb      .
      *              *-------------------------------------------------*
      *              * Test se piu' di max records letti               *
      *              *-------------------------------------------------*
           if        w-aux-mne-pdc-crb    >    w-aux-mne-pdc-max
                     go to aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      rf-pdc-cod-pdc       to   w-aux-mne-pdc-cbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-cod-mne       to   w-aux-mne-pdc-mbu
                                              (w-aux-mne-pdc-crb)     .
           move      rf-pdc-des-pdc       to   w-aux-mne-pdc-dbu
                                              (w-aux-mne-pdc-crb)     .
       aco-fnd-800.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     aco-fnd-200.
       aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-fnd-999.
       aco-fnd-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


