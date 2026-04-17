       Identification Division.
       Program-Id.                                 acmnzcc0           .
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
      * Descrizione pgm:   Modulo accettazione codice causale di       *
      *                    contabilita' generale                       *
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
      *              Input  : w-cod-mne-zcc-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zcc-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-zcc-cod : codice causale       *
      *                                                                *
      *                       w-cod-mne-zcc-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-zcc-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-zcc-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-mne-zcc-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zcc-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zcc-cod : codice causale       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zcc-cod : codice causale       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-zcc-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-zcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-zcc-cod : codice causale       *
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
      *        * [zcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzcc"                          .

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
       01  w-aux-mne-zcc.
           05  w-aux-mne-zcc-c01          pic  9(02)                  .
           05  w-aux-mne-zcc-c02          pic  9(02)                  .
           05  w-aux-mne-zcc-c03          pic  9(02)                  .
           05  w-aux-mne-zcc-c04          pic  9(02)                  .
           05  w-aux-mne-zcc-c05          pic  9(02)                  .
           05  w-aux-mne-zcc-nli          pic  9(02)                  .
           05  w-aux-mne-zcc-m03          pic  x(03)                  .
           05  w-aux-mne-zcc-tpf          pic  x(01)                  .
           05  w-aux-mne-zcc-crb          pic  9(02)                  .
           05  w-aux-mne-zcc-cpb          pic  9(02)                  .
           05  w-aux-mne-zcc-cpa          pic  9(02)                  .
           05  w-aux-mne-zcc-max          pic  9(02) value 54         .
           05  w-aux-mne-zcc-buf occurs 54.
               10  w-aux-mne-zcc-cbu      pic  9(03)                  .
               10  w-aux-mne-zcc-mbu      pic  x(03)                  .
               10  w-aux-mne-zcc-dbu      pic  x(40)                  .
               10  w-aux-mne-zcc-bbu      pic  x(01)                  .
               10  w-aux-mne-zcc-tbu      pic  x(01)                  .
           05  w-aux-mne-zcc-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-zcc-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-zcc-lt2      pic  9(01)                  .
           05  w-aux-mne-zcc-dup          pic  x(30)                  .
           05  w-aux-mne-zcc-vnr          pic  x(20)                  .
           05  w-aux-mne-zcc-dmx.
               10  w-aux-mne-zcc-dch
                               occurs 30  pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Si/no movimento di bilancio                *
      *        *-------------------------------------------------------*
           05  w-exp-snx-bil.
               10  w-exp-snx-bil-num      pic  9(02)       value 3    .
               10  w-exp-snx-bil-lun      pic  9(02)       value 30   .
               10  w-exp-snx-bil-tbl.
                   15  filler             pic  x(30) value
                            "Di contabilita' ordinaria     "          .
                   15  filler             pic  x(30) value
                            "Di rettifica di bilancio      "          .
                   15  filler             pic  x(30) value
                            "Di chiusura/apertura bilancio "          .
      *        *-------------------------------------------------------*
      *        * Work per : Tipo movimento iva                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-moi.
               10  w-exp-tip-moi-num      pic  9(02)       value 11   .
               10  w-exp-tip-moi-lun      pic  9(02)       value 36   .
               10  w-exp-tip-moi-tbl.
                   15  filler             pic  x(36) value
                            "Movimento non iva                   "    .
                   15  filler             pic  x(36) value
                            "Dare clienti it. o esteri non CEE   "    .
                   15  filler             pic  x(36) value
                            "Avere clienti it. o esteri non CEE  "    .
                   15  filler             pic  x(36) value
                            "Corrispettivi                       "    .
                   15  filler             pic  x(36) value
                            "Dare fornitori it. o esteri non CEE "    .
                   15  filler             pic  x(36) value
                            "Avere fornitori it. o esteri non CEE"    .
                   15  filler             pic  x(36) value
                            "Bolla doganale                      "    .
                   15  filler             pic  x(36) value
                            "Dare clienti esteri CEE             "    .
                   15  filler             pic  x(36) value
                            "Avere clienti esteri CEE            "    .
                   15  filler             pic  x(36) value
                            "Dare fornitori esteri CEE           "    .
                   15  filler             pic  x(36) value
                            "Avere fornitori esteri CEE          "    .

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
      *    * Link-area per accettazione codice causale contabile       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/acmnzcc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-zcc
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
           if        w-cod-mne-zcc-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-zcc-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-zcc-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-zcc-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-zcc-ope    =    "A+" or
                     w-cod-mne-zcc-ope    =    "I+" or
                     w-cod-mne-zcc-ope    =    "F+"
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
      *                  * Open file [zcc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
      *                  * Close file [zcc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
                     move  spaces         to   w-cod-mne-zcc-ope      .
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
           move      "pcge0310"           to   s-pro                  .
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
           move      "pcge0300"           to   s-pro                  .
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
           move      w-cod-mne-zcc-cod    to   w-cod-mne-zcc-num      .
           move      v-edm                to   w-cod-mne-zcc-edm      .
           move      v-ufk                to   w-cod-mne-zcc-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zcc-edm    to   v-edm                  .
           move      w-cod-mne-zcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zcc-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-zcc-ufk    to   v-ufk                  .
           move      w-cod-mne-zcc-lin    to   v-lin                  .
           move      w-cod-mne-zcc-pos    to   v-pos                  .
           move      w-cod-mne-zcc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zcc-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-zcc-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-zcc-ope    =    "F+"
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
           move      w-cod-mne-zcc-ufk    to   v-ufk                  .
           move      w-cod-mne-zcc-lin    to   v-lin                  .
           move      w-cod-mne-zcc-pos    to   v-pos                  .
           move      w-cod-mne-zcc-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-zcc-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-zcc-alf      .
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
           move      w-cod-mne-zcc-lin    to   v-lin                  .
           move      w-cod-mne-zcc-pos    to   v-pos                  .
           move      w-cod-mne-zcc-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-zcc-num    to   w-cod-mne-zcc-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zcc-dln      .
           move      zero                 to   w-cod-mne-zcc-dps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-cau"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-zcc-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-zcc-edm    to   v-edm                  .
           move      w-cod-mne-zcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-zcc-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-zcc-ope      .
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
           move      "F+"                 to   w-cod-mne-zcc-ope      .
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
           if        w-cod-mne-zcc-alf    =    spaces
                     move  zero           to   w-cod-mne-zcc-num
                     go to aco-075.
           if        w-cod-mne-zcc-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-zcc-c01      .
       aco-176.
           add       1                    to   w-aux-mne-zcc-c01      .
           if        w-aux-mne-zcc-c01    >    03
                     go to aco-180.
           if        w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-zcc-c01      .
           if        w-aux-mne-zcc-c01    >    03
                     go to aco-180.
           if        w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-zcc-num      .
           move      zero                 to   w-aux-mne-zcc-c01      .
       aco-181.
           add       1                    to   w-aux-mne-zcc-c01      .
           if        w-aux-mne-zcc-c01    >    03
                     go to aco-200.
           if        w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   <    "0" or
                     w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-mne-zcc-num      .
           move      w-cod-mne-zcc-cha
                    (w-aux-mne-zcc-c01)   to   w-cod-mne-zcc-chn (03) .
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
           move      w-cod-mne-zcc-edm    to   v-edm                  .
           move      w-cod-mne-zcc-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-zcc-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-zcc-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite      *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           if        w-cod-mne-zcc-alf    =    "-" and
                     w-cod-mne-zcc-dln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-zcc-tpf      .
           move      zero                 to   w-aux-mne-zcc-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-zcc-alf    to   rf-zcc-mne-cau         .
           move      zero                 to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 3  *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-zcc-mne-cau       to   w-aux-mne-zcc-m03      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-zcc-m03    not  = w-cod-mne-zcc-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zcc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zcc-crb    >    w-aux-mne-zcc-max
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zcc-cod-cau       to   w-aux-mne-zcc-cbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-mne-cau       to   w-aux-mne-zcc-mbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-des-cau       to   w-aux-mne-zcc-dbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-snx-bil       to   w-aux-mne-zcc-bbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-tip-moi       to   w-aux-mne-zcc-tbu
                                              (w-aux-mne-zcc-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so mnemonico                                *
      *                  *---------------------------------------------*
           if        w-aux-mne-zcc-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-zcc-crb    =    1
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
           if        w-cod-mne-zcc-dln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-mne-zcc-dln    to   v-lin                  .
           move      w-cod-mne-zcc-dps    to   v-pos                  .
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
           move      w-aux-mne-zcc-cbu (1)
                                          to   w-cod-mne-zcc-num      .
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
           move      w-aux-mne-zcc-crb    to   w-aux-mne-zcc-cpb      .
           subtract  1                    from w-aux-mne-zcc-cpb      .
           divide    6                    into w-aux-mne-zcc-cpb      .
           add       1                    to   w-aux-mne-zcc-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-zcc-c01      .
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
           move      " Selezionare la causale desiderata "
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
           move      "Codice causale   :" to   v-alf                  .
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
           move      "Tipo movimento   :" to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Tipo mov. iva    :" to   v-alf                  .
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
           move      w-aux-mne-zcc-c01    to   w-aux-mne-zcc-nli      .
       aco-355.
           if        w-aux-mne-zcc-nli    >    6
                     subtract  6          from w-aux-mne-zcc-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-zcc-nli      .
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice causale                      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-zcc-cbu
                    (w-aux-mne-zcc-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Descrizione causale                 *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-zcc-dbu
                    (w-aux-mne-zcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Tipo movimento                      *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-snx-bil-lun    to   v-car                  .
           move      w-exp-snx-bil-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-snx-bil-tbl    to   v-txt                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-aux-mne-zcc-bbu
                    (w-aux-mne-zcc-c01)   =    "N"
                     move  01             to   v-num
           else if   w-aux-mne-zcc-bbu
                    (w-aux-mne-zcc-c01)   =    "S"
                     move  02             to   v-num
           else if   w-aux-mne-zcc-bbu
                    (w-aux-mne-zcc-c01)   =    "X"
                     move  03             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Tipo movimento IVA                  *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-moi-lun    to   v-car                  .
           move      w-exp-tip-moi-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-moi-tbl    to   v-txt                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "0"
                     move  01             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "1"
                     move  02             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "2"
                     move  03             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "3"
                     move  04             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "4"
                     move  05             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "5"
                     move  06             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "6"
                     move  07             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "A"
                     move  08             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "B"
                     move  09             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "D"
                     move  10             to   v-num
           else if   w-aux-mne-zcc-tbu
                    (w-aux-mne-zcc-c01)   =    "E"
                     move  11             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Mnemonico                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-zcc-mbu
                    (w-aux-mne-zcc-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           if        w-aux-mne-zcc-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-zcc-c01    <    w-aux-mne-zcc-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-zcc-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-zcc-cpa    <    w-aux-mne-zcc-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-zcc-nli    to   v-lin                  .
           move      29                   to   v-pos                  .
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
           move      w-aux-mne-zcc-cbu
                    (w-aux-mne-zcc-c01)   to   w-cod-mne-zcc-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-zcc-c01      .
           if        w-aux-mne-zcc-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-zcc-c01    <    w-aux-mne-zcc-crb
                     add   1              to   w-aux-mne-zcc-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-zcc-nli    =    12
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-zcc-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-zcc-cpa      .
       aco-396.
           move      w-aux-mne-zcc-cpa    to   w-aux-mne-zcc-c01      .
           multiply  6                    by   w-aux-mne-zcc-c01      .
           subtract  5                    from w-aux-mne-zcc-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-zcc-tpf    =    "D"
                     go to aco-510
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pcge0310"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-025.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-aux-mne-zcc-tpf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "cod-mne"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      w-cod-mne-zcc-alf    to   s-alf                  .
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
           move      spaces               to   w-aux-mne-zcc-dup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-cod-mne-zcc-dln    to   v-lin                  .
           move      w-cod-mne-zcc-dps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-510.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-zcc-dln    to   v-lin                  .
           move      w-cod-mne-zcc-dps    to   v-pos                  .
           move      w-aux-mne-zcc-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-zcc-dup      .
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
           move      30                   to   v-car                  .
           move      w-cod-mne-zcc-dln    to   v-lin                  .
           move      w-cod-mne-zcc-dps    to   v-pos                  .
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
           if        w-aux-mne-zcc-dup    =    spaces
                     go to aco-510.
      *                      *-----------------------------------------*
      *                      * Normalizzazione del valore impostato in *
      *                      * formato privo di spaces e di caratteri  *
      *                      * diversi da A..Z - 0..9, e salvataggio   *
      *                      * del risultato                           *
      *                      *-----------------------------------------*
           move      w-aux-mne-zcc-dup    to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-aux-mne-zcc-vnr      .
      *                      *-----------------------------------------*
      *                      * Se il valore normalizzato e' a spaces : *
      *                      * rientro ad accettazione descrizione     *
      *                      * in uppercase                            *
      *                      *-----------------------------------------*
           if        w-aux-mne-zcc-vnr    =    spaces
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
           if        w-aux-mne-zcc-crb    >    w-aux-mne-zcc-max
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
           if        w-aux-mne-zcc-dup    =    spaces
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per descrizione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione descrizione massima        *
      *                      *-----------------------------------------*
           move      w-aux-mne-zcc-dup    to   w-aux-mne-zcc-dmx      .
           move      30                   to   w-aux-mne-zcc-c01      .
       aco-720.
           if        w-aux-mne-zcc-c01    >    zero
                     if    w-aux-mne-zcc-dch
                          (w-aux-mne-zcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-zcc-dch
                                              (w-aux-mne-zcc-c01)
                           subtract 1     from w-aux-mne-zcc-c01
                           go to    aco-720.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "D"                  to   w-aux-mne-zcc-tpf      .
           move      zero                 to   w-aux-mne-zcc-crb      .
       aco-740.
      *                      *-----------------------------------------*
      *                      * Start per descrizione                   *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-mne-zcc-dup    to   rf-zcc-des-key         .
           move      zero                 to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        rf-zcc-des-key       >    w-aux-mne-zcc-dmx
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-zcc-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-zcc-crb    >    w-aux-mne-zcc-max
                     go to aco-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-zcc-cod-cau       to   w-aux-mne-zcc-cbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-mne-cau       to   w-aux-mne-zcc-mbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-des-cau       to   w-aux-mne-zcc-dbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-snx-bil       to   w-aux-mne-zcc-bbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-tip-moi       to   w-aux-mne-zcc-tbu
                                              (w-aux-mne-zcc-crb)     .
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
           move      "pcge0310"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-510.
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      w-aux-mne-zcc-tpf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "PV"                 to   s-ope                  .
           move      "des-cau"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      30                   to   s-car                  .
           move      w-aux-mne-zcc-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-zcc-c01    to   w-aux-mne-zcc-c02      .
           add       5                    to   w-aux-mne-zcc-c02      .
           divide    6                    into w-aux-mne-zcc-c02      .
           move      w-aux-mne-zcc-c02    to   w-aux-mne-zcc-cpa      .
           subtract  1                    from w-aux-mne-zcc-c02      .
           multiply  6                    by   w-aux-mne-zcc-c02      .
           add       1                    to   w-aux-mne-zcc-c02      .
           add       5
                     w-aux-mne-zcc-c02  giving w-aux-mne-zcc-c03      .
           move      w-aux-mne-zcc-c03    to   w-aux-mne-zcc-c04      .
           if        w-aux-mne-zcc-c03    >    w-aux-mne-zcc-crb
                     move  w-aux-mne-zcc-crb
                                          to   w-aux-mne-zcc-c03      .
           move      07                   to   w-aux-mne-zcc-c05      .
       aco-951.
      *                  *---------------------------------------------*
      *                  * Codice causale                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-zcc-c05    to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aux-mne-zcc-cbu
                    (w-aux-mne-zcc-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione causale                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      w-aux-mne-zcc-c05    to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-aux-mne-zcc-dbu
                    (w-aux-mne-zcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zcc-c02      .
           add       1                    to   w-aux-mne-zcc-c05      .
           if        w-aux-mne-zcc-c02    not  > w-aux-mne-zcc-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-zcc-c02    >    w-aux-mne-zcc-c04
                     go to aco-955.
           if        w-aux-mne-zcc-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-zcc-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-zcc-c02      .
           add       1                    to   w-aux-mne-zcc-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-zcc-cpa    to   w-aux-mne-zcc-lt1      .
           move      w-aux-mne-zcc-cpb    to   w-aux-mne-zcc-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-zcc-ltp    to   v-alf                  .
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
           move      "d"                  to   w-aux-mne-zcc-tpf      .
           move      zero                 to   w-aux-mne-zcc-crb      .
       aco-fnd-100.
      *              *-------------------------------------------------*
      *              * Start per descrizione                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DESKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      spaces               to   rf-zcc-des-key         .
           move      zero                 to   rf-zcc-cod-cau         .
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
           move      "pgm/cge/fls/ioc/obj/iofzcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcc                 .
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
           if        rf-zcc-des-cau       =    spaces
                     go to aco-fnd-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione A..Z - 0..9, e preparazione *
      *                  * 2. valore per il match                      *
      *                  *---------------------------------------------*
           move      rf-zcc-des-cau       to   w-all-str-alf          .
           move      30                   to   w-all-str-lun          .
           perform   all-nor-atz-000      thru all-nor-atz-999        .
           move      w-all-str-alf        to   w-all-str-cat (2)      .
      *                  *---------------------------------------------*
      *                  * Preparazione 1. valore per il match         *
      *                  *---------------------------------------------*
           move      w-aux-mne-zcc-vnr    to   w-all-str-cat (1)      .
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
           add       1                    to   w-aux-mne-zcc-crb      .
      *              *-------------------------------------------------*
      *              * Test se piu' di max records letti               *
      *              *-------------------------------------------------*
           if        w-aux-mne-zcc-crb    >    w-aux-mne-zcc-max
                     go to aco-fnd-900.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      rf-zcc-cod-cau       to   w-aux-mne-zcc-cbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-mne-cau       to   w-aux-mne-zcc-mbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-des-cau       to   w-aux-mne-zcc-dbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-snx-bil       to   w-aux-mne-zcc-bbu
                                              (w-aux-mne-zcc-crb)     .
           move      rf-zcc-tip-moi       to   w-aux-mne-zcc-tbu
                                              (w-aux-mne-zcc-crb)     .
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
