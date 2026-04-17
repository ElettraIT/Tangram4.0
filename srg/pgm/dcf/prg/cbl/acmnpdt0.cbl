       Identification Division.
       Program-Id.                                 acmnpdt0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/05/91    *
      *                       Ultima revisione:    NdK del 13/09/05    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice casa produttrice *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdt-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "AC"                 *
      *                                                                *
      *                       w-cod-mne-pdt-cod : codice casa prod.    *
      *                                                                *
      *                       w-cod-mne-pdt-lin : linea codice         *
      *                                                                *
      *                       w-cod-mne-pdt-pos : posizione codice     *
      *                                                                *
      *                       w-cod-mne-pdt-rln : linea ragione soc.   *
      *                                                                *
      *                       w-cod-mne-pdt-rps : posizione rag. soc.  *
      *                                                                *
      *                       w-cod-mne-pdt-vln : linea indirizzo      *
      *                                                                *
      *                       w-cod-mne-pdt-vps : posizione indirizzo  *
      *                                                                *
      *                       w-cod-mne-pdt-lln : linea localita'      *
      *                                                                *
      *                       w-cod-mne-pdt-lps : posizione localita'  *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdt-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-pdt-cod : codice casa prod.    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-pdt-cod : codice casa prod.    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-mne-pdt-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-mne-pdt-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-mne-pdt-cod : codice casa prod.    *
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
      *        * [pdt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfpdt"                          .

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
       01  w-aux-mne-pdt.
           05  w-aux-mne-pdt-c01          pic  9(02)                  .
           05  w-aux-mne-pdt-c02          pic  9(02)                  .
           05  w-aux-mne-pdt-c03          pic  9(02)                  .
           05  w-aux-mne-pdt-c04          pic  9(02)                  .
           05  w-aux-mne-pdt-c05          pic  9(02)                  .
           05  w-aux-mne-pdt-nli          pic  9(02)                  .
           05  w-aux-mne-pdt-m10          pic  x(10)                  .
           05  w-aux-mne-pdt-tpf          pic  x(01)                  .
           05  w-aux-mne-pdt-crb          pic  9(02)                  .
           05  w-aux-mne-pdt-cpb          pic  9(02)                  .
           05  w-aux-mne-pdt-cpa          pic  9(02)                  .
           05  w-aux-mne-pdt-buf
                               occurs 30.
               10  w-aux-mne-pdt-cbu      pic  9(07)                  .
               10  w-aux-mne-pdt-mbu      pic  x(10)                  .
               10  w-aux-mne-pdt-rbu.
                   20  w-aux-mne-pdt-fbu  pic  x(01)                  .
                   20  filler             pic  x(39)                  .
               10  w-aux-mne-pdt-vbu      pic  x(40)                  .
               10  w-aux-mne-pdt-lbu      pic  x(40)                  .
               10  w-aux-mne-pdt-pbu      pic  x(11)                  .
           05  w-aux-mne-pdt-ltp.
               10  filler                 pic  x(07) value "Pagina "  .
               10  w-aux-mne-pdt-lt1      pic  9(01)                  .
               10  filler                 pic  x(04) value " di "     .
               10  w-aux-mne-pdt-lt2      pic  9(01)                  .
           05  w-aux-mne-pdt-rup          pic  x(40)                  .
           05  w-aux-mne-pdt-r01 redefines
               w-aux-mne-pdt-rup.
               10  w-aux-mne-pdt-p00      pic  9(11)                  .
               10  w-aux-mne-pdt-p99      pic  x(29)                  .
           05  w-aux-mne-pdt-r02 redefines
               w-aux-mne-pdt-rup.
               10  w-aux-mne-pdt-f00.
                   15  w-aux-mne-pdt-f01  pic  x(03)                  .
                   15  w-aux-mne-pdt-f02  pic  x(03)                  .
                   15  w-aux-mne-pdt-f03  pic  9(02)                  .
                   15  w-aux-mne-pdt-f04  pic  x(01)                  .
                   15  w-aux-mne-pdt-f05  pic  9(02)                  .
                   15  w-aux-mne-pdt-f06  pic  x(01)                  .
                   15  w-aux-mne-pdt-f07  pic  9(03)                  .
                   15  w-aux-mne-pdt-f08  pic  x(01)                  .
               10  w-aux-mne-pdt-f99      pic  x(24)                  .
           05  w-aux-mne-pdt-rmx.
               10  w-aux-mne-pdt-rch
                               occurs 40  pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice casa produttrice        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcf/prg/cpy/acmnpdt0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-mne-pdt
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
           if        w-cod-mne-pdt-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-mne-pdt-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-mne-pdt-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-mne-pdt-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-mne-pdt-ope    =    "A+" or
                     w-cod-mne-pdt-ope    =    "I+" or
                     w-cod-mne-pdt-ope    =    "F+"
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
      *                  * Open file [pdt]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
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
      *                  * Close file [pdt]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
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
                     move  spaces         to   w-cod-mne-pdt-ope      .
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
           move      "pbol2010"           to   s-pro                  .
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
           move      "pbol2000"           to   s-pro                  .
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
           move      w-cod-mne-pdt-cod    to   w-cod-mne-pdt-num      .
           move      v-edm                to   w-cod-mne-pdt-edm      .
           move      v-ufk                to   w-cod-mne-pdt-ufk      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore originale in alfanumerico    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-pdt-edm    to   v-edm                  .
           move      w-cod-mne-pdt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-pdt-alf      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-mne-pdt-ufk    to   v-ufk                  .
           move      w-cod-mne-pdt-lin    to   v-lin                  .
           move      w-cod-mne-pdt-pos    to   v-pos                  .
           move      w-cod-mne-pdt-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-pdt-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-mne-pdt-ope    =    "A+"
                     go to aco-030.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-mne-pdt-ope    =    "F+"
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
           move      w-cod-mne-pdt-ufk    to   v-ufk                  .
           move      w-cod-mne-pdt-lin    to   v-lin                  .
           move      w-cod-mne-pdt-pos    to   v-pos                  .
           move      w-cod-mne-pdt-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-mne-pdt-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-030.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-pdt-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-mne-pdt-alf      .
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
           move      w-cod-mne-pdt-lin    to   v-lin                  .
           move      w-cod-mne-pdt-pos    to   v-pos                  .
           move      w-cod-mne-pdt-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-075.
      *              *-------------------------------------------------*
      *              * Valore numerico in uscita                       *
      *              *-------------------------------------------------*
           move      w-cod-mne-pdt-num    to   w-cod-mne-pdt-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-pdt-rln      .
           move      zero                 to   w-cod-mne-pdt-rps      .
           move      zero                 to   w-cod-mne-pdt-vln      .
           move      zero                 to   w-cod-mne-pdt-vps      .
           move      zero                 to   w-cod-mne-pdt-lln      .
           move      zero                 to   w-cod-mne-pdt-lps      .
           go to     aco-999.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-pdt"            to   s-var                  .
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
           move      s-num                to   w-cod-mne-pdt-num      .
      *                      *-----------------------------------------*
      *                      * Editing valore numerico                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-pdt-edm    to   v-edm                  .
           move      w-cod-mne-pdt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cod-mne-pdt-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-pdt-ope      .
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
           move      "F+"                 to   w-cod-mne-pdt-ope      .
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
           if        w-cod-mne-pdt-alf    =    spaces
                     move  zero           to   w-cod-mne-pdt-num
                     go to aco-075.
           if        w-cod-mne-pdt-cha (1)
                                          =    spaces
                     go to aco-025.
           move      1                    to   w-aux-mne-pdt-c01      .
       aco-176.
           add       1                    to   w-aux-mne-pdt-c01      .
           if        w-aux-mne-pdt-c01    >    10
                     go to aco-180.
           if        w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   not  = spaces
                     go to aco-176.
       aco-177.
           add       1                    to   w-aux-mne-pdt-c01      .
           if        w-aux-mne-pdt-c01    >    10
                     go to aco-180.
           if        w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   =    spaces
                     go to aco-177
           else      go to aco-025.
       aco-180.
      *              *-------------------------------------------------*
      *              * Conversione valore alfanumerico in numerico     *
      *              *-------------------------------------------------*
           move      zero                 to   w-cod-mne-pdt-num      .
           move      zero                 to   w-aux-mne-pdt-c01      .
       aco-181.
           add       1                    to   w-aux-mne-pdt-c01      .
           if        w-aux-mne-pdt-c01    >    10
                     go to aco-200.
           if        w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   =    spaces
                     go to aco-200.
           if        w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   <    "0" or
                     w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   >    "9"
                     go to aco-225.
           multiply  10                   by   w-cod-mne-pdt-num      .
           move      w-cod-mne-pdt-cha
                    (w-aux-mne-pdt-c01)   to   w-cod-mne-pdt-chn (10) .
           go to     aco-181.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico veramente numerico       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che il valore numerico non superi il   *
      *                  * massimo consentito                          *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdt-num    >    9999999
                     go to aco-025.
      *                  *---------------------------------------------*
      *                  * Editing valore numerico                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-mne-pdt-edm    to   v-edm                  .
           move      w-cod-mne-pdt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Se valore editato uguale al valore alfanu-  *
      *                  * merico : uscita                             *
      *                  *---------------------------------------------*
           if        v-edt                =    w-cod-mne-pdt-alf
                     go to aco-075
      *                  *---------------------------------------------*
      *                  * Altrimenti : rivisualizzazione              *
      *                  *---------------------------------------------*
           else      move  v-edt          to   w-cod-mne-pdt-alf
                     go to aco-050.
       aco-225.
      *              *-------------------------------------------------*
      *              * Se valore alfanumerico non numerico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se valore '-' per ricerca tramite ra-  *
      *                  * gione sociale                               *
      *                  *---------------------------------------------*
           if        w-cod-mne-pdt-alf    =    "-" and
                     w-cod-mne-pdt-rln    not  = zero
                     go to aco-500.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per mnemonico            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records con lo    *
      *                      * stesso mnemonico nel buffer             *
      *                      *-----------------------------------------*
           move      "M"                  to   w-aux-mne-pdt-tpf      .
           move      zero                 to   w-aux-mne-pdt-crb      .
      *                      *-----------------------------------------*
      *                      * Start per mnemonico                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODMNE    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-mne-pdt-alf    to   rf-pdt-cod-mne         .
           move      zero                 to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
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
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Mnemonico letto in area di lavoro di 10 *
      *                      * caratteri                               *
      *                      *-----------------------------------------*
           move      rf-pdt-cod-mne       to   w-aux-mne-pdt-m10      .
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-m10    not  = w-cod-mne-pdt-alf
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-pdt-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso mnemonico                 *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-crb    >    30
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-pdt-cod-pdt       to   w-aux-mne-pdt-cbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-cod-mne       to   w-aux-mne-pdt-mbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-rag-soc       to   w-aux-mne-pdt-rbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-via-pdt       to   w-aux-mne-pdt-vbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-loc-pdt       to   w-aux-mne-pdt-lbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-prt-iva       to   w-aux-mne-pdt-pbu
                                              (w-aux-mne-pdt-crb)     .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     aco-230.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-aux-mne-pdt-crb    =    zero
                     go to aco-275
           else if   w-aux-mne-pdt-crb    =    1
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
           if        w-cod-mne-pdt-rln    =    zero
                     go to aco-276.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-rln    to   v-lin                  .
           move      w-cod-mne-pdt-rps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-276.
           if        w-cod-mne-pdt-vln    =    zero
                     go to aco-277.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-vln    to   v-lin                  .
           move      w-cod-mne-pdt-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-277.
           if        w-cod-mne-pdt-lln    =    zero
                     go to aco-278.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-lln    to   v-lin                  .
           move      w-cod-mne-pdt-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
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
           move      w-aux-mne-pdt-cbu (1)
                                          to   w-cod-mne-pdt-num      .
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
           move      w-aux-mne-pdt-crb    to   w-aux-mne-pdt-cpb      .
           subtract  1                    from w-aux-mne-pdt-cpb      .
           divide    6                    into w-aux-mne-pdt-cpb      .
           add       1                    to   w-aux-mne-pdt-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-aux-mne-pdt-c01      .
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
           move      "         Selezionare la casa produttrice desiderat
      -              "a         "         to   v-alf                  .
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
           move      "Casa produttrice :" to   v-alf                  .
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
           move      w-aux-mne-pdt-c01    to   w-aux-mne-pdt-nli      .
       aco-355.
           if        w-aux-mne-pdt-nli    >    6
                     subtract  6          from w-aux-mne-pdt-nli
                     go to aco-355.
           add       06                   to   w-aux-mne-pdt-nli      .
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-pdt-cbu
                    (w-aux-mne-pdt-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-pdt-rbu
                    (w-aux-mne-pdt-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-pdt-vbu
                    (w-aux-mne-pdt-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-pdt-lbu
                    (w-aux-mne-pdt-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      20                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aux-mne-pdt-pbu
                    (w-aux-mne-pdt-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      w-aux-mne-pdt-mbu
                    (w-aux-mne-pdt-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-375.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-mne-pdt-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-mne-pdt-c01    <    w-aux-mne-pdt-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-mne-pdt-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-mne-pdt-cpa    <    w-aux-mne-pdt-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-mne-pdt-nli    to   v-lin                  .
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
           else      go to aco-375.
       aco-382.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      w-aux-mne-pdt-cbu
                    (w-aux-mne-pdt-c01)   to   w-cod-mne-pdt-num      .
           go to     aco-200.
       aco-384.
           subtract  1                    from w-aux-mne-pdt-c01      .
           if        w-aux-mne-pdt-nli    =    07
                     go to aco-390
           else      go to aco-350.
       aco-386.
           if        w-aux-mne-pdt-c01    <    w-aux-mne-pdt-crb
                     add   1              to   w-aux-mne-pdt-c01
                     go to aco-388
           else      go to aco-375.
       aco-388.
           if        w-aux-mne-pdt-nli    =    12
                     go to aco-390
           else      go to aco-350.
       aco-390.
           perform   aco-950              thru aco-959                .
           go to     aco-350.
       aco-392.
           add       1                    to   w-aux-mne-pdt-cpa      .
           go to     aco-396.
       aco-394.
           subtract  1                    from w-aux-mne-pdt-cpa      .
       aco-396.
           move      w-aux-mne-pdt-cpa    to   w-aux-mne-pdt-c01      .
           multiply  6                    by   w-aux-mne-pdt-c01      .
           subtract  5                    from w-aux-mne-pdt-c01      .
           go to     aco-390.
       aco-398.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-aux-mne-pdt-tpf    not  = "M"
                     go to aco-525
           else      go to aco-025.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se piu' di trenta record con lo stesso mne- *
      *                  * monico impostato                            *
      *                  *---------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol2010"           to   s-pro                  .
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
           move      w-cod-mne-pdt-alf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-500.
      *              *-------------------------------------------------*
      *              * Se ricerca per ragione sociale                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spaces in ragione sociale di comodo         *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-mne-pdt-rup      .
      *                  *---------------------------------------------*
      *                  * Spaces in area per anagrafica               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-rln    to   v-lin                  .
           move      w-cod-mne-pdt-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           if        w-cod-mne-pdt-vln    =    zero
                     go to aco-502.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-vln    to   v-lin                  .
           move      w-cod-mne-pdt-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-502.
           if        w-cod-mne-pdt-lln    =    zero
                     go to aco-525.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-lln    to   v-lin                  .
           move      w-cod-mne-pdt-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-525.
      *                  *---------------------------------------------*
      *                  * Accettazione ragione sociale in uppercase   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-mne-pdt-rln    to   v-lin                  .
           move      w-cod-mne-pdt-rps    to   v-pos                  .
           move      w-aux-mne-pdt-rup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-mne-pdt-rup      .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-550.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-mne-pdt-rln    to   v-lin                  .
           move      w-cod-mne-pdt-rps    to   v-pos                  .
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
           if        w-aux-mne-pdt-rup    =    spaces
                     go to aco-025.
       aco-555.
      *                  *---------------------------------------------*
      *                  * Determinazione se impostazione pari a :     *
      *                  * - Ragione sociale                           *
      *                  * - Partita iva                               *
      *                  * - Codice fiscale                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se partita iva                     *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-p00    not  numeric
                     go to aco-560.
           if        w-aux-mne-pdt-p99    =    spaces
                     move  "P"            to   w-aux-mne-pdt-tpf
                     go to aco-570.
       aco-560.
      *                      *-----------------------------------------*
      *                      * Test se codice fiscale                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-pdt-c01      .
           inspect   w-aux-mne-pdt-f00
                                      tallying w-aux-mne-pdt-c01
                                          for  all   " "              .
           if        w-aux-mne-pdt-c01    >    zero
                     go to aco-565.
           if        w-aux-mne-pdt-f01    not  alphabetic or
                     w-aux-mne-pdt-f02    not  alphabetic or
                     w-aux-mne-pdt-f03    not  numeric    or
                     w-aux-mne-pdt-f04    not  alphabetic or
                     w-aux-mne-pdt-f05    not  numeric    or
                     w-aux-mne-pdt-f06    not  alphabetic or
                     w-aux-mne-pdt-f07    not  numeric    or
                     w-aux-mne-pdt-f08    not  alphabetic
                     go to aco-565.
           if        w-aux-mne-pdt-f99    =    spaces
                     move  "F"            to   w-aux-mne-pdt-tpf
                     go to aco-570.
       aco-565.
      *                      *-----------------------------------------*
      *                      * Se ragione sociale                      *
      *                      *-----------------------------------------*
           move      "R"                  to   w-aux-mne-pdt-tpf      .
       aco-570.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica per :                    *
      *                  * - Ragione sociale                           *
      *                  * - Partita iva                               *
      *                  * - Codice fiscale                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione limite massimo             *
      *                      *-----------------------------------------*
           move      w-aux-mne-pdt-rup    to   w-aux-mne-pdt-rmx      .
           move      40                   to   w-aux-mne-pdt-c01      .
       aco-575.
           if        w-aux-mne-pdt-c01    >    zero
                     if    w-aux-mne-pdt-rch
                          (w-aux-mne-pdt-c01)
                                          =    spaces
                           move     "z"   to   w-aux-mne-pdt-rch
                                              (w-aux-mne-pdt-c01)
                           subtract 1     from w-aux-mne-pdt-c01
                           go to    aco-575.
      *                      *-----------------------------------------*
      *                      * Azzeramento contatore records nel buf-  *
      *                      * fer                                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-mne-pdt-crb      .
      *                      *-----------------------------------------*
      *                      * Start per :                             *
      *                      * - Ragione sociale                       *
      *                      * - Partita iva                           *
      *                      * - Codice fiscale                        *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           if        w-aux-mne-pdt-tpf    =    "P"
                     move  "PRTIVA    "   to   f-key
                     move  w-aux-mne-pdt-p00
                                          to   rf-pdt-prt-iva
           else if   w-aux-mne-pdt-tpf    =    "F"
                     move  "CODFIS    "   to   f-key
                     move  w-aux-mne-pdt-f00
                                          to   rf-pdt-cod-fis
           else      move  "RAGKEY    "   to   f-key
                     move  w-aux-mne-pdt-rup
                                          to   rf-pdt-rag-key         .
           move      zero                 to   rf-pdt-cod-pdt         .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *                      *-----------------------------------------*
      *                      * Se start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
       aco-600.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale                     *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofpdt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdt                 .
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-250.
      *                      *-----------------------------------------*
      *                      * Test se oltre il max                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-tpf    =    "P"
                     if    rf-pdt-prt-iva =    w-aux-mne-pdt-p00
                           go to  aco-625
                     else  go to  aco-250.
           if        w-aux-mne-pdt-tpf    =    "F"
                     if    rf-pdt-cod-fis =    w-aux-mne-pdt-f00
                           go to  aco-625
                     else  go to  aco-250.
           if        rf-pdt-rag-key       >    w-aux-mne-pdt-rmx
                     go to aco-250.
       aco-625.
      *                      *-----------------------------------------*
      *                      * Incremento numero records nel buffer    *
      *                      *-----------------------------------------*
           add       1                    to   w-aux-mne-pdt-crb      .
      *                      *-----------------------------------------*
      *                      * Test se piu' di trenta records letti    *
      *                      * con lo stesso valore                    *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-crb    >    30
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      rf-pdt-cod-pdt       to   w-aux-mne-pdt-cbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-cod-mne       to   w-aux-mne-pdt-mbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-rag-soc       to   w-aux-mne-pdt-rbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-via-pdt       to   w-aux-mne-pdt-vbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-loc-pdt       to   w-aux-mne-pdt-lbu
                                              (w-aux-mne-pdt-crb)     .
           move      rf-pdt-prt-iva       to   w-aux-mne-pdt-pbu
                                              (w-aux-mne-pdt-crb)     .
      *                      *-----------------------------------------*
      *                      * Se raggiunti il trentesimo record in    *
      *                      * interrogazione per partita iva o per    *
      *                      * codice fiscale : come per fine file,    *
      *                      * altrimenti : riciclo in lettura         *
      *                      *-----------------------------------------*
           if        w-aux-mne-pdt-tpf    not  = "P" and
                     w-aux-mne-pdt-tpf    not  = "F"
                     go to aco-600.
           if        w-aux-mne-pdt-crb    =    30
                     go to aco-250
           else      go to aco-600.
       aco-650.
      *                      *-----------------------------------------*
      *                      * Se piu' di trenta record con la stessa  *
      *                      * ragione sociale impostata               *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol2010"           to   s-pro                  .
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
           move      "rag-soc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      20                   to   s-car                  .
           move      w-aux-mne-pdt-rup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           go to     aco-125.
       aco-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-mne-pdt-c01    to   w-aux-mne-pdt-c02      .
           add       5                    to   w-aux-mne-pdt-c02      .
           divide    6                    into w-aux-mne-pdt-c02      .
           move      w-aux-mne-pdt-c02    to   w-aux-mne-pdt-cpa      .
           subtract  1                    from w-aux-mne-pdt-c02      .
           multiply  6                    by   w-aux-mne-pdt-c02      .
           add       1                    to   w-aux-mne-pdt-c02      .
           add       5
                     w-aux-mne-pdt-c02  giving w-aux-mne-pdt-c03      .
           move      w-aux-mne-pdt-c03    to   w-aux-mne-pdt-c04      .
           if        w-aux-mne-pdt-c03    >    w-aux-mne-pdt-crb
                     move  w-aux-mne-pdt-crb
                                          to   w-aux-mne-pdt-c03      .
           move      07                   to   w-aux-mne-pdt-c05      .
       aco-951.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-aux-mne-pdt-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-aux-mne-pdt-cbu
                    (w-aux-mne-pdt-c02)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-mne-pdt-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-mne-pdt-rbu
                    (w-aux-mne-pdt-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-pdt-c02      .
           add       1                    to   w-aux-mne-pdt-c05      .
           if        w-aux-mne-pdt-c02    not  > w-aux-mne-pdt-c03
                     go to aco-951.
       aco-952.
           if        w-aux-mne-pdt-c02    >    w-aux-mne-pdt-c04
                     go to aco-955.
           if        w-aux-mne-pdt-crb    not  > 6
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-aux-mne-pdt-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-mne-pdt-c02      .
           add       1                    to   w-aux-mne-pdt-c05      .
           go to     aco-952.
       aco-955.
           move      w-aux-mne-pdt-cpa    to   w-aux-mne-pdt-lt1      .
           move      w-aux-mne-pdt-cpb    to   w-aux-mne-pdt-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-aux-mne-pdt-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.
