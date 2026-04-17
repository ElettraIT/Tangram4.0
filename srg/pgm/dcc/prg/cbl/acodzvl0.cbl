       Identification Division.
       Program-Id.                                 acodzvl0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 01/08/91    *
      *                       Ultima revisione:    NdK del 05/03/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice valuta           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-zvl-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-zvl-cod : codice valuta        *
      *                                                                *
      *                       w-cod-cod-zvl-lin : linea                *
      *                                                                *
      *                       w-cod-cod-zvl-pos : posizione            *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-zvl-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-zvl-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-zvl-cod : codice valuta        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-zvl-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-zvl-cod : codice valuta        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-zvl-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-zvl-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-zvl-cod : codice valuta        *
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
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

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
       01  w-aux-cod-zvl.
           05  w-aux-cod-zvl-c01          pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice valuta                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acodzvl0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-zvl
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
           if        w-cod-cod-zvl-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-zvl-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-zvl-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-zvl-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-zvl-ope    =    "A+" or
                     w-cod-cod-zvl-ope    =    "I+" or
                     w-cod-cod-zvl-ope    =    "F+"
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
      *                  * Open file [zvl]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
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
      *                  * Close file [zvl]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
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
                     move  spaces         to   w-cod-cod-zvl-ope      .
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
           move      "pdcc0210"           to   s-pro                  .
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
           move      "pdcc0200"           to   s-pro                  .
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
           move      w-cod-cod-zvl-cod    to   w-cod-cod-zvl-alf      .
           move      v-edm                to   w-cod-cod-zvl-edm      .
           move      v-ufk                to   w-cod-cod-zvl-ufk      .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-zvl-ufk    to   v-ufk                  .
           move      w-cod-cod-zvl-lin    to   v-lin                  .
           move      w-cod-cod-zvl-pos    to   v-pos                  .
           move      w-cod-cod-zvl-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-zvl-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Test se rientro da impostazione                 *
      *              *-------------------------------------------------*
           if        w-cod-cod-zvl-ope    =    "A+"
                     go to aco-200.
      *              *-------------------------------------------------*
      *              * Test se rientro da Find                         *
      *              *-------------------------------------------------*
           if        w-cod-cod-zvl-ope    =    "F+"
                     go to aco-500.
       aco-100.
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
           move      w-cod-cod-zvl-ufk    to   v-ufk                  .
           move      w-cod-cod-zvl-lin    to   v-lin                  .
           move      w-cod-cod-zvl-pos    to   v-pos                  .
           move      w-cod-cod-zvl-alf    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-zvl-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Tipo operazione a : non-continuazione           *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
      *              *-------------------------------------------------*
      *              * Salvataggio valore alfanumerico impostato       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-cod-cod-zvl-alf      .
      *              *-------------------------------------------------*
      *              * Test se Exit o Delt                             *
      *              *-------------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-700.
       aco-300.
      *              *-------------------------------------------------*
      *              * Rivisualizzazione                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-zvl-lin    to   v-lin                  .
           move      w-cod-cod-zvl-pos    to   v-pos                  .
           move      w-cod-cod-zvl-alf    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-400.
      *              *-------------------------------------------------*
      *              * Valore finale in uscita                         *
      *              *-------------------------------------------------*
           move      w-cod-cod-zvl-alf    to   w-cod-cod-zvl-cod      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     aco-999.
       aco-500.
      *              *-------------------------------------------------*
      *              * Se rientro da Find                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select                 *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "sgl-vlt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata                 *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     go to aco-100.
      *                  *---------------------------------------------*
      *                  * Se selezione effettuata                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio valore alfanumerico         *
      *                      *-----------------------------------------*
           move      s-alf                to   w-cod-cod-zvl-alf      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-zvl-ope      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore e preparazione   *
      *                      * valore in uscita                        *
      *                      *-----------------------------------------*
           go to     aco-300.
       aco-600.
      *              *-------------------------------------------------*
      *              * Preparazione uscita per Find precablato         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : esecuzione Find         *
      *                  *---------------------------------------------*
           move      "F+"                 to   w-cod-cod-zvl-ope      .
      *                  *---------------------------------------------*
      *                  * Simulazione tasto Find                      *
      *                  *---------------------------------------------*
           move      "FIND"               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-700.
      *              *-------------------------------------------------*
      *              * Test se valore "-"                              *
      *              *-------------------------------------------------*
           if        w-cod-cod-zvl-alf    =    "-"
                     go to aco-100.
      *              *-------------------------------------------------*
      *              * Test se blanks embedded                         *
      *              *-------------------------------------------------*
           if        w-cod-cod-zvl-alf    =    spaces
                     go to aco-400.
           if        w-cod-cod-zvl-cha (1)
                                          =    spaces
                     go to aco-100.
           move      1                    to   w-aux-cod-zvl-c01      .
       aco-800.
           add       1                    to   w-aux-cod-zvl-c01      .
           if        w-aux-cod-zvl-c01    >    03
                     go to aco-400.
           if        w-cod-cod-zvl-cha
                    (w-aux-cod-zvl-c01)   not  = spaces
                     go to aco-800.
       aco-900.
           add       1                    to   w-aux-cod-zvl-c01      .
           if        w-aux-cod-zvl-c01    >    03
                     go to aco-400.
           if        w-cod-cod-zvl-cha
                    (w-aux-cod-zvl-c01)   =    spaces
                     go to aco-900
           else      go to aco-100.
       aco-999.
           exit.
