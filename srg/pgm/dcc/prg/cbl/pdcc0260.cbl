       Identification Division.
       Program-Id.                                 pdcc0260           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:    prs                 *
      *                                   Fase:    dcc026              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/12/95    *
      *                       Ultima revisione:    NdK del 05/03/04    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazione su cambi valute              *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Questo programma prepara una variabile globale di i.p.c., con  *
      * nome variabile 'dcc025-sub-pgm', e poi richiama e cancella il  *
      * programma 'pdcc0250', che riconosce la variabile globale ed    *
      * accetta solo la visualizzazione                                *
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
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "dcc"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "prs"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dcc026"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdcc0260"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     INTERROGAZIONE SU CAMBI VALUTE     "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c.                         *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile 'dcc025-sub-pgm'                            *
      *        *-------------------------------------------------------*
           05  w-ipc-sub-pgm.
      *            *---------------------------------------------------*
      *            * Valore della variabile                            *
      *            *---------------------------------------------------*
               10  w-ipc-sub-pgm-val.
      *                *-----------------------------------------------*
      *                * Dati relativi all'identificazione             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ide.
      *                    *-------------------------------------------*
      *                    * Sistema applicativo                       *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-sap
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Area gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-arg
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Settore gestionale                        *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-set
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Fase gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-fas
                                          pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Sigla interna del programma               *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-pro
                                          pic  x(10)                  .
      *                    *-------------------------------------------*
      *                    * Descrizione del programma                 *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-des
                                          pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indicatore                                    *
      *                *  - Spaces : Nessun significato                *
      *                *  - VI     : Il programma e' eseguito come     *
      *                *             sottoprogramma per la sola        *
      *                *             visualizzazione                   *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ind  pic  x(02)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Esecuzione programma                            *
      *              *-------------------------------------------------*
           perform   fas-dcc-026-000      thru fas-dcc-026-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione programma                                      *
      *    *-----------------------------------------------------------*
       fas-dcc-026-000.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'dcc025-sub-pgm'  *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dcc025-sub-pgm"     to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      spaces               to   w-ipc-sub-pgm-val      .
           move      i-ide-sap            to   w-ipc-sub-pgm-sap      .
           move      i-ide-arg            to   w-ipc-sub-pgm-arg      .
           move      i-ide-set            to   w-ipc-sub-pgm-set      .
           move      i-ide-fas            to   w-ipc-sub-pgm-fas      .
           move      i-ide-pro            to   w-ipc-sub-pgm-pro      .
           move      i-ide-des            to   w-ipc-sub-pgm-des      .
           move      "VI"                 to   w-ipc-sub-pgm-ind      .
           move      w-ipc-sub-pgm-val    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del programma di gestione cambi valute *
      *              *-------------------------------------------------*
           move      "pgm/dcc/prg/obj/pdcc0250"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
      *              *-------------------------------------------------*
      *              * Cancellazione del programma di gestione cambi   *
      *              * valute                                          *
      *              *-------------------------------------------------*
           move      "pgm/dcc/prg/obj/pdcc0250"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel   s-pat                                             .
       fas-dcc-026-999.
           exit.
