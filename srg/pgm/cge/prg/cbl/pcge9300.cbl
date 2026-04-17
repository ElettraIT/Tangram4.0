       Identification Division.
       Program-Id.                                 pcge9300           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    uti                 *
      *                                   Fase:    cge930              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/03/23    *
      *                       Ultima revisione:    NdK del 24/03/23    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Primanota di contabilita' generale senza    *
      *                    controlli sulle date                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Questo programma prepara una variabile globale di i.p.c., con  *
      * nome variabile 'cge300-sub-pgm', e poi richiama e cancella il  *
      * programma 'pcge3000', che riconosce la variabile globale ed    *
      * accetta solo modifiche ad alcuni dati prestabiliti             *
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
                     "cge"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "uti"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cge930"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcge9300"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "          PRIMANOTA - UTILITA'          "       .

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
      *        * Variabile 'cge300-sub-pgm'                            *
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
      *                *  - UT     : Il programma e' eseguito come     *
      *                *             sottoprogramma per la gestione    *
      *                *             della primanota - utility         *
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
           perform   fas-cge-930-000      thru fas-cge-930-999        .
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
       fas-cge-930-000.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'cge300-sub-pgm'  *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cge300-sub-pgm"     to   s-var                  .
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
           move      "UT"                 to   w-ipc-sub-pgm-ind      .
           move      w-ipc-sub-pgm-val    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del programma di primanota             *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge3000"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
      *              *-------------------------------------------------*
      *              * Cancellazione del programma di primanota        *
      *              *-------------------------------------------------*
           move      "pgm/cge/prg/obj/pcge3000"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel   s-pat                                             .
       fas-cge-930-999.
           exit.
