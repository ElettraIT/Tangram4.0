       Identification Division.
       Program-Id.                                 acoddpz0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/09/91    *
      *                       Ultima revisione:    NdK del 28/06/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice dipendenza del-  *
      *                    l'azienda                                   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dpz-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-dpz-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dpz-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-dpz-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-dpz-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dpz-dps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dpz-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dpz-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dpz-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dpz-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-dpz-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-dpz-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dpz-dps : posizione descriz.   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dpz-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dpz-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dpz-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-dpz-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-dpz-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dpz-dps : posizione descriz.   *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-dpz-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dpz-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dpz-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dpz-lin : linea a video        *
      *                                                                *
      *                       w-cod-cod-dpz-pos : posizione a video    *
      *                                                                *
      *                       w-cod-cod-dpz-dln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dpz-dps : posizione descriz.   *
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
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Work-area per le personalizzazioni                        *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione per gestione dipendenze parallele   *
      *        *-------------------------------------------------------*
           05  w-prs-snx-gdp.
      *            *---------------------------------------------------*
      *            * Si/No gestione dipendenze parallele               *
      *            *---------------------------------------------------*
               10  w-prs-snx-gdp-snx      pic  x(01)     value spaces .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-dpz
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
           if        w-cod-cod-dpz-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-dpz-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-dpz-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-dpz-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-dpz-ope    =    "A+" or
                     w-cod-cod-dpz-ope    =    "I+" or
                     w-cod-cod-dpz-ope    =    "F+"
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
      *              * Lettura personalizzazione per Si/No gestione    *
      *              * dipendenze parallele, se necessario             *
      *              *-------------------------------------------------*
           perform   prs-snx-gdp-000      thru prs-snx-gdp-999        .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
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
                     move  spaces         to   w-cod-cod-dpz-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per Si/No gestione dipendenze   *
      *    * parallele                                                 *
      *    *-----------------------------------------------------------*
       prs-snx-gdp-000.
      *              *-------------------------------------------------*
      *              * Test se lettura necessaria                      *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = spaces
                     go to prs-snx-gdp-999.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[snx-gdp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-gdp-snx
           else      move  spaces         to   w-prs-snx-gdp-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = "S"
                     move  "N"            to   w-prs-snx-gdp-snx      .
       prs-snx-gdp-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pazi0010"           to   s-pro                  .
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
           move      "pazi0000"           to   s-pro                  .
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
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      w-cod-cod-dpz-cod    to   w-cod-cod-dpz-s01      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing v-edm                   *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-cod-dpz-s70      .
      *                  *---------------------------------------------*
      *                  * User function-keys eventualmente epurate    *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-cod-dpz-s90      .
       acc-400.
      *              *-------------------------------------------------*
      *              * Preparazione accettazione                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-cod-dpz-s70    to   v-edm                  .
           move      w-cod-cod-dpz-s90    to   v-ufk                  .
           move      w-cod-cod-dpz-lin    to   v-lin                  .
           move      w-cod-cod-dpz-pos    to   v-pos                  .
           move      w-cod-cod-dpz-s01    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-dpz-ope      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-999.
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-cod-dpz-ope    =    "A+"
                     go to aco-300
           else if   w-cod-cod-dpz-ope    =    "F+"
                     go to aco-200.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Insr                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione accettazione                   *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-cod-dpz-s70    to   v-edm                  .
           move      w-cod-cod-dpz-s90    to   v-ufk                  .
           move      w-cod-cod-dpz-lin    to   v-lin                  .
           move      w-cod-cod-dpz-pos    to   v-pos                  .
           move      w-cod-cod-dpz-s01    to   v-num                  .
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : continuazione           *
      *                  *---------------------------------------------*
           move      "A+"                 to   w-cod-cod-dpz-ope      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Find                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della variabile di i.p.c. di ritor- *
      *                  * no dall'interrogazione, e deviazione in     *
      *                  * funzione dell'esito della lettura           *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     go to aco-220.
       aco-210.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. non esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-100.
       aco-220.
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore selezionato in area di uscita    *
      *                      *-----------------------------------------*
           move      s-num                to   w-cod-cod-dpz-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione del valore in uscita    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-cod-dpz-s70    to   v-edm                  .
           move      w-cod-cod-dpz-lin    to   v-lin                  .
           move      w-cod-cod-dpz-pos    to   v-pos                  .
           move      w-cod-cod-dpz-cod    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione : non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-300.
      *              *-------------------------------------------------*
      *              * Se rientro ne' dopo Insr ne' dopo Find          *
      *              *-------------------------------------------------*
       aco-350.
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-400.
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione valore originale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      w-cod-cod-dpz-s70    to   v-edm                  .
           move      w-cod-cod-dpz-lin    to   v-lin                  .
           move      w-cod-cod-dpz-pos    to   v-pos                  .
           move      w-cod-cod-dpz-s01    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-400.
      *                  *---------------------------------------------*
      *                  * Se impostazione normale                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore impostato in area di uscita      *
      *                      *-----------------------------------------*
           move      v-num                to   w-cod-cod-dpz-cod      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione : non-continuazione     *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-999.
           exit.
