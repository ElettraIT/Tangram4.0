       Identification Division.
       Program-Id.                                 pxpg0800           .
      *================================================================*
      *                                                                *
      * Programma supervisore per esecuzione in background             *
      *                                                                *
      * N.B.: Attualmente non richiamato da nessun programma           *
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
      *    * Area per parametri di 'chaining' dal chiamante            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * Variabile di environment V_DKB_PFIX                   *
      *        *-------------------------------------------------------*
       77  w-env-v-dkb-pfix               pic  x(20)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .
       
      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Work-area                                                 *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Pathname del file immagine della segreteria           *
      *        *-------------------------------------------------------*
           05  w-ims                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma in background                  *
      *        *-------------------------------------------------------*
           05  w-pmo                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Nome della variabile di 'Status Background'           *
      *        *-------------------------------------------------------*
           05  w-vsb                      pic  x(20) value
                     "Status Background   "                           .

      ******************************************************************
       Procedure Division             chaining w-env-v-dkb-pfix       .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Apertura modulo                       "mopsys"  *
      *              *-------------------------------------------------*
           move      "OP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Composizione del pathname del file che contiene *
      *              * l'immagine della segreteria passata dal fore-   *
      *              * ground                                          *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    w-env-v-dkb-pfix
                                delimited by   spaces
                     ".sgr"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-ims                  .
      *              *-------------------------------------------------*
      *              * Open modulo di segreteria in background         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   s-ope                  .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           move      "B"                  to   s-fun                  .
      *                  *---------------------------------------------*
      *                  * Pathname immagine segreteria                *
      *                  *---------------------------------------------*
           move      w-ims                to   s-alf                  .
      *                  *---------------------------------------------*
      *                  * Prefisso o postfisso unico per files tempo- *
      *                  * ranei                                       *
      *                  *---------------------------------------------*
           move      w-env-v-dkb-pfix     to   s-pat                  .
      *                  *---------------------------------------------*
      *                  * Richiamo funzione                           *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se errori : fine lavoro                     *
      *                  *---------------------------------------------*
           if        s-fun                not  = "B"
                     go to main-975.
      *              *-------------------------------------------------*
      *              * Salvataggio pathname programma in background    *
      *              *-------------------------------------------------*
           move      s-pmo                to   w-pmo                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di background *
      *              *-------------------------------------------------*
           move      "OB"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errori : fine lavoro                     *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to main-975.
      *              *-------------------------------------------------*
      *              * Lancio del programma in background              *
      *              *-------------------------------------------------*
           call      w-pmo                                            .
      *              *-------------------------------------------------*
      *              * Cancel del programma in background              *
      *              *-------------------------------------------------*
           cancel    w-pmo                                            .
      *              *-------------------------------------------------*
      *              * Si pone il valore della variabile di 'Status    *
      *              * Background' a 'T'                               *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "T"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Close modulo di segreteria                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Cancel del modulo di segreteria                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/msegrt"                         .
      *              *-------------------------------------------------*
      *              * Cancel del modulo di messaggi                   *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mmessg"                         .
      *              *-------------------------------------------------*
      *              * Chiusura modulo                       "mopsys"  *
      *              *-------------------------------------------------*
           move      "CL"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       main-975.
      *              *-------------------------------------------------*
      *              * Cancellazione di tutti i sottoprogrammi che so- *
      *              * no stati richiamati durante la sessione         *
      *              *-------------------------------------------------*
           cancel    all                                              .
       main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione dello 'Stop Run'                     *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mstopr"                         .
