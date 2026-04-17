      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *                                                           *
      *    * MEMO: si potrebbe implementare il tipo 'v-tip' per poter  *
      *    *       scegliere se l'accettazione del carattere e' libera *
      *    *       o deve corrispondere al carattere passato in        *
      *    *       'v-sgn'                                             *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Box standard per messaggio di errore            *
      *              *-------------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      w-err-box-err-msg    to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Box standard per messaggio di errore            *
      *              *-------------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      w-err-box-err-msg    to   v-nt1                  .
           move      w-err-box-err-m02    to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

