      *    *===========================================================*
      *    * Subroutine per editing voce 'Fatturato'                   *
      *    *-----------------------------------------------------------*
       stp-edt-fat-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : uscita con zero editato      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-stp-edt-fat-fat    not  = zero
                     go to stp-edt-fat-200.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dei decimali valuta  *
      *                  * base                                        *
      *                  *---------------------------------------------*
           if        c-dec                =    0
                     move  "               0"
                                          to   w-stp-edt-fat-edt
           else if   c-dec                =    1
                     move  "             0,0"
                                          to   w-stp-edt-fat-edt
           else if   c-dec                =    2
                     move  "            0,00"
                                          to   w-stp-edt-fat-edt
           else      move  "               0"
                                          to   w-stp-edt-fat-edt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-edt-fat-999.
       stp-edt-fat-200.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-stp-edt-fat-fat    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Valore editato in campo di destinazione         *
      *              *-------------------------------------------------*
           move      p-edt                to   w-stp-edt-fat-edt      .
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           move      "AA"                 to   p-ope                  .
           move      16                   to   p-car                  .
           move      "D"                  to   p-edm                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-alf                to   w-stp-edt-fat-edt      .
       stp-edt-fat-999.
           exit.

