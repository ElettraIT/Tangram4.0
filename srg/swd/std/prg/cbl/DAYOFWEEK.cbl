      *    *===========================================================*
      *    * Per determinazione del giorno della settimana e primo     *
      *    * giorno non festivo                                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdowlit0.cpw"                   .




      *    *===========================================================*
      *    * Per determinazione del giorno della settimana             *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cpw"                   .




      *    *===========================================================*
      *    * Per determinazione del primo giorno non festivo           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgnflit0.cpw"                   .






      *                  *---------------------------------------------*
      *                  * Determinazione literal giorno della setti-  *
      *                  * mana                                        *
      *                  *---------------------------------------------*
           move      w-tes-dat-___ (1)    to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
           move      w-det-dow-lit        to   w-tes-dat-___-dow (1)  .


              ___________ facoltativo __________



      *                  *---------------------------------------------*
      *                  * Determinazione del primo giorno non festivo *
      *                  *---------------------------------------------*
           move      w-tes-dat-___ (1)    to   w-det-dow-fst-dat      .

              ___________ facoltativi __________

           move      _                    to   w-det-dow-fst-1gf      .
           move      _                    to   w-det-dow-fst-2gf      .

           perform   det-dow-fst-000      thru det-dow-fst-999        .
           move      w-det-dow-fst-dnf    to   w-tes-dat-___ (1)      .





      *    *===========================================================*
      *    * Routine di determinazione del giorno della settimana      *
      *    *-----------------------------------------------------------*
      *    * In input  : w-det-dow-dat = Data completa                 *
      *    *                                                           *
      *    * In output : w-det-dow-lit = Giorno della settimana, lite- *
      *    *                             ral                           *
      *    *                                                           *
      *    *             w-det-dow-num = Giorno della settimana, nu-   *
      *    *                             mero secondo la tabella :     *
      *    *                                                           *
      *    *                             -  1 : Lunedi'                *
      *    *                             -  2 : Martedi'               *
      *    *                             -  3 : Mercoledi'             *
      *    *                             -  4 : Giovedi'               *
      *    *                             -  5 : Venerdi'               *
      *    *                             -  6 : Sabato                 *
      *    *                             -  7 : Domenica               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di determinazione del primo giorno non festivo    *
      *    * rispetto ad una data. Si presume che sia stato prece-     *
      *    * dentemente determinato il giorno della settimana della    *
      *    * data in input                                             *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * In input  : w-det-dow-fst-dat = data completa             *
      *    *             w-det-dow-fst-num = giorno della settimana    *
      *    *                                                           *
      *    * In output : w-det-dow-fst-dnf = Data non festiva          *
      *    *                                                           *
      *    *       N.B.: Vengono considerati festivi i giorni :        *
      *    *                                                           *
      *    *             - sabato       (6)  (w-det-dow-fst-1gf)       *
      *    *             - domenica     (7)  (w-det-dow-fst-2gf)       *
      *    *                                                           *
      *    *             salvo diversa indicazione                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdowlit0.cps"                   .




      *    *===========================================================*
      *    * Routine di determinazione del giorno della settimana      *
      *    *-----------------------------------------------------------*
      *    * In input  : w-det-dow-dat = Data completa                 *
      *    *                                                           *
      *    * In output : w-det-dow-lit = Giorno della settimana, lite- *
      *    *                             ral                           *
      *    *                                                           *
      *    *             w-det-dow-num = Giorno della settimana, nu-   *
      *    *                             mero secondo la tabella :     *
      *    *                                                           *
      *    *                             -  1 : Lunedi'                *
      *    *                             -  2 : Martedi'               *
      *    *                             -  3 : Mercoledi'             *
      *    *                             -  4 : Giovedi'               *
      *    *                             -  5 : Venerdi'               *
      *    *                             -  6 : Sabato                 *
      *    *                             -  7 : Domenica               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cps"                   .




      *    *===========================================================*
      *    * Routine di determinazione del primo giorno non festivo    *
      *    * rispetto ad una data. Si presume che sia stato prece-     *
      *    * dentemente determinato il giorno della settimana della    *
      *    * data in input                                             *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * In input  : w-det-dow-fst-dat = data completa             *
      *    *             w-det-dow-fst-num = giorno della settimana    *
      *    *                                                           *
      *    * In output : w-det-dow-fst-dnf = Data non festiva          *
      *    *                                                           *
      *    *       N.B.: Vengono considerati festivi i giorni :        *
      *    *                                                           *
      *    *             - sabato       (6)  (w-det-dow-fst-1gf)       *
      *    *             - domenica     (7)  (w-det-dow-fst-2gf)       *
      *    *                                                           *
      *    *             salvo diversa indicazione                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgnflit0.cps"                   .

