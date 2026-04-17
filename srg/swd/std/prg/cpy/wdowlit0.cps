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

