       Identification Division.
       Program-Id.                                 mpioer             .
      *================================================================*
      *                                                                *
      * Modulo per il trattamento degli errori di i-o per il sottosi-  *
      * sistema di stampa.                                             *
      *                                                                *
      * - Errori su file di stampa vero e proprio ......: [stp]        *
      * - Errori su file di catalogo dei print-files ...: [pfc]        *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Utilizza come link-area l'area di interfaccia stampa : p       *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * Parametri in ingresso :                                        *
      *                                                                *
      *  - p-nam : file name                                           *
      *            - stp = file di stampa vero e proprio               *
      *            - pfc = file di catalogo dei print-files            *
      *                                                                *
      *  - p-pat : file pathname                                       *
      *                                                                *
      *  - p-sts : file status                                         *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "miosts"  *
      *    *-----------------------------------------------------------*
       01  c.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  c-nam                      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  c-pat                      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  c-sts                      pic  x(02)                  .

      *    *===========================================================*
      *    * Work per determinazione se foreground o background        *
      *    *-----------------------------------------------------------*
       01  w-frg-bkg.
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento                                    *
      *        * - F : Foreground                                      *
      *        * - B : Background                                      *
      *        *-------------------------------------------------------*
           05  w-frg-bkg-fun              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per estensione al codice di errore                   *
      *    *-----------------------------------------------------------*
       01  w-ext-err.
      *        *-------------------------------------------------------*
      *        * Estensione al codice di errore                        *
      *        *-------------------------------------------------------*
           05  w-ext-err-ext              pic  x(02)                  .

      *    *===========================================================*
      *    * Work per visualizzazione errore in foreground             *
      *    *-----------------------------------------------------------*
       01  w-err-frg.
      *        *-------------------------------------------------------*
      *        * Comodo per risposta data dall'operatore               *
      *        *-------------------------------------------------------*
           05  w-err-frg-acc              pic  x(02)                  .

      *    *===========================================================*
      *    * Work per visualizzazione errore in background             *
      *    *-----------------------------------------------------------*
       01  w-err-bkg.
      *        *-------------------------------------------------------*
      *        * Comodo per codice azienda                             *
      *        *-------------------------------------------------------*
           05  w-err-bkg-azi              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice terminale                           *
      *        *-------------------------------------------------------*
           05  w-err-bkg-ter              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice utente                              *
      *        *-------------------------------------------------------*
           05  w-err-bkg-ute              pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per sistema applicativo                        *
      *        *-------------------------------------------------------*
           05  w-err-bkg-sap              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per area gestionale                            *
      *        *-------------------------------------------------------*
           05  w-err-bkg-arg              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per settore gestionale                         *
      *        *-------------------------------------------------------*
           05  w-err-bkg-set              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per fase gestionale                            *
      *        *-------------------------------------------------------*
           05  w-err-bkg-fas              pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per programma                                  *
      *        *-------------------------------------------------------*
           05  w-err-bkg-pro              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file name                                  *
      *        *-------------------------------------------------------*
           05  w-err-bkg-nam              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file pathname                              *
      *        *-------------------------------------------------------*
           05  w-err-bkg-pat              pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file status                                *
      *        *-------------------------------------------------------*
           05  w-err-bkg-sts              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per string del parametro                       *
      *        *-------------------------------------------------------*
           05  w-err-bkg-cps              pic  x(40)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      ******************************************************************
       Procedure Division                using p                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Determinazione se funzionamento in foreground o *
      *              * in background                                   *
      *              *-------------------------------------------------*
           perform   det-frg-bkg-000      thru det-frg-bkg-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se foreground o background *
      *              *-------------------------------------------------*
           if        w-frg-bkg-fun        =    "F"
                     go to main-200
           else      go to main-400.
       main-200.
      *              *-------------------------------------------------*
      *              * Se funzionamento in foreground                  *
      *              *-------------------------------------------------*
           perform   vis-err-frg-000      thru vis-err-frg-999        .
           go to     main-999.
       main-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento in background                  *
      *              *-------------------------------------------------*
           perform   vis-err-bkg-000      thru vis-err-bkg-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Determinazione se funzionamento foreground o background   *
      *    *-----------------------------------------------------------*
       det-frg-bkg-000.
      *              *-------------------------------------------------*
      *              * Raccolta informazioni generali da segreteria    *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Estrazione tipo funzionamento                   *
      *              *-------------------------------------------------*
           move      s-fun                to   w-frg-bkg-fun          .
           if        w-frg-bkg-fun        not  = "B"
                     move  "F"            to   w-frg-bkg-fun          .
       det-frg-bkg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in foreground     *
      *    *-----------------------------------------------------------*
       vis-err-frg-000.
      *              *-------------------------------------------------*
      *              * Determinazione estensione al codice errore      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'ultimo file che   *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      low-values           to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se file name e pathname differiscono da     *
      *                  * quelli passati in interfaccia p : estensio- *
      *                  * ne a high-values                            *
      *                  *---------------------------------------------*
           if        c-nam                not  = p-nam or
                     c-pat                not  = p-pat
                     move  high-values    to   w-ext-err-ext
                     go to vis-err-frg-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizza il codice di errore *
      *                  * cobol reale                                 *
      *                  *---------------------------------------------*
           move      c-sts                to   p-sts                  .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'estensione al co- *
      *                  * dice di errore relativo all ultimo file che *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      high-values          to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se estensione a high-values : estensione a  *
      *                  * high-values                                 *
      *                  *---------------------------------------------*
           if        c-sts                =    high-values
                     move  high-values    to   w-ext-err-ext
                     go to vis-err-frg-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizza l'estensione al co- *
      *                  * dice di errore cobol reale                  *
      *                  *---------------------------------------------*
           move      c-sts                to   w-ext-err-ext          .
       vis-err-frg-100.
      *              *-------------------------------------------------*
      *              * Save video                                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box vuoto                                       *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      19                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio interno al box                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Errore durante l'accesso all'archivio stampa"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titolo                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "--------------------------------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Nome archivio                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Nome archivio                     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      p-nam                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Identificatore archivio                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Identificatore archivio           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      p-pat                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice di errore                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      35                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice interno di errore          :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      39                   to   v-pos                  .
           move      p-sts                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Estensione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           if        w-ext-err-ext        not  = high-values
                     string "," delimited by   size
                            w-ext-err-ext
                                delimited by   size
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per presa visione                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-200.
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore di default per la risposta           *
      *                  *---------------------------------------------*
           move      spaces               to   w-err-frg-acc          .
       vis-err-frg-300.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-err-frg-acc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio valore impostato                *
      *                  *---------------------------------------------*
           move      v-alf                to   w-err-frg-acc          .
      *                  *---------------------------------------------*
      *                  * Deviazione socondo il tasto funzione        *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to vis-err-frg-400
           else if   v-key                =    "DO  " or
                     v-key                =    "EXIT"
                     go to vis-err-frg-500
           else if   v-key                =    "FIND"
                     go to vis-err-frg-600
           else      go to vis-err-frg-300.
       vis-err-frg-400.
      *                  *---------------------------------------------*
      *                  * Se impostazione con Return                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se valore non Ok : reimpostazione       *
      *                      *-----------------------------------------*
           if        w-err-frg-acc        not  = "OK"
                     go to vis-err-frg-300.
      *                      *-----------------------------------------*
      *                      * Altrimenti : Restore ed uscita          *
      *                      *-----------------------------------------*
           go to     vis-err-frg-900.
       vis-err-frg-500.
      *                  *---------------------------------------------*
      *                  * Se Do o Exit                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Restore ed uscita                       *
      *                      *-----------------------------------------*
           go to     vis-err-frg-900.
       vis-err-frg-600.
      *                  *---------------------------------------------*
      *                  * Se Find                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo di interrogazione   *
      *                      * sui codici di errore                    *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mioerh"                         .
      *                      *-----------------------------------------*
      *                      * Cancellazione del modulo                *
      *                      *-----------------------------------------*
           cancel    "swd/mod/prg/obj/mioerh"                         .
      *                      *-----------------------------------------*
      *                      * Rientro ad accettazione                 *
      *                      *-----------------------------------------*
           go to     vis-err-frg-200.
       vis-err-frg-900.
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-err-frg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione errore se funzionamento in background     *
      *    *-----------------------------------------------------------*
       vis-err-bkg-000.
      *              *-------------------------------------------------*
      *              * Raccolta informazioni in area di comodo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Da segreteria                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiesta informazioni generali al mo-  *
      *                      * dulo di segreteria                      *
      *                      *-----------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   w-err-bkg-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   w-err-bkg-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   w-err-bkg-ute          .
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      s-sap                to   w-err-bkg-sap          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      s-arg                to   w-err-bkg-arg          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      s-set                to   w-err-bkg-set          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      s-fas                to   w-err-bkg-fas          .
      *                      *-----------------------------------------*
      *                      * Programma                               *
      *                      *-----------------------------------------*
           move      s-pro                to   w-err-bkg-pro          .
      *                  *---------------------------------------------*
      *                  * Da area di link                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * File name                               *
      *                      *-----------------------------------------*
           move      p-nam                to   w-err-bkg-nam          .
      *                      *-----------------------------------------*
      *                      * File pathname                           *
      *                      *-----------------------------------------*
           move      p-pat                to   w-err-bkg-pat          .
      *                      *-----------------------------------------*
      *                      * File status                             *
      *                      *-----------------------------------------*
           move      p-sts                to   w-err-bkg-sts          .
       vis-err-bkg-100.
      *              *-------------------------------------------------*
      *              * Determinazione estensione al codice errore      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'ultimo file che   *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      low-values           to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se file name e pathname differiscono da     *
      *                  * quelli passati in interfaccia p : estensio- *
      *                  * ne a high-values                            *
      *                  *---------------------------------------------*
           if        c-nam                not  = w-err-bkg-nam or
                     c-pat                not  = w-err-bkg-pat
                     move  high-values    to   w-ext-err-ext
                     go to vis-err-bkg-200.
      *                  *---------------------------------------------*
      *                  * ALtrimenti si memorizza il codice di errore *
      *                  * cobol reale                                 *
      *                  *---------------------------------------------*
           move      c-sts                to   w-err-bkg-sts          .
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo miosts per ottenere le  *
      *                  * informazioni relative all'estensione al co- *
      *                  * dice di errore relativo all ultimo file che *
      *                  * ha provocato un errore di i-o               *
      *                  *---------------------------------------------*
           move      high-values          to   c-sts                  .
           call      "swd/mod/prg/obj/miosts"
                                         using c                      .
      *                  *---------------------------------------------*
      *                  * Se estensione a high-values : estensione a  *
      *                  * high-values                                 *
      *                  *---------------------------------------------*
           if        c-sts                =    high-values
                     move  high-values    to   w-ext-err-ext
                     go to vis-err-bkg-200.
      *                  *---------------------------------------------*
      *                  * Altrimenti si memorizza l'estensione al co- *
      *                  * dice di errore cobol reale                  *
      *                  *---------------------------------------------*
           move      c-sts                to   w-ext-err-ext          .
       vis-err-bkg-200.
      *              *-------------------------------------------------*
      *              * Emissione messaggio di errore                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "+-------------------------------------------------
      -              "-----------------------------+"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                 Errore durante l'accesso all'arc
      -              "hivio stampa                 |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                                                 
      -              "                             |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 04                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-azi        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Azienda .................... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 05                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-ute        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Utente ..................... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 06                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-ter        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Terminale .................. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 07                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-sap        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Sistema applicativo ........ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 08                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-arg        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Area gestionale ............ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 09                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-set        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Settore gestionale ......... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 10                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-fas        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Fase gestionale ............ : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 11                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-pro        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Programma .................. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 12                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-nam        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Nome archivio .............. : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 13                                    *
      *                  *---------------------------------------------*
           move      w-err-bkg-pat        to   w-err-bkg-cps          .
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Identificatore archivio .... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       vis-err-bkg-800.
      *                  *---------------------------------------------*
      *                  * Linea 14                                    *
      *                  *---------------------------------------------*
           if        w-ext-err-ext        =    spaces
                     move  w-err-bkg-sts  to   w-err-bkg-cps
                     go to vis-err-bkg-850.
           move      spaces               to   w-err-bkg-cps          .
           string    w-err-bkg-sts
                                delimited by   size
                     ","        delimited by   size
                     w-ext-err-ext
                                delimited by   size
                                          into w-err-bkg-cps          .
       vis-err-bkg-850.
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "| Codice interno di errore ... : "
                                delimited by   size
                     w-err-bkg-cps
                                delimited by   size
                     "      |"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 15                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "|                                                 
      -              "                             |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 16                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "| Prendere nota dei valori sopraesposti e rivolger
      -              "si all'assistenza software.  |"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Linea 17                                    *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "+-------------------------------------------------
      -              "-----------------------------+"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       vis-err-bkg-999.
           exit.
