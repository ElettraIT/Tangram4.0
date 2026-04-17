       Identification Division.
       Program-Id.                                 pxpg0000           .
      *================================================================*
      *                                                                *
      * Programma supervisore per esecuzione in foreground             *
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
      *    * Area per parametri di 'chaining' dal chiamante            *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wenvxpgp.cpy"                   .

      *    *===========================================================*
      *    * Area di comunicazione per programmi della serie "pxpg"    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpglink.cpy"                   .

      *    *===========================================================*
      *    * Work-area per trattamento esecuzione desk-accessory       *
      *    *-----------------------------------------------------------*
       01  w-exe-dac.
      *        *-------------------------------------------------------*
      *        * Si/No in esecuzione di desk-accessory                 *
      *        *  - N : No                                             *
      *        *  - S : Si                                             *
      *        *-------------------------------------------------------*
           05  w-exe-dac-snx-dac          pic  x(01)                  .

      ******************************************************************
       Procedure Division             chaining w-env-v-dkb-home
                                               w-env-v-dkb-user
                                               w-env-v-dkb-ttyc
                                               w-env-v-dkb-term
                                               w-env-v-dkb-runt
                                               w-env-v-dkb-getp
                                               w-env-v-dkb-subt
                                               w-env-v-dkb-azie
                                               w-env-v-dkb-uten
                                               w-env-v-dkb-prms
                                               w-env-v-dkb-post       .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Raccolta variabili di chaining                  *
      *              *-------------------------------------------------*
           move      w-env-v-dkb-home     to   x-env-home             .
           move      w-env-v-dkb-user     to   x-env-user             .
           move      w-env-v-dkb-ttyc     to   x-env-ttyc             .
           move      w-env-v-dkb-term     to   x-env-term             .
           move      w-env-v-dkb-runt     to   x-env-runt             .
           move      spaces               to   x-env-pfix
                                               x-env-hoid             .
           unstring  w-env-v-dkb-getp
                                delimited by   all   spaces
                                          into x-env-pfix
                                               x-env-hoid             .
           move      w-env-v-dkb-subt     to   x-env-subt             .
           move      w-env-v-dkb-azie     to   x-env-azie             .
           move      w-env-v-dkb-uten     to   x-env-uten             .
           move      w-env-v-dkb-prms     to   x-env-prms             .
           move      w-env-v-dkb-post     to   x-env-post             .
      *              *-------------------------------------------------*
      *              * Inizializzazione nome menu' principale          *
      *              *-------------------------------------------------*
           move      spaces               to   x-mnu                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero pagina di menu'         *
      *              *-------------------------------------------------*
           move      zero                 to   x-npm                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione max numero utenti              *
      *              *-------------------------------------------------*
           move      zero                 to   x-nut                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione nr. progressivo utente in uso  *
      *              *-------------------------------------------------*
           move      zero                 to   x-npu                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione per foreground                 *
      *              *                                                 *
      *              * Nota : Viene modificato il valore della varia-  *
      *              *        bile 'x-env-pfix'                        *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg0001"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg0001"                       .
      *              *-------------------------------------------------*
      *              * Ricomposizione di 'w-env-v-dkb-getp' secondo    *
      *              * la variazione di 'x-env-pfix'                   *
      *              *-------------------------------------------------*
           string    x-env-pfix
                                delimited by   size
                     x-env-hoid
                                delimited by   size
                                          into w-env-v-dkb-getp       .
      *              *-------------------------------------------------*
      *              * Se lo status di uscita indica errore grave :    *
      *              * si esce immediatamente                          *
      *              *-------------------------------------------------*
           if        x-sts                =    "##"
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Se lo status di uscita indica che si e' in ese- *
      *              * cuzione di un desk-accessory si memorizza il    *
      *              * segnale corrispondente                          *
      *              *-------------------------------------------------*
           if        x-sts                not  < "01" and
                     x-sts                not  > "99"
                     move  "S"            to   w-exe-dac-snx-dac
           else      move  "N"            to   w-exe-dac-snx-dac      .
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory si  *
      *              * omette il richiamo del sottoprogramma di pre-   *
      *              * presentazione del primo menu'. Cio' in quanto   *
      *              * il sottoprogramma esegue solamente la visualiz- *
      *              * zazione dei print-files in attesa di stampa.    *
      *              *-------------------------------------------------*
           if        w-exe-dac-snx-dac    =    "S"
                     go to main-100.
      *              *-------------------------------------------------*
      *              * Pre-presentazione primo menu'                   *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg0002"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg0002"                       .
           if        x-sts                not  = spaces
                     go to main-900.
       main-100.
      *              *-------------------------------------------------*
      *              * Presentazione menu'                             *
      *              *-------------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg0003"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg0003"                       .
      *              *-------------------------------------------------*
      *              * Controllo uscita da menu'                       *
      *              *-------------------------------------------------*
           if        x-sts                =    "T " or
                     x-sts                =    "X "
                     go to main-200
           else if   x-sts                =    "P "
                     go to main-300
           else if   x-sts                =    "##"
                     go to main-900
           else      go to main-100.
       main-200.
      *              *-------------------------------------------------*
      *              * Da menu' selezionato un task                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del task                           *
      *                  *---------------------------------------------*
           call      x-pat                                            .
      *                  *---------------------------------------------*
      *                  * Cancel del task                             *
      *                  *---------------------------------------------*
           cancel    x-pat                                            .
      *                  *---------------------------------------------*
      *                  * Se si e' in esecuzione di un desk-accessory *
      *                  * si va' a fine lavoro                        *
      *                  *---------------------------------------------*
           if        w-exe-dac-snx-dac    =    "S"
                     go to main-900.
      *                  *---------------------------------------------*
      *                  * Se il task era associato all'utente si va'  *
      *                  * a fine lavoro                               *
      *                  *---------------------------------------------*
           if        x-sts                =    "X "
                     go to main-900.
      *                  *---------------------------------------------*
      *                  * Altrimenti si ricicla alla presentazione    *
      *                  * del menu'                                   *
      *                  *---------------------------------------------*
           go to     main-100.
       main-300.
      *              *-------------------------------------------------*
      *              * Da menu' selezionata una procedura              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riciclo a presentazione menu'               *
      *                  *---------------------------------------------*
           go to     main-100.
       main-900.
      *              *-------------------------------------------------*
      *              * Da menu' selezionato lo stop                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fine lavoro per foreground                  *
      *                  *---------------------------------------------*
           call      "swd/xpg/prg/obj/pxpg0009"
                                         using x                      .
           cancel    "swd/xpg/prg/obj/pxpg0009"                       .
       main-950.
      *              *-------------------------------------------------*
      *              * Post-esecuzione foreground                      *
      *              *-------------------------------------------------*
           chain     "swd/xpg/prg/obj/pxpg0010"
                                         using w-env-v-dkb-home
                                               w-env-v-dkb-user
                                               w-env-v-dkb-ttyc
                                               w-env-v-dkb-term
                                               w-env-v-dkb-runt
                                               w-env-v-dkb-getp
                                               w-env-v-dkb-subt
                                               w-env-v-dkb-azie
                                               w-env-v-dkb-uten
                                               w-env-v-dkb-prms
                                               w-env-v-dkb-post       .
