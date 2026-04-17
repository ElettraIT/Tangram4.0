       Identification Division.
       Program-Id.                                 eprzlst0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcp                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/02/95    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Espansione prezzo di listino                *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-xpd-prz-lst-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-xpd-prz-lst-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-xpd-prz-lst-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-xpd-prz-lst-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "XP"  Espansione prezzo di listino                             *
      *                                                                *
      *                                                                *
      *              Input  : w-xpd-prz-lst-ope : "XP"                 *
      *                                                                *
      *                       w-xpd-prz-lst-sqv : Sequenza di visua-   *
      *                                           lizzazione dei prez- *
      *                                           zi. Ogni carattere   *
      *                                           assume il seguente   *
      *                                           significato:         *
      *                                            - C : Concordati    *
      *                                                  per cliente   *
      *                                            - L : Di listino    *
      *                                            - P : Per campagna  *
      *                                                  promozionale  *
      *                                           Ad esempio: PL si-   *
      *                                           gnifica che si ri-   *
      *                                           chiede prima la vi-  *
      *                                           sualizzazione delle  *
      *                                           campagne promoziona- *
      *                                           li, e poi la visua-  *
      *                                           lizzazione dei prez- *
      *                                           zi storici del li-   *
      *                                           stino indicato.      *
      *                                           Se a spaces viene    *
      *                                           interpretato: CLP    *
      *                                                                *
      *                       w-xpd-prz-lst-cli : Codice cliente cui i *
      *                                           prezzi si riferisco- *
      *                                           no, oppure zero se   *
      *                                           non significativo    *
      *                                                                *
      *                       w-xpd-prz-lst-pro : Codice prodotto nu-  *
      *                                           merico               *
      *                                                                *
      *                       w-xpd-prz-lst-lst : Codice listino       *
      *                                                                *
      *                       w-xpd-prz-lst-drp : Data di riferimento  *
      *                                           per il prezzo        *
      *                                                                *
      *                       w-xpd-prz-lst-sns : Si/No ammessa la se- *
      *                                           lezione di un prezzo *
      *                                           tra quelli visualiz- *
      *                                           zati                 *
      *                                           Per ora non gestito  *
      *                                                                *
      *              Output : nessuno                                  *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [lst]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflst"                          .
      *        *-------------------------------------------------------*
      *        * [lsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rflsd"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *    *===========================================================*
      *    * Work-area per l'esecuzione dell'espansione vera e propria *
      *    *-----------------------------------------------------------*
       01  w-exe-esp.
      *        *-------------------------------------------------------*
      *        * Comodo per parametri in input                         *
      *        *-------------------------------------------------------*
           05  w-exe-esp-inp.
      *            *---------------------------------------------------*
      *            * Sequenza di visualizzazione                       *
      *            *---------------------------------------------------*
               10  w-exe-esp-seq-vis      pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-exe-esp-cod-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto, numerico                         *
      *            *---------------------------------------------------*
               10  w-exe-esp-num-pro      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice listino                                    *
      *            *---------------------------------------------------*
               10  w-exe-esp-cod-lst      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Data riferimento per il prezzo                    *
      *            *---------------------------------------------------*
               10  w-exe-esp-ddr-prz      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Si/No ammessa la selezione di un prezzo           *
      *            *---------------------------------------------------*
               10  w-exe-esp-snx-sel      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area libera per espansioni future                 *
      *            *---------------------------------------------------*
               10  w-exe-esp-alx-exp.
                   15  filler  occurs 80  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per elaborazione parametri in input            *
      *        *-------------------------------------------------------*
           05  w-exe-esp-epi.
      *            *---------------------------------------------------*
      *            * Numero di maschere da visualizzare                *
      *            *---------------------------------------------------*
               10  w-exe-esp-num-msk      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Maschere da visualizzare, per ognuna delle quali  *
      *            * vale la seguente codifica:                        *
      *            *  - C : Prezzi netti concordati per cliente        *
      *            *  - L : Prezzi di listino                          *
      *            *  - P : Prezzi di campagna promozionale            *
      *            *---------------------------------------------------*
               10  w-exe-esp-msk-vis      pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Campi di work generici                                *
      *        *-------------------------------------------------------*
           05  w-exe-esp-war.
      *            *---------------------------------------------------*
      *            * Indici                                            *
      *            *---------------------------------------------------*
               10  w-exe-esp-inx-i01      pic  9(02)                  .
               10  w-exe-esp-inx-i02      pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per espansione prezzo di listino                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/eprzlst0.exl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-xpd-prz-lst
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
           if        w-xpd-prz-lst-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-xpd-prz-lst-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-xpd-prz-lst-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-xpd-prz-lst-ope    =    "XP"
                     perform   exp-000    thru exp-999                .
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
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa non e' la prima Open per il modulo si *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    1
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
       opn-300.
      *                  *---------------------------------------------*
      *                  * Open files                                  *
      *                  *---------------------------------------------*
       opn-310.
      *                      *-----------------------------------------*
      *                      * Open file [dcp]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       opn-320.
      *                      *-----------------------------------------*
      *                      * Open file [lst]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       opn-330.
      *                      *-----------------------------------------*
      *                      * Open file [lsd]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       opn-340.
      *                      *-----------------------------------------*
      *                      * Open file [zls]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
       opn-350.
      *                      *-----------------------------------------*
      *                      * Open file [zvl]                         *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
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
      *              *-------------------------------------------------*
      *              * Se questa non e' l'ultima Close per il modulo   *
      *              * si esce                                         *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    zero
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
       cls-300.
      *                  *---------------------------------------------*
      *                  * Close files                                 *
      *                  *---------------------------------------------*
       cls-310.
      *                      *-----------------------------------------*
      *                      * Open file [dcp]                         *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       cls-320.
      *                      *-----------------------------------------*
      *                      * Open file [lst]                         *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflst"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lst                 .
       cls-330.
      *                      *-----------------------------------------*
      *                      * Open file [lsd]                         *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/ioflsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lsd                 .
       cls-340.
      *                      *-----------------------------------------*
      *                      * Open file [zls]                         *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzls"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zls                 .
       cls-350.
      *                      *-----------------------------------------*
      *                      * Open file [zvl]                         *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
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
                     move  spaces         to   w-xpd-prz-lst-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Espansione prezzo di listino                              *
      *    *-----------------------------------------------------------*
       exp-000.
      *              *-------------------------------------------------*
      *              * Parametri in input in area di comodo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sequenza di visualizzazione                 *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-sqv    to   w-exe-esp-seq-vis      .
           if        w-exe-esp-seq-vis    =    spaces
                     move  "CLP   "       to   w-exe-esp-seq-vis      .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-cli    to   w-exe-esp-cod-cli      .
      *                  *---------------------------------------------*
      *                  * Codice prodotto, numerico                   *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-pro    to   w-exe-esp-num-pro      .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-lst    to   w-exe-esp-cod-lst      .
      *                  *---------------------------------------------*
      *                  * Data riferimento per il prezzo              *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-drp    to   w-exe-esp-ddr-prz      .
      *                  *---------------------------------------------*
      *                  * Si/No ammessa la selezione di un prezzo     *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-sns    to   w-exe-esp-snx-sel      .
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni future           *
      *                  *---------------------------------------------*
           move      w-xpd-prz-lst-alx    to   w-exe-esp-alx-exp      .
       exp-100.
      *              *-------------------------------------------------*
      *              * Elaborazione parametri in input                 *
      *              *-------------------------------------------------*
       exp-200.
      *                  *---------------------------------------------*
      *                  * Determinazione, dalla sequenza di visualiz- *
      *                  * zazione, del numero di maschere da visua-   *
      *                  * lizzare e delle maschere da visualizzare    *
      *                  *---------------------------------------------*
       exp-210.
      *                      *-----------------------------------------*
      *                      * Numero maschere da visualizzare a zero  *
      *                      *-----------------------------------------*
           move      zero                 to   w-exe-esp-num-msk      .
      *                      *-----------------------------------------*
      *                      * Maschere da visualizzare a spaces       *
      *                      *-----------------------------------------*
           move      spaces               to   w-exe-esp-msk-vis      .
       exp-220.
      *                      *-----------------------------------------*
      *                      * Indice 1..6 su sequenza di visualizza-  *
      *                      * zione a zero                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-exe-esp-inx-i01      .
       exp-230.
      *                      *-----------------------------------------*
      *                      * Incremento ndice 1..6 su sequenza di    *
      *                      * visualizzazione                         *
      *                      *-----------------------------------------*
           add       1                    to   w-exe-esp-inx-i01      .
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo : fine determina-   *
      *                      * zione sequenza di visualizzazione       *
      *                      *-----------------------------------------*
           if        w-exe-esp-inx-i01    >    6
                     go to exp-280.
       exp-240.
      *                      *-----------------------------------------*
      *                      * Se tipo maschera non riconoscuto        *
      *                      *-----------------------------------------*
       exp-241.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-exe-esp-seq-vis
                    (w-exe-esp-inx-i01 : 1)
                                          =    "C" or
                     w-exe-esp-seq-vis
                    (w-exe-esp-inx-i01 : 1)
                                          =    "L" or
                     w-exe-esp-seq-vis
                    (w-exe-esp-inx-i01 : 1)
                                          =    "P"
                     go to exp-250.
       exp-242.
      *                          *-------------------------------------*
      *                          * Si ignora e si passa ad esaminare   *
      *                          * l'elemento successivo               *
      *                          *-------------------------------------*
           go to     exp-230.
       exp-250.
      *                      *-----------------------------------------*
      *                      * Se tipo maschera doppio                 *
      *                      *-----------------------------------------*
       exp-251.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        w-exe-esp-inx-i01    =    1
                     go to exp-260.
           move      zero                 to   w-exe-esp-inx-i02      .
       exp-252.
           add       1                    to   w-exe-esp-inx-i02      .
           if        w-exe-esp-inx-i02    =    w-exe-esp-inx-i01
                     go to exp-260.
           if        w-exe-esp-seq-vis
                    (w-exe-esp-inx-i02 : 1)
                                          =    w-exe-esp-seq-vis
                                              (w-exe-esp-inx-i01 : 1)
                     go to exp-253
           else      go to exp-252.
       exp-253.
      *                          *-------------------------------------*
      *                          * Si ignora e si passa ad esaminare   *
      *                          * l'elemento successivo               *
      *                          *-------------------------------------*
           go to     exp-230.
       exp-260.
      *                      *-----------------------------------------*
      *                      * Incremento numero maschere da visualiz- *
      *                      * zare                                    *
      *                      *-----------------------------------------*
           add       1                    to   w-exe-esp-num-msk      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione maschera da visualizzare *
      *                      *-----------------------------------------*
           move      w-exe-esp-seq-vis
                    (w-exe-esp-inx-i01 : 1)
                                          to   w-exe-esp-msk-vis
                                              (w-exe-esp-num-msk : 1) .
       exp-270.
      *                      *-----------------------------------------*
      *                      * Riciclo ad elemento successivo          *
      *                      *-----------------------------------------*
           go to     exp-230.
       exp-280.
      *                      *-----------------------------------------*
      *                      * Se zero maschere da visualizzare : u-   *
      *                      * scita senza alcuna azione               *
      *                      *-----------------------------------------*
           if        w-exe-esp-num-msk    =    zero
                     go to exp-900.
       exp-300.
      *                  *---------------------------------------------*
      *                  * _qui_                                       *
      *                  *---------------------------------------------*




       exp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exp-999.
       exp-999.
           exit.

