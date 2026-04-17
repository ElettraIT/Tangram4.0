       Identification Division.
       Program-Id.                                 mnotsvb0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/92    *
      *                       Ultima revisione:    NdK del 05/02/01    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la determinazione del valore da  *
      *                    esporre nel campo 'Note' delle statistiche  *
      *                    di vendita sul fatturato.                   *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Attenzione : I records rf-fit e rf-fir in input hanno gia' su- *
      *              bito l'inversione, quando e' presente il codice   *
      *              cliente statistico, del cliente statistico e del  *
      *              cliente commerciale.                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-not-svb-ope : "OP"                 *
      *                                                                *
      *                       w-mod-not-svb-fas : Sigla fase gestiona- *
      *                                           le del programma che *
      *                                           invoca il modulo     *
      *                                                                *
      *                       w-mod-not-svb-war : Copia di lavoro del- *
      *                                           l'area di richieste  *
      *                                           del programma chia-  *
      *                                           mante                *
      *                                                                *
      *                       w-mod-not-svb-npr : Numero periodi di    *
      *                                           riferimento per la   *
      *                                           statistica           *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-not-svb-ope : "CL"                 *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "VN"  Determinazione voce 'Note' da esporre in riga            *
      *                                                                *
      *              Input  : w-mod-not-svb-ope : "VN"                 *
      *                                                                *
      *                       w-mod-not-svb-tdd : Tipo di determina-   *
      *                                           zione, che varia da  *
      *                                           una fase gestionale  *
      *                                           all'altra            *
      *                                                                *
      *                       w-mod-not-svb-ncd : Numero di caratteri  *
      *                                           disponibili per il   *
      *                                           valore del campo di  *
      *                                           'Note' da determina- *
      *                                           re                   *
      *                                                                *
      *                       w-mod-not-svb-q01 : 1. quantita'         *
      *                       w-mod-not-svb-f01 : 1. fatturato         *
      *                       w-mod-not-svb-q02 : 2. quantita'         *
      *                       w-mod-not-svb-f02 : 2. fatturato         *
      *                       w-mod-not-svb-p01 : 1. % di variazione   *
      *                       w-mod-not-svb-q03 : 3. quantita'         *
      *                       w-mod-not-svb-f03 : 3. fatturato         *
      *                       w-mod-not-svb-p02 : 2. % di variazione   *
      *                       w-mod-not-svb-tdo : Tipo documento       *
      *                       w-mod-not-svb-ddo : Data documento       *
      *                       w-mod-not-svb-ndo : Nr.  documento       *
      *                       w-mod-not-svb-cli : Codice cliente       *
      *                       w-mod-not-svb-dpc : Dipendenza cliente   *
      *                       w-mod-not-svb-pro : Codice prodotto      *
      *                                                                *
      *                       rf-fit            : Record [fit] da con- *
      *                                           siderare se necessa- *
      *                                           rio                  *
      *                                                                *
      *                       rf-fir            : Record [fir] da con- *
      *                                           siderare se necessa- *
      *                                           rio                  *
      *                                                                *
      *                                                                *
      *              Output : w-mod-not-svb-not : Valore determinato   *
      *                                           del campo 'Note'     *
      *                                           da esporre           *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CP"  Codice del prodotto da esporre                           *
      *                                                                *
      *              Input  : w-mod-not-svb-ope : "CP"                 *
      *                                                                *
      *                       w-mod-not-svb-cpe : tipo di codice da    *
      *                                           esporre              *
      *                                                                *
      *                       w-mod-not-svb-pro : codice numerico del  *
      *                                           prodotto da stampare *
      *                                                                *
      *                       w-mod-not-svb-alf : codice alfanumerico  *
      *                                           prodotto da stampare *
      *                                                                *
      *                                                                *
      *              Output : w-mod-not-svb-alf : Valore determinato   *
      *                                           del campo 'Note'     *
      *                                           da esporre           *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-not-svb.
      *        *-------------------------------------------------------*
      *        * Valore del campo 'Note' da ritornare al chiamante     *
      *        *-------------------------------------------------------*
           05  w-aux-not-svb-not          pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Comodi                                                *
      *        *-------------------------------------------------------*
           05  w-aux-not-svb-wk1          pic  9(09)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'mnotsvb0' *
      *    *-----------------------------------------------------------*
           copy      "ele/svf/prg/cpy/mnotsvb0.mdl"                   .

      *    *===========================================================*
      *    * Record file [bit]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .

      *    *===========================================================*
      *    * Record file [bir]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .

      ******************************************************************
       Procedure Division                using w-mod-not-svb
                                               rf-bit
                                               rf-bir                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-not-svb-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-not-svb-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-not-svb-ope    =    "VN"
                     perform   dvn-000    thru dvn-999
           else if   w-mod-not-svb-ope    =    "CP"
                     perform   cpe-000    thru cpe-999                .
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
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * [aaf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore della voce 'Note' da esporre        *
      *    *                                                           *
      *    *                                                           *
      *    *   Dati a disposizione dalla funzione 'OP'                 *
      *    *                                                           *
      *    * - w-mod-not-svb-fas : Sigla fase gestionale del programma *
      *    *                       chiamante                           *
      *    *                                                           *
      *    * - w-mod-not-svb-war : Copia di lavoro dell'area richieste *
      *    *                       del programma chiamante             *
      *    *                                                           *
      *    * - w-mod-not-svb-npr : Numero periodi di riferimento per   *
      *    *                       la statistica                       *
      *    *                                                           *
      *    *                                                           *
      *    *   Dati a disposizione dalla funzione 'VN'                 *
      *    *                                                           *
      *    * - w-mod-not-svb-tdd : Tipo di determinazione che deve es- *
      *    *                       sere eseguita, che varia per ogni   *
      *    *                       fase gestionale chiamante           *
      *    *                                                           *
      *    * - w-mod-not-svb-ncd : Numero di caratteri disponibili per *
      *    *                       il valore del campo 'Note'          *
      *    *                                                           *
      *    * - w-mod-not-svb-q01 : 1. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-svb-f01 : 1. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-svb-q02 : 2. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-svb-f02 : 2. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-svb-p01 : 1. % di variazione                  *
      *    *                                                           *
      *    * - w-mod-not-svb-q03 : 3. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-svb-f03 : 3. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-svb-p02 : 2. % di variazione                  *
      *    *                                                           *
      *    * - w-mod-not-svb-tdo : Tipo documento                      *
      *    *                                                           *
      *    * - w-mod-not-svb-ddo : Data documento                      *
      *    *                                                           *
      *    * - w-mod-not-svb-ndo : Nr.  documento                      *
      *    *                                                           *
      *    * - rf-fit            : Record [fit]                        *
      *    *                                                           *
      *    * - rf-fir            : Record [fir]                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dvn-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
       dvn-010.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare del valore del cam- *
      *              * po 'Note' da ritornare al chiamante             *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-not-svb-not      .
       dvn-100.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine specifica a seconda   *
      *              * della fase gestionale chiamante                 *
      *              *-------------------------------------------------*
           if        w-mod-not-svb-fas    =    "svf200"
                     perform   dvn-svf-200-000
                                          thru dvn-svf-200-999
           else if   w-mod-not-svb-fas    =    "svf250"
                     perform   dvn-svf-250-000
                                          thru dvn-svf-250-999
           else if   w-mod-not-svb-fas    =    "svf260"
                     perform   dvn-svf-260-000
                                          thru dvn-svf-260-999
           else if   w-mod-not-svb-fas    =    "svf300"
                     perform   dvn-svf-300-000
                                          thru dvn-svf-300-999
           else if   w-mod-not-svb-fas    =    "svf350"
                     perform   dvn-svf-350-000
                                          thru dvn-svf-350-999
           else if   w-mod-not-svb-fas    =    "svf360"
                     perform   dvn-svf-360-000
                                          thru dvn-svf-360-999
           else if   w-mod-not-svb-fas    =    "svf400"
                     perform   dvn-svf-400-000
                                          thru dvn-svf-400-999
           else if   w-mod-not-svb-fas    =    "svf450"
                     perform   dvn-svf-450-000
                                          thru dvn-svf-450-999
           else if   w-mod-not-svb-fas    =    "svf460"
                     perform   dvn-svf-460-000
                                          thru dvn-svf-460-999
           else if   w-mod-not-svb-fas    =    "svf500"
                     perform   dvn-svf-500-000
                                          thru dvn-svf-500-999
           else if   w-mod-not-svb-fas    =    "svf502"
                     perform   dvn-svf-502-000
                                          thru dvn-svf-502-999
           else if   w-mod-not-svb-fas    =    "svf520"
                     perform   dvn-svf-520-000
                                          thru dvn-svf-520-999
           else if   w-mod-not-svb-fas    =    "svf530"
                     perform   dvn-svf-530-000
                                          thru dvn-svf-530-999
           else if   w-mod-not-svb-fas    =    "svf540"
                     perform   dvn-svf-540-000
                                          thru dvn-svf-540-999
           else if   w-mod-not-svb-fas    =    "svf570"
                     perform   dvn-svf-570-000
                                          thru dvn-svf-570-999
           else if   w-mod-not-svb-fas    =    "svf600"
                     perform   dvn-svf-600-000
                                          thru dvn-svf-600-999
           else if   w-mod-not-svb-fas    =    "svf620"
                     perform   dvn-svf-620-000
                                          thru dvn-svf-620-999
           else if   w-mod-not-svb-fas    =    "svf630"
                     perform   dvn-svf-630-000
                                          thru dvn-svf-630-999
           else if   w-mod-not-svb-fas    =    "svf640"
                     perform   dvn-svf-640-000
                                          thru dvn-svf-640-999
           else if   w-mod-not-svb-fas    =    "svf670"
                     perform   dvn-svf-670-000
                                          thru dvn-svf-670-999
           else if   w-mod-not-svb-fas    =    "svf700"
                     perform   dvn-svf-700-000
                                          thru dvn-svf-700-999
           else if   w-mod-not-svb-fas    =    "svf720"
                     perform   dvn-svf-720-000
                                          thru dvn-svf-720-999
           else if   w-mod-not-svb-fas    =    "svf725"
                     perform   dvn-svf-725-000
                                          thru dvn-svf-725-999
           else if   w-mod-not-svb-fas    =    "svf730"
                     perform   dvn-svf-730-000
                                          thru dvn-svf-730-999
           else if   w-mod-not-svb-fas    =    "svf740"
                     perform   dvn-svf-740-000
                                          thru dvn-svf-740-999
           else if   w-mod-not-svb-fas    =    "svf750"
                     perform   dvn-svf-750-000
                                          thru dvn-svf-750-999
           else if   w-mod-not-svb-fas    =    "svf760"
                     perform   dvn-svf-760-000
                                          thru dvn-svf-760-999
           else      perform   dvn-svf-xyz-000
                                          thru dvn-svf-xyz-999        .
       dvn-900.
      *              *-------------------------------------------------*
      *              * Valore del campo 'Note' determinato in ritorno  *
      *              * al chiamante                                    *
      *              *-------------------------------------------------*
           move      w-aux-not-svb-not    to   w-mod-not-svb-not      .
       dvn-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf200'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-200-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-200-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf250'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-250-000.
      *              *-------------------------------------------------*
      *              * Test su tipo determinazione                     *
      *              *-------------------------------------------------*
           if        w-mod-not-svb-tdd    not  = 00
                     go to dvn-svf-250-800.
       dvn-svf-250-100.
      *              *-------------------------------------------------*
      *              * Note per il dettaglio                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione valore unitario              *
      *                  *---------------------------------------------*
           divide    w-mod-not-svb-q01    into w-mod-not-svb-f01
                                        giving w-aux-not-svb-wk1      .
      *                  *---------------------------------------------*
      *                  * Editing valore unitario                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BG"                to   p-edm                  .
           move      w-aux-not-svb-wk1    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore unitario editato in campo note       *
      *                  *---------------------------------------------*
           move      p-edt                to   w-aux-not-svb-not      .
       dvn-svf-250-190.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     dvn-svf-250-900.
       dvn-svf-250-800.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-250-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dvn-svf-250-999.
       dvn-svf-250-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf260'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-260-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-260-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf300'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-300-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-300-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf350'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-350-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-350-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf360'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-360-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-360-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf400'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-400-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-400-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf450'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-450-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-450-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf460'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-460-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-460-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf500'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-500-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-500-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf502'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-502-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-502-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf520'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-520-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-520-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf530'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-530-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-530-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf540'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-540-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-540-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf570'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-570-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-570-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf600'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-600-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-600-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf620'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-620-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-620-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf630'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-630-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-630-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf640'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-640-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-640-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf670'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-670-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-670-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf700'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-700-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-700-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf720'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-720-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-720-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf725'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-725-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-725-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf730'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-730-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-730-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf740'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-740-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-740-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf750'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-750-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-750-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'svf760'                              *
      *    *-----------------------------------------------------------*
       dvn-svf-760-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-760-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase non riconosciuta dal modulo           *
      *    *-----------------------------------------------------------*
       dvn-svf-xyz-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-svf-gen-000      thru dvn-svf-gen-999        .
       dvn-svf-xyz-999.
           exit.

      *    *===========================================================*
      *    * Subroutine generica                                       *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    * Output : - w-aux-not-svb-not = Valore del campo 'Note'    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dvn-svf-gen-000.
       dvn-svf-gen-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codice del prodotto da esporre             *
      *    *-----------------------------------------------------------*
       cpe-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo determinazione  *
      *              *-------------------------------------------------*
           if        w-mod-not-svb-cpe    =    01
                     go to cpe-100
           else if   w-mod-not-svb-cpe    =    02
                     go to cpe-300
           else if   w-mod-not-svb-cpe    =    03
                     go to cpe-500
           else      go to cpe-100.
       cpe-100.
      *              *-------------------------------------------------*
      *              * Codice alfanumerico anagrafico del prodotto     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si esce con il codice in input              *
      *                  *---------------------------------------------*
           go to     cpe-900.
       cpe-300.
      *              *-------------------------------------------------*
      *              * Codice prodotto assegnato da casa produttrice   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [aaq]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Lettura [aaq]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      w-mod-not-svb-pro    to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per la casa produttrice     *
      *                  *---------------------------------------------*
           if        rf-aaq-cdp-pdt       not  = spaces
                     move  rf-aaq-cdp-pdt to   w-mod-not-svb-alf      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     cpe-900.
       cpe-500.
      *              *-------------------------------------------------*
      *              * Codice prodotto assegnato dal fornitore prefer. *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [aaq]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Lettura [aaq]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      w-mod-not-svb-pro    to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Codice fornitore preferenziale              *
      *                  *---------------------------------------------*
           move      rf-aaq-dcf-pfz       to   w-aux-not-svb-wk1      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione [aaf]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Lettura [aaf]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM    "         to   f-key                  .
           move      01                   to   rf-aaf-tip-mag         .
           move      w-mod-not-svb-pro    to   rf-aaf-num-pro         .
           move      w-aux-not-svb-wk1    to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Codice prodotto per il fornitore            *
      *                  *---------------------------------------------*
           if        rf-aaf-cop-sfn       not  = spaces
                     move  rf-aaf-cop-sfn to   w-mod-not-svb-alf      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     cpe-900.
       cpe-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cpe-999.
       cpe-999.
           exit.

