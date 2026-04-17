       Identification Division.
       Program-Id.                                 mnotsaf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    saf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 13/05/96    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la determinazione del valore da  *
      *                    esporre nel campo 'Note' delle statistiche  *
      *                    di acquisto sul fatturato.                  *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Attenzione : I records rf-fft e rf-ffr in input hanno gia' su- *
      *              bito l'inversione, quando e' presente il codice   *
      *              fornitore statistico, del fornitore statistico e  *
      *              del fornitore commerciale.                        *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-not-saf-ope : "OP"                 *
      *                                                                *
      *                       w-mod-not-saf-fas : Sigla fase gestiona- *
      *                                           le del programma che *
      *                                           invoca il modulo     *
      *                                                                *
      *                       w-mod-not-saf-war : Copia di lavoro del- *
      *                                           l'area di richieste  *
      *                                           del programma chia-  *
      *                                           mante                *
      *                                                                *
      *                       w-mod-not-saf-npr : Numero periodi di    *
      *                                           riferimento per la   *
      *                                           statistica           *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-not-saf-ope : "CL"                 *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "VN"  Determinazione voce 'Note' da esporre in riga            *
      *                                                                *
      *              Input  : w-mod-not-saf-ope : "VN"                 *
      *                                                                *
      *                       w-mod-not-saf-tdd : Tipo di determina-   *
      *                                           zione, che varia da  *
      *                                           una fase gestionale  *
      *                                           all'altra            *
      *                                                                *
      *                       w-mod-not-saf-ncd : Numero di caratteri  *
      *                                           disponibili per il   *
      *                                           valore del campo di  *
      *                                           'Note' da determina- *
      *                                           re                   *
      *                                                                *
      *                       w-mod-not-saf-q01 : 1. quantita'         *
      *                                                                *
      *                       w-mod-not-saf-f01 : 1. fatturato         *
      *                                                                *
      *                       w-mod-not-saf-q02 : 2. quantita'         *
      *                                                                *
      *                       w-mod-not-saf-f02 : 2. fatturato         *
      *                                                                *
      *                       w-mod-not-saf-p01 : 1. % di variazione   *
      *                                                                *
      *                       w-mod-not-saf-q03 : 3. quantita'         *
      *                                                                *
      *                       w-mod-not-saf-f03 : 3. fatturato         *
      *                                                                *
      *                       w-mod-not-saf-p02 : 2. % di variazione   *
      *                                                                *
      *                       w-mod-not-saf-tdo : Tipo documento       *
      *                                                                *
      *                       w-mod-not-saf-ddo : Data documento       *
      *                                                                *
      *                       w-mod-not-saf-ndo : Nr.  documento       *
      *                                                                *
      *                       rf-fft            : Record [fft] da con- *
      *                                           siderare se necessa- *
      *                                           rio                  *
      *                                                                *
      *                       rf-ffr            : Record [ffr] da con- *
      *                                           siderare se necessa- *
      *                                           rio                  *
      *                                                                *
      *                                                                *
      *              Output : w-mod-not-saf-not : Valore determinato   *
      *                                           del campo 'Note'     *
      *                                           da esporre           *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-not-saf.
      *        *-------------------------------------------------------*
      *        * Valore del campo 'Note' da ritornare al chiamante     *
      *        *-------------------------------------------------------*
           05  w-aux-not-saf-not          pic  x(80)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'saf'           'mnotsaf0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/saf/prg/cpy/mnotsaf0.mdl"                   .

      *    *===========================================================*
      *    * Record file [fft]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .

      *    *===========================================================*
      *    * Record file [ffr]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .

      ******************************************************************
       Procedure Division                using w-mod-not-saf
                                               rf-fft
                                               rf-ffr                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-not-saf-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-not-saf-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-not-saf-ope    =    "VN"
                     perform   dvn-000    thru dvn-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
       cls-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valore della voce 'Note' da esporre        *
      *    *                                                           *
      *    *                                                           *
      *    *   Dati a disposizione dalla funzione 'OP'                 *
      *    *                                                           *
      *    * - w-mod-not-saf-fas : Sigla fase gestionale del programma *
      *    *                       chiamante                           *
      *    *                                                           *
      *    * - w-mod-not-saf-war : Copia di lavoro dell'area richieste *
      *    *                       del programma chiamante             *
      *    *                                                           *
      *    * - w-mod-not-saf-npr : Numero periodi di riferimento per   *
      *    *                       la statistica                       *
      *    *                                                           *
      *    *                                                           *
      *    *   Dati a disposizione dalla funzione 'VN'                 *
      *    *                                                           *
      *    * - w-mod-not-saf-tdd : Tipo di determinazione che deve es- *
      *    *                       sere eseguita, che varia per ogni   *
      *    *                       fase gestionale chiamante           *
      *    *                                                           *
      *    * - w-mod-not-saf-ncd : Numero di caratteri disponibili per *
      *    *                       il valore del campo 'Note'          *
      *    *                                                           *
      *    * - w-mod-not-saf-q01 : 1. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-saf-f01 : 1. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-saf-q02 : 2. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-saf-f02 : 2. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-saf-p01 : 1. % di variazione                  *
      *    *                                                           *
      *    * - w-mod-not-saf-q03 : 3. quantita'                        *
      *    *                                                           *
      *    * - w-mod-not-saf-f03 : 3. fatturato                        *
      *    *                                                           *
      *    * - w-mod-not-saf-p02 : 2. % di variazione                  *
      *    *                                                           *
      *    * - w-mod-not-saf-tdo : Tipo documento                      *
      *    *                                                           *
      *    * - w-mod-not-saf-ddo : Data documento                      *
      *    *                                                           *
      *    * - w-mod-not-saf-ndo : Nr.  documento                      *
      *    *                                                           *
      *    * - rf-fft            : Record [fft]                        *
      *    *                                                           *
      *    * - rf-ffr            : Record [ffr]                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dvn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare del valore del cam- *
      *              * po 'Note' da ritornare al chiamante             *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-not-saf-not      .
       dvn-100.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine specifica a seconda   *
      *              * della fase gestionale chiamante                 *
      *              *-------------------------------------------------*
           if        w-mod-not-saf-fas    =    "saf200"
                     perform   dvn-saf-200-000
                                          thru dvn-saf-200-999
           else if   w-mod-not-saf-fas    =    "saf250"
                     perform   dvn-saf-250-000
                                          thru dvn-saf-250-999
           else if   w-mod-not-saf-fas    =    "saf260"
                     perform   dvn-saf-260-000
                                          thru dvn-saf-260-999
           else if   w-mod-not-saf-fas    =    "saf300"
                     perform   dvn-saf-300-000
                                          thru dvn-saf-300-999
           else if   w-mod-not-saf-fas    =    "saf350"
                     perform   dvn-saf-350-000
                                          thru dvn-saf-350-999
           else if   w-mod-not-saf-fas    =    "saf360"
                     perform   dvn-saf-360-000
                                          thru dvn-saf-360-999
           else if   w-mod-not-saf-fas    =    "saf400"
                     perform   dvn-saf-400-000
                                          thru dvn-saf-400-999
           else if   w-mod-not-saf-fas    =    "saf450"
                     perform   dvn-saf-450-000
                                          thru dvn-saf-450-999
           else if   w-mod-not-saf-fas    =    "saf460"
                     perform   dvn-saf-460-000
                                          thru dvn-saf-460-999
           else if   w-mod-not-saf-fas    =    "saf500"
                     perform   dvn-saf-500-000
                                          thru dvn-saf-500-999
           else if   w-mod-not-saf-fas    =    "saf502"
                     perform   dvn-saf-502-000
                                          thru dvn-saf-502-999
           else if   w-mod-not-saf-fas    =    "saf520"
                     perform   dvn-saf-520-000
                                          thru dvn-saf-520-999
           else if   w-mod-not-saf-fas    =    "saf530"
                     perform   dvn-saf-530-000
                                          thru dvn-saf-530-999
           else if   w-mod-not-saf-fas    =    "saf540"
                     perform   dvn-saf-540-000
                                          thru dvn-saf-540-999
           else if   w-mod-not-saf-fas    =    "saf570"
                     perform   dvn-saf-570-000
                                          thru dvn-saf-570-999
           else if   w-mod-not-saf-fas    =    "saf600"
                     perform   dvn-saf-600-000
                                          thru dvn-saf-600-999
           else if   w-mod-not-saf-fas    =    "saf620"
                     perform   dvn-saf-620-000
                                          thru dvn-saf-620-999
           else if   w-mod-not-saf-fas    =    "saf630"
                     perform   dvn-saf-630-000
                                          thru dvn-saf-630-999
           else if   w-mod-not-saf-fas    =    "saf640"
                     perform   dvn-saf-640-000
                                          thru dvn-saf-640-999
           else if   w-mod-not-saf-fas    =    "saf670"
                     perform   dvn-saf-670-000
                                          thru dvn-saf-670-999
           else if   w-mod-not-saf-fas    =    "saf700"
                     perform   dvn-saf-700-000
                                          thru dvn-saf-700-999
           else if   w-mod-not-saf-fas    =    "saf720"
                     perform   dvn-saf-720-000
                                          thru dvn-saf-720-999
           else if   w-mod-not-saf-fas    =    "saf725"
                     perform   dvn-saf-725-000
                                          thru dvn-saf-725-999
           else if   w-mod-not-saf-fas    =    "saf730"
                     perform   dvn-saf-730-000
                                          thru dvn-saf-730-999
           else if   w-mod-not-saf-fas    =    "saf740"
                     perform   dvn-saf-740-000
                                          thru dvn-saf-740-999
           else if   w-mod-not-saf-fas    =    "saf750"
                     perform   dvn-saf-750-000
                                          thru dvn-saf-750-999
           else if   w-mod-not-saf-fas    =    "saf760"
                     perform   dvn-saf-760-000
                                          thru dvn-saf-760-999
           else      perform   dvn-saf-xyz-000
                                          thru dvn-saf-xyz-999        .
       dvn-900.
      *              *-------------------------------------------------*
      *              * Valore del campo 'Note' determinato in ritorno  *
      *              * al chiamante                                    *
      *              *-------------------------------------------------*
           move      w-aux-not-saf-not    to   w-mod-not-saf-not      .
       dvn-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf200'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-200-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-200-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf250'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-250-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-250-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf260'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-260-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-260-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf300'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-300-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-300-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf350'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-350-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-350-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf360'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-360-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-360-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf400'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-400-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-400-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf450'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-450-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-450-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf460'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-460-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-460-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf500'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-500-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-500-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf502'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-502-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-502-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf520'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-520-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-520-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf530'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-530-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-530-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf540'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-540-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-540-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf570'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-570-000.
           if        w-mod-not-saf-tdd    =    00
                     go to dvn-saf-570-100
           else      go to dvn-saf-570-999.
       dvn-saf-570-100.
      *              *-------------------------------------------------*
      *              * Editing prezzo                                  *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      rf-ffr-dec-vpp       to   p-dec                  .
           add       rf-ffr-dec-prz       to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-ffr-prz-net       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Pr:"                to   w-all-str-cat (1)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-aux-not-saf-not      .
      *
           go to     dvn-saf-570-999.
      *              *-------------------------------------------------*
      *              * Protocollo Iva                                  *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-fft-npt-iva       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (4)      .
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
______*    perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-570-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf600'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-600-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-600-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf620'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-620-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-620-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf630'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-630-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-630-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf640'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-640-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-640-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf670'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-670-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-670-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf700'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-700-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-700-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf720'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-720-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-720-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf725'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-725-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-725-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf730'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-730-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-730-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf740'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-740-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-740-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf750'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-750-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-750-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase 'saf760'                              *
      *    *-----------------------------------------------------------*
       dvn-saf-760-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-760-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per fase non riconosciuta dal modulo           *
      *    *-----------------------------------------------------------*
       dvn-saf-xyz-000.
      *              *-------------------------------------------------*
      *              * Richiamo subroutine generica                    *
      *              *-------------------------------------------------*
           perform   dvn-saf-gen-000      thru dvn-saf-gen-999        .
       dvn-saf-xyz-999.
           exit.

      *    *===========================================================*
      *    * Subroutine generica                                       *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    * Output : - w-aux-not-saf-not = Valore del campo 'Note'    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       dvn-saf-gen-000.
       dvn-saf-gen-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


