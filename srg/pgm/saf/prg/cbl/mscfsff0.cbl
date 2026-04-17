       Identification Division.
       Program-Id.                                 mscfsff0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/06/04    *
      *                       Ultima revisione:    NdK del 16/02/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la lettura dei documenti, e per  *
      *                    la determinazione della voce 'Fatturato'    *
      *                    da trattare per le statistiche di acquisto  *
      *                    sul fatturato su testate documento.         *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-scf-sff-ope : "OP"                 *
      *                                                                *
      *                       rf-sff            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-scf-sff-ope : "CL"                 *
      *                                                                *
      *                       rf-sff            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GS"  Inizializzazione lettura sequenziale archivio [sff]      *
      *                                                                *
      *              Input  : w-mod-scf-sff-ope : "GS"                 *
      *                                                                *
      *                       w-mod-scf-sff-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-scf-sff-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-scf-sff-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-scf-sff-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-scf-sff-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-scf-sff-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-scf-sff-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-sff            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GT"  Lettura sequenziale archivio [sff]                       *
      *                                                                *
      *              Input  : w-mod-scf-sff-ope : "GT"                 *
      *                                                                *
      *                       rf-sff            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-sff            : Record [sff] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-scf-sff-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-scf-sff-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-scf-sff-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-scf-sff-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-scf-sff-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-scf-sff-pon : Valore della voce    *
      *                                           con incidenza Po-    *
      *                                           sitiva o Negativa    *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Records logici                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-scf-sff.
      *        *-------------------------------------------------------*
      *        * Flag di inizializzazione lettura sequenziale [sff]    *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-fgs          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-npr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p1i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p1f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p2i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p2f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p3i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p3f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero record letti in totale                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-nrl          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, numero record letti                       *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p1n          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, numero record letti                       *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p2n          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, numero record letti                       *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-p3n          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 1. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-x1p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 2. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-x2p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 3. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-x3p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato                                    *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-val          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale merce in valuta base                    *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-tom          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale servizi in valuta base                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-tos          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale imballi in valuta base                  *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-toi          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale libero 4 in valuta base                 *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-to4          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale libero 5 in valuta base                 *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-to5          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale libero 6 in valuta base                 *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-to6          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale libero 7 in valuta base                 *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-to7          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale libero 8 in valuta base                 *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-to8          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Totale extra attivita' aziendale in valuta ba- *
      *        * se                                                    *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-tox          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Sconto in chiusura in valuta base              *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-scc          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Sconto pagamento in valuta base                *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-scp          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 1 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp1          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 2 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp2          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 3 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp3          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 4 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp4          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 5 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp5          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spesa 6 in valuta base                         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sp6          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spese incasso calcolate in valuta base         *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sic          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spese incasso aggiuntive in valuta base        *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sia          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Spese bollo calcolate in valuta base           *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sbc          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore Righe di addebito in valuta base               *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-rda          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Flag di selezione su record [sff]                     *
      *        *-------------------------------------------------------*
           05  w-aux-scf-sff-sel          pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'scf'           'mscfsff0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/saf/prg/cpy/mscfsff0.mdl"                   .

      *    *===========================================================*
      *    * Record file [sff]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsff"                          .

      ******************************************************************
       Procedure Division                using w-mod-scf-sff
                                               rf-sff                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-scf-sff-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-scf-sff-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-scf-sff-ope    =    "GS"
                     perform   gts-000    thru gts-999
           else if   w-mod-scf-sff-ope    =    "GT"
                     perform   get-000    thru get-999                .
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
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * non ancora eseguita Start                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-scf-sff-fgs      .
      *              *-------------------------------------------------*
      *              * Open file [dcf]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Close file [dcf]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [sff]       *
      *    *-----------------------------------------------------------*
       gts-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * Start eseguita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-aux-scf-sff-fgs      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-npr    to   w-aux-scf-sff-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p1i    to   w-aux-scf-sff-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p1f    to   w-aux-scf-sff-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p2i    to   w-aux-scf-sff-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p2f    to   w-aux-scf-sff-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p3i    to   w-aux-scf-sff-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-scf-sff-p3f    to   w-aux-scf-sff-p3f      .
      *              *-------------------------------------------------*
      *              * Numero records letti in totale                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-scf-sff-nrl      .
      *              *-------------------------------------------------*
      *              * 1. periodo, inizializzazone nr. records letti   *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-scf-sff-p1n      .
      *              *-------------------------------------------------*
      *              * 2. periodo, inizializzazone nr. records letti   *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-scf-sff-p2n      .
      *              *-------------------------------------------------*
      *              * 3. periodo, inizializzazone nr. records letti   *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-scf-sff-p3n      .
       gts-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale archivio [sff]                        *
      *    *-----------------------------------------------------------*
       get-000.
      *              *-------------------------------------------------*
      *              * Se flag di inizializzazione lettura sequenziale *
      *              * archivio [sff] ancora in Off : uscita come per  *
      *              * fine file                                       *
      *              *-------------------------------------------------*
           if        w-aux-scf-sff-fgs    =    spaces
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Se numero records letti in totale pari a zero   *
      *              * si va' alla Start sul 1. periodo, altrimenti    *
      *              * si va' alla Read Next su [sff]                  *
      *              *-------------------------------------------------*
           if        w-aux-scf-sff-nrl    =    zero
                     go to get-100
           else      go to get-400.
       get-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [sff] per il 1. periodo       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-scf-sff-p1i    to   rf-sff-ddo-ftf         .
           move      spaces               to   rf-sff-ndo-ftf         .
           move      zero                 to   rf-sff-num-ftf         .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita per fine file          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Altrimenti : a Read Next su [sff]               *
      *              *-------------------------------------------------*
           go to     get-400.
       get-200.
      *              *-------------------------------------------------*
      *              * Start su archivio [sff] per il 2. periodo       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-scf-sff-p2i    to   rf-sff-ddo-ftf         .
           move      spaces               to   rf-sff-ndo-ftf         .
           move      zero                 to   rf-sff-num-ftf         .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita per fine file          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Altrimenti : a Read Next su [sff]               *
      *              *-------------------------------------------------*
           go to     get-400.
       get-300.
      *              *-------------------------------------------------*
      *              * Start su archivio [sff] per il 3. periodo       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-scf-sff-p3i    to   rf-sff-ddo-ftf         .
           move      spaces               to   rf-sff-ndo-ftf         .
           move      zero                 to   rf-sff-num-ftf         .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita per fine file          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Altrimenti : a Read Next su [sff]               *
      *              *-------------------------------------------------*
           go to     get-400.
       get-400.
      *              *-------------------------------------------------*
      *              * Read Next su [sff]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsff"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sff                 .
      *              *-------------------------------------------------*
      *              * Se At End : uscita per fine file                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Manipolazioni preliminari sul record relativo   *
      *              * alla testata documento rf-sff letta             *
      *              *-------------------------------------------------*
           perform   man-pre-sff-000      thru man-pre-sff-999        .
       get-450.
      *              *-------------------------------------------------*
      *              * Test Max su [sff], a seconda del numero di pe-  *
      *              * riodi di riferimento, se non superato : uscita  *
      *              * per fine file                                   *
      *              *-------------------------------------------------*
       get-455.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero periodi   *
      *                  * di riferimento                              *
      *                  *---------------------------------------------*
           if        w-aux-scf-sff-npr    =    01
                     go to get-460
           else if   w-aux-scf-sff-npr    =    02
                     go to get-465
           else if   w-aux-scf-sff-npr    =    03
                     go to get-470.
       get-460.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 01       *
      *                  *---------------------------------------------*
           if        rf-sff-ddo-ftf       >    w-aux-scf-sff-p1f
                     go to get-900
           else      go to get-500.
       get-465.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 02       *
      *                  *---------------------------------------------*
           if        rf-sff-ddo-ftf       >    w-aux-scf-sff-p2f
                     go to get-900
           else      go to get-500.
       get-470.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 03       *
      *                  *---------------------------------------------*
           if        rf-sff-ddo-ftf       >    w-aux-scf-sff-p3f
                     go to get-900
           else      go to get-500.
       get-500.
      *              *-------------------------------------------------*
      *              * Determinazione appartenenza record letto ad uno *
      *              * o piu' periodi di riferimento, con incremento   *
      *              * del numero di records letti riferiti al periodo *
      *              *-------------------------------------------------*
       get-505.
      *                  *---------------------------------------------*
      *                  * Si/No record letto riferito a 1. periodo a  *
      *                  * No                                          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-aux-scf-sff-x1p      .
      *                  *---------------------------------------------*
      *                  * Si/No record letto riferito a 2. periodo a  *
      *                  * No                                          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-aux-scf-sff-x2p      .
      *                  *---------------------------------------------*
      *                  * Si/No record letto riferito a 3. periodo a  *
      *                  * No                                          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-aux-scf-sff-x3p      .
       get-510.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero periodi   *
      *                  * di riferimento                              *
      *                  *---------------------------------------------*
           if        w-aux-scf-sff-npr    =    01
                     go to get-515
           else if   w-aux-scf-sff-npr    =    02
                     go to get-520
           else if   w-aux-scf-sff-npr    =    03
                     go to get-525.
       get-515.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 01       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 1. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p1i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p1f
                     move  "S"            to   w-aux-scf-sff-x1p
                     add   1              to   w-aux-scf-sff-p1n      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     get-550.
       get-520.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 02       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 1. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p1i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p1f
                     move  "S"            to   w-aux-scf-sff-x1p
                     add   1              to   w-aux-scf-sff-p1n      .
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 2. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p2i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p2f
                     move  "S"            to   w-aux-scf-sff-x2p
                     add   1              to   w-aux-scf-sff-p2n      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     get-550.
       get-525.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 03       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 1. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p1i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p1f
                     move  "S"            to   w-aux-scf-sff-x1p
                     add   1              to   w-aux-scf-sff-p1n      .
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 2. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p2i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p2f
                     move  "S"            to   w-aux-scf-sff-x2p
                     add   1              to   w-aux-scf-sff-p2n      .
      *                      *-----------------------------------------*
      *                      * Si/No record letto riferito a 3. perio- *
      *                      * do                                      *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       not  < w-aux-scf-sff-p3i and
                     rf-sff-ddo-ftf       not  > w-aux-scf-sff-p3f
                     move  "S"            to   w-aux-scf-sff-x3p
                     add   1              to   w-aux-scf-sff-p3n      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     get-550.
       get-550.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dei risultati della de-  *
      *              * terminazione precedente                         *
      *              *-------------------------------------------------*
           if        w-aux-scf-sff-x1p    =    "S" or
                     w-aux-scf-sff-x2p    =    "S" or
                     w-aux-scf-sff-x3p    =    "S"
                     go to get-600
           else      go to get-650.
       get-600.
      *              *-------------------------------------------------*
      *              * Se il record letto appartiene ad almeno uno dei *
      *              * tre periodi di riferimento                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero totale records letti      *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-scf-sff-nrl      .
      *                  *---------------------------------------------*
      *                  * Selezione e determinazioni su [sff]         *
      *                  *---------------------------------------------*
           perform   sel-rec-sff-000      thru sel-rec-sff-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : riciclo alla    *
      *                  * Read next su [sff]                          *
      *                  *---------------------------------------------*
           if        w-aux-scf-sff-sel    not  = spaces
                     go to get-400.
      *                  *---------------------------------------------*
      *                  * Flag di uscita : Ok                         *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-scf-sff-flg      .
      *                  *---------------------------------------------*
      *                  * Si/No record riferito al 1. periodo         *
      *                  *---------------------------------------------*
           move      w-aux-scf-sff-x1p    to   w-mod-scf-sff-sn1      .
      *                  *---------------------------------------------*
      *                  * Si/No record riferito al 2. periodo         *
      *                  *---------------------------------------------*
           move      w-aux-scf-sff-x2p    to   w-mod-scf-sff-sn2      .
      *                  *---------------------------------------------*
      *                  * Si/No record riferito al 3. periodo         *
      *                  *---------------------------------------------*
           move      w-aux-scf-sff-x3p    to   w-mod-scf-sff-sn3      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-999.
       get-650.
      *              *-------------------------------------------------*
      *              * Se il record letto non appartiene a nessuno dei *
      *              * tre periodi di riferimento                      *
      *              *-------------------------------------------------*
       get-655.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero periodi   *
      *                  * di riferimento                              *
      *                  *---------------------------------------------*
           if        w-aux-scf-sff-npr    =    01
                     go to get-660
           else if   w-aux-scf-sff-npr    =    02
                     go to get-665
           else if   w-aux-scf-sff-npr    =    03
                     go to get-670.
       get-660.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 01       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Uscita per fine file                    *
      *                      *-----------------------------------------*
           go to     get-900.
       get-665.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 02       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la data documento in esame e' mino-  *
      *                      * re della data min del 2. periodo si va' *
      *                      * alla Start sul 2. periodo, altrimenti   *
      *                      * si va' ad uscita per fine file          *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       <    w-aux-scf-sff-p2i
                     go to get-200
           else      go to get-900.
       get-670.
      *                  *---------------------------------------------*
      *                  * Se numero periodi di riferimento : 03       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la data documento in esame e' mino-  *
      *                      * re della data min del 2. periodo si va' *
      *                      * alla Start sul 2; periodo.              *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       <    w-aux-scf-sff-p2i
                     go to get-200.
      *                      *-----------------------------------------*
      *                      * Se la data documento in esame e' mino-  *
      *                      * re della data min del 3. periodo si va' *
      *                      * alla Start sul 3. periodo, altrimenti   *
      *                      * si va' ad uscita per fine file          *
      *                      *-----------------------------------------*
           if        rf-sff-ddo-ftf       <    w-aux-scf-sff-p3i
                     go to get-300
           else      go to get-900.
       get-900.
      *              *-------------------------------------------------*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mod-scf-sff-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-scf-sff-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-scf-sff-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-scf-sff-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-scf-sff-val      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-scf-sff-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-999.
       get-999.
           exit.

      *    *===========================================================*
      *    * Determinazione                                            *
      *    *-----------------------------------------------------------*
       sel-rec-sff-000.
      *              *-------------------------------------------------*
      *              * Selezione sul flag di esclusione del documento  *
      *              * dalle statistiche                               *
      *              *-------------------------------------------------*
           if        rf-sff-flg-nbx (1)   not  = spaces
                     move  "#"            to   w-aux-scf-sff-sel
                     go to sel-rec-sff-999.
       sel-rec-sff-100.
      *              *-------------------------------------------------*
      *              * Determinazione del valore                       *
      *              *-------------------------------------------------*
       sel-rec-sff-200.
      *                  *---------------------------------------------*
      *                  * Inizializzazione preliminare a zero         *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-scf-sff-val      .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-sff-sgl-vlt       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-sff-dec-vlt       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-sff-tdc-vlt       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-sff-cdc-ftf       to   w-cvs-vlt-cdc          .
       sel-rec-sff-300.
      *                  *---------------------------------------------*
      *                  * Accumulo Totali Imponibili                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test sul codice valuta                  *
      *                      *-----------------------------------------*
           if        rf-sff-sgl-vlt       =    c-sgl
                     go to sel-rec-sff-400.
      *                      *-----------------------------------------*
      *                      * Conversione dei valori in valuta base   *
      *                      *-----------------------------------------*
           move      rf-sff-iiv-ftf       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *                      *-----------------------------------------*
      *                      * Test sul valore determinato             *
      *                      *-----------------------------------------*
           if        w-aux-scf-sff-val    >    zero
                     go to sel-rec-sff-390.
      *                      *-----------------------------------------*
      *                      * Conversione imponibili in valuta base   *
      *                      *-----------------------------------------*
           move      rf-sff-iva-ibl (01)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *
           move      rf-sff-iva-ibl (02)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *
           move      rf-sff-iva-ibl (03)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *
           move      rf-sff-iva-ibl (04)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *
           move      rf-sff-iva-ibl (05)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
      *
           move      rf-sff-iva-ibl (06)  to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           add       w-cvs-vlt-avb        to   w-aux-scf-sff-val      .
       sel-rec-sff-390.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     sel-rec-sff-900.
       sel-rec-sff-400.
      *                      *-----------------------------------------*
      *                      * Se valuta base                          *
      *                      *-----------------------------------------*
           add       rf-sff-iva-ibl (01)  to   w-aux-scf-sff-val      .
           add       rf-sff-iva-ibl (02)  to   w-aux-scf-sff-val      .
           add       rf-sff-iva-ibl (03)  to   w-aux-scf-sff-val      .
           add       rf-sff-iva-ibl (04)  to   w-aux-scf-sff-val      .
           add       rf-sff-iva-ibl (05)  to   w-aux-scf-sff-val      .
           add       rf-sff-iva-ibl (06)  to   w-aux-scf-sff-val      .
      *                      *-----------------------------------------*
      *                      * Test sul valore determinato             *
      *                      *-----------------------------------------*
           if        w-aux-scf-sff-val    >    zero
                     go to sel-rec-sff-490.
      *                      *-----------------------------------------*
      *                      * Totale fattura                          *
      *                      *-----------------------------------------*
           add       rf-sff-imp-ftf       to   w-aux-scf-sff-val      .
       sel-rec-sff-490.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     sel-rec-sff-900.
       sel-rec-sff-900.
      *              *-------------------------------------------------*
      *              * Uscita dalla determinazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore determinato in uscita                *
      *                  *---------------------------------------------*
           move      w-aux-scf-sff-val    to   w-mod-scf-sff-val      .
      *                  *---------------------------------------------*
      *                  * Incidenza in positivo o in negativo         *
      *                  *---------------------------------------------*
           if        rf-sff-tip-ftf       =    01 or
                     rf-sff-tip-ftf       =    02
                     move  "P"            to   w-mod-scf-sff-pon
           else      move  "N"            to   w-mod-scf-sff-pon      .
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           if        w-mod-scf-sff-val    =    zero
                     move  "#"            to   w-aux-scf-sff-sel
           else      move  spaces         to   w-aux-scf-sff-sel      .
       sel-rec-sff-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla testa- *
      *    * ta documento rf-sff letta                                 *
      *    *-----------------------------------------------------------*
       man-pre-sff-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice fornitore e   *
      *              * della sua dipendenza in funzione dello status   *
      *              * espresso in anagrafica commerciale              *
      *              *-------------------------------------------------*
       man-pre-sff-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcf]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Lettura record principale dell'anagrafica   *
      *                  * commerciale del fornitore                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-sff-cod-fnt       to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : nessuna manipola- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to man-pre-sff-900.
       man-pre-sff-200.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del fornitore non  *
      *                  * indica che e' stato sostituito da una altro *
      *                  * fornitore : nessuna manipolazione           *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tus       not  = 21 and
                     rf-dcf-sta-tus       not  = 52 and
                     rf-dcf-sta-tus       not  = 62 and
                     rf-dcf-sta-tus       not  = 72
                     go to man-pre-sff-900.
      *                  *---------------------------------------------*
      *                  * Se il codice fornitore di riferimento e' a  *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tuc       =    zero
                     go to man-pre-sff-900.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo fornitore : nessuna ma- *
      *                  * nipolazione                                 *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tux       not  = 02 and
                     rf-dcf-sta-tux       not  = 03
                     go to man-pre-sff-900.
       man-pre-sff-300.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * del fornitore, ed eventualmente anche la    *
      *                  * dipendenza                                  *
      *                  *---------------------------------------------*
           move      rf-dcf-sta-tuc       to   rf-sff-cod-fnt         .
           if        rf-dcf-sta-tux       =    02
                     move  spaces         to   rf-sff-dpz-fnt         .
       man-pre-sff-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     man-pre-sff-999.
       man-pre-sff-999.
           exit.

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare da convertire, espres- *
      *    *                          so nell'altra valuta             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-avb = Ammontare convertito, espresso   *
      *    *                          nella valuta base                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta base ad altra valuta     *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare da convertire, espres- *
      *    *                          so nella valuta base             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-aav = Ammontare convertito, espresso   *
      *    *                          nell'altra valuta                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta a valuta                 *
      *    *                                                           *
      *    * Input  : Valuta da trasformare                            *
      *    *                                                           *
      *    *            - w-cvs-vdt-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vdt-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vdt-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vdt-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    *            - w-cvs-vdt-amm = Ammontare da convertire,     *
      *    *                              espresso nella valuta da     *
      *    *                              trasformare                  *
      *    *                                                           *
      *    *          Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vnu-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vnu-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vnu-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    * Output : Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-amm = Ammontare convertito,        *
      *    *                              espresso nella valuta        *
      *    *                              in cui trasformare           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

