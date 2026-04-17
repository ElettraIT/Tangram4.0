       Identification Division.
       Program-Id.                                 acoddcc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dcc                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 11/07/91    *
      *                       Ultima revisione:    NdK del 07/10/03    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice dipendenza del   *
      *                    cliente                                     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-dcc-cli : codice cliente       *
      *                                                                *
      *                       w-cod-cod-dcc-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dcc-lin : linea codice         *
      *                                                                *
      *                       w-cod-cod-dcc-pos : posizione codice     *
      *                                                                *
      *                       w-cod-cod-dcc-rln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dcc-rps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A*"  Inizio accettazione, con ammissibilita' del codice di-   *
      *       pendenza "*   "                                          *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "A*"                 *
      *                                                                *
      *                       w-cod-cod-dcc-cli : codice cliente       *
      *                                                                *
      *                       w-cod-cod-dcc-cod : codice dipendenza    *
      *                                                                *
      *                       w-cod-cod-dcc-lin : linea codice         *
      *                                                                *
      *                       w-cod-cod-dcc-pos : posizione codice     *
      *                                                                *
      *                       w-cod-cod-dcc-rln : linea descrizione    *
      *                                                                *
      *                       w-cod-cod-dcc-rps : posizione descriz.   *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcc-cli : codice cliente       *
      *                                                                *
      *                       w-cod-cod-dcc-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "I+"  Continuazione accettazione dopo esecuzione "Insr"        *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "I+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcc-cli : codice cliente       *
      *                                                                *
      *                       w-cod-cod-dcc-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "F+"  Continuazione accettazione dopo esecuzione "Find"        *
      *                                                                *
      *              Input  : w-cod-cod-dcc-ope : "F+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-dcc-ope : "A+" = continuare    *
      *                                           "F+" = eseguire Find *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-dcc-cli : codice cliente       *
      *                                                                *
      *                       w-cod-cod-dcc-cod : codice dipendenza    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "NT"  Richiesta eventuali note associate al cliente            *
      *                                                                *
      *              Input  : w-cod-mne-dcc-ope : "NT"                 *
      *                       w-cod-mne-dcc-prg : nome del programma   *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [dcx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcx"                          .

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
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux-cde-dcc.
           05  w-aux-cde-dcc-nli          pic  9(02)                  .
           05  w-aux-cde-dcc-crb          pic  9(02)                  .
           05  w-aux-cde-dcc-cpb          pic  9(02)                  .
           05  w-aux-cde-dcc-cpa          pic  9(02)                  .
           05  w-aux-cde-dcc-bix          pic  9(02)                  .
           05  w-aux-cde-dcc-buf
                               occurs 30.
               10  w-aux-cde-dcc-bco      pic  x(04)                  .
               10  w-aux-cde-dcc-brs      pic  x(40)                  .
               10  w-aux-cde-dcc-bvd      pic  x(40)                  .
               10  w-aux-cde-dcc-bld      pic  x(40)                  .
           05  w-aux-cde-dcc-ltp          pic  x(17)                  .
           05  w-aux-cde-dcc-le1          pic  x(03)                  .
           05  w-aux-cde-dcc-le2          pic  x(03)                  .
           05  w-aux-cde-dcc-dup          pic  x(40)                  .
           05  w-aux-cde-dcc-dmx.
               10  w-aux-cde-dcc-dch
                               occurs 40  pic  x(01)                  .
           05  w-aux-cde-dcc-c01          pic  9(02)                  .
           05  w-aux-cde-dcc-c02          pic  9(02)                  .
           05  w-aux-cde-dcc-c03          pic  9(02)                  .
           05  w-aux-cde-dcc-c04          pic  9(02)                  .
           05  w-aux-cde-dcc-c05          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutine relativa alle note                    *
      *    *-----------------------------------------------------------*
       01  w-not-cli.
      *        *-------------------------------------------------------*
      *        * Numero di linee necessarie                            *
      *        *-------------------------------------------------------*
           05  w-not-cli-num-lin          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee da togliere eventualmente             *
      *        *-------------------------------------------------------*
           05  w-not-cli-num-lis          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente salvato                                *
      *        *-------------------------------------------------------*
           05  w-not-cli-cod-cli          pic  9(07)                  .
           05  w-not-cli-dpz-cli          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza cliente per la start                *
      *        *-------------------------------------------------------*
           05  w-not-cli-dpz-str          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Buffer annotazioni                                    *
      *        *-------------------------------------------------------*
           05  w-not-cli-ctr-ele          pic  9(03)                  .
           05  w-not-cli-inx-ele          pic  9(03)                  .
           05  w-not-cli-max-ele          pic  9(03)       value 30   .
           05  w-not-cli-not-buf occurs 30.
               10  w-not-cli-cod-dpz      pic  x(04)                  .
               10  w-not-cli-cod-ann      pic  9(03)                  .
               10  w-not-cli-cod-ang      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodi                                                *
      *        *-------------------------------------------------------*
           05  w-not-cli-ctr-001          pic  9(02)                  .
           05  w-not-cli-ctr-002          pic  9(05)                  .
           05  w-not-cli-ctr-003          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-ble.
           05  w-ble-max                  pic  9(02)                  .
           05  w-ble-flg                  pic  x(01)                  .
           05  w-ble-str.
               10  w-ble-chr    occurs 40 pic  x(01)                  .
           05  w-ble-ctr                  pic  9(02)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-dcc
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
           if        w-cod-cod-dcc-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-dcc-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-dcc-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-dcc-ope    =    "AC" or
                     w-cod-cod-dcc-ope    =    "A*"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-dcc-ope    =    "A+" or
                     w-cod-cod-dcc-ope    =    "I+" or
                     w-cod-cod-dcc-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-cod-dcc-ope    =    "NT"
                     perform   not-000    thru not-999                .
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
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cod-cod-dcc-prg      .
           move      zero                 to   w-not-cli-cod-cli      .
           move      spaces               to   w-not-cli-dpz-cli      .
      *                  *---------------------------------------------*
      *                  * Open file [dcc]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Open file [dcx]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
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
      *                  *---------------------------------------------*
      *                  * Close file [dcc]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Close file [dcx]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
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
                     move  spaces         to   w-cod-cod-dcc-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione segnale se accettazione con am-  *
      *              * missibilita' del codice dipendenza "*   " oppu- *
      *              * re no                                           *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcc-ope    =    "A*"
                     move  "#"            to   w-cod-cod-dcc-fla
           else      move  spaces         to   w-cod-cod-dcc-fla      .
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Find           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4011"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-100.
           if        v-pfk (03)           =    "FIND"
                     move  spaces         to   v-pfk (03)             .
       acc-100.
      *              *-------------------------------------------------*
      *              * Eliminazione eventuale del tasto Insr           *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4000"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     go to acc-200.
           if        v-pfk (04)           =    "INSR"
                     move  spaces         to   v-pfk (04)             .
       acc-200.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente di default                   *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcc-cli    to   w-cod-cod-dcc-s02      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza di default                *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcc-cod    to   w-cod-cod-dcc-s01      .
      *                  *---------------------------------------------*
      *                  * Maschera di editing                         *
      *                  *---------------------------------------------*
           move      v-edm                to   w-cod-cod-dcc-s80      .
      *                  *---------------------------------------------*
      *                  * User function keys                          *
      *                  *---------------------------------------------*
           move      v-ufk                to   w-cod-cod-dcc-s90      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcc-s90    to   v-ufk                  .
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-dcc-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di rientro      *
      *              *-------------------------------------------------*
           if        w-cod-cod-dcc-ope    =    "A+"
                     go to aco-400
           else if   w-cod-cod-dcc-ope    =    "F+"
                     go to aco-200.
       aco-100.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Insr                            *
      *              *-------------------------------------------------*
       aco-150.
      *                  *---------------------------------------------*
      *                  * Reimpostazione                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione function key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Preparazione accettazione               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcc-s90    to   v-ufk                  .
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : continuazione       *
      *                      *-----------------------------------------*
           move      "A+"                 to   w-cod-cod-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro dopo Find                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione i.p.c. di Select eventuali       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Variabile 'dpz-cli'                     *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dpz-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se e' stata effettuata *
      *                  * una selezione oppure no                     *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to aco-250.
       aco-225.
      *                  *---------------------------------------------*
      *                  * Se selezione non effettuata                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     aco-150.
       aco-250.
      *                  *---------------------------------------------*
      *                  * Se selezione effettuata                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore selezionato in uscita            *
      *                      *-----------------------------------------*
           move      s-alf                to   w-cod-cod-dcc-cod      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valore selezionato      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           move      w-cod-cod-dcc-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key            *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non-continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-400.
      *              *-------------------------------------------------*
      *              * Se rientro dopo impostazione non terminata da   *
      *              * Find ne' da Insr                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt                              *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-425.
      *                      *-----------------------------------------*
      *                      * Rivisualizzazione valore originale      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           move      w-cod-cod-dcc-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-425.
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore impostato             *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-cod-dcc-cod      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di comodo          *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcc-cod    to   w-cod-cod-dcc-alf      .
      *                  *---------------------------------------------*
      *                  * Test se valore pari a "*   " e valore non   *
      *                  * ammissibile                                 *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-alf    =    "*   " and
                     w-cod-cod-dcc-fla    =    spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Test se blanks embedded, e se si a reimpo-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcc-alf    to   w-ble-str              .
           move      04                   to   w-ble-max              .
           perform   ble-000              thru ble-999                .
           if        w-ble-flg            not  = spaces
                     go to aco-150.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del valore impostato *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-alf    =    "-"
                     go to aco-600.
       aco-500.
      *                  *---------------------------------------------*
      *                  * Se valore impostato normale                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-600.
      *                  *---------------------------------------------*
      *                  * Se valore impostato pari a '-', per esegui- *
      *                  * re la ricerca per descrizione               *
      *                  *---------------------------------------------*
       aco-610.
      *                      *-----------------------------------------*
      *                      * Test che esistano linea e posizione per *
      *                      * la descrizione, e se no a reimpostazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        w-cod-cod-dcc-rln    =    zero or
                     w-cod-cod-dcc-rps    =    zero
                     go to aco-150.
       aco-620.
      *                  *---------------------------------------------*
      *                  * Spaces in comodo per impostazione descri-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-cde-dcc-dup      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area di accetta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcc-rln    to   v-lin                  .
           move      w-cod-cod-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-623.
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area per indiriz- *
      *                  * zo                                          *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-vln    =    zero
                     go to aco-626.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcc-vln    to   v-lin                  .
           move      w-cod-cod-dcc-vps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-626.
      *                  *---------------------------------------------*
      *                  * Visualizzazione spaces in area per locali-  *
      *                  * ta'                                         *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-lln    =    zero
                     go to aco-630.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcc-lln    to   v-lin                  .
           move      w-cod-cod-dcc-lps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-630.
      *                  *---------------------------------------------*
      *                  * Accettazione descrizione in uppercase       *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-cod-cod-dcc-rln    to   v-lin                  .
           move      w-cod-cod-dcc-rps    to   v-pos                  .
           move      w-aux-cde-dcc-dup    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-alf                to   w-aux-cde-dcc-dup      .
       aco-640.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to aco-650.
      *                      *-----------------------------------------*
      *                      * Visualizzazione spaces in area di ac-   *
      *                      * cettazione                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcc-rln    to   v-lin                  .
           move      w-cod-cod-dcc-rps    to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * A reimpostazione codice                 *
      *                      *-----------------------------------------*
           go to     aco-150.
       aco-650.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT"
                     go to aco-660.
      *                      *-----------------------------------------*
      *                      * Tipo operazione a : non continuazione   *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     aco-999.
       aco-660.
      *                  *---------------------------------------------*
      *                  * Se Return                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se impostazione a spaces : a reimposta- *
      *                      * zione codice                            *
      *                      *-----------------------------------------*
           if        w-aux-cde-dcc-dup    =    spaces
                     go to aco-150.
       aco-670.
      *                      *-----------------------------------------*
      *                      * Comodo per descrizione in uppercase con *
      *                      * padding finale per il max               *
      *                      *-----------------------------------------*
           move      w-aux-cde-dcc-dup    to   w-aux-cde-dcc-dmx      .
           move      40                   to   w-aux-cde-dcc-c01      .
       aco-680.
           if        w-aux-cde-dcc-c01    >    zero
                     if    w-aux-cde-dcc-dch
                          (w-aux-cde-dcc-c01)
                                          =    spaces
                           move     "z"   to   w-aux-cde-dcc-dch
                                              (w-aux-cde-dcc-c01)
                           subtract 1     from w-aux-cde-dcc-c01
                           go to aco-680.
       aco-700.
      *                      *-----------------------------------------*
      *                      * Lettura e bufferizzazione fino ad un    *
      *                      * massimo di 30 records con descrizio-    *
      *                      * ne compresa tra il minimo ed il mas-    *
      *                      * simo                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Azzeramento contatore records nel   *
      *                          * buffer                              *
      *                          *-------------------------------------*
           move      zero                 to   w-aux-cde-dcc-crb      .
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "RAGKEY    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-aux-cde-dcc-dup    to   rf-dcc-rag-key         .
           move      w-cod-cod-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Se start errata : a trattamento per *
      *                          * fine file                           *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-800.
       aco-710.
      *                          *-------------------------------------*
      *                          * Read next                           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       aco-720.
      *                          *-------------------------------------*
      *                          * Test se At End                      *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to aco-800.
       aco-730.
      *                          *-------------------------------------*
      *                          * Test sul max                        *
      *                          *-------------------------------------*
           if        rf-dcc-rag-key       >    w-aux-cde-dcc-dmx
                     go to aco-800.
      *                          *-------------------------------------*
      *                          * Selezione su codice cliente : se    *
      *                          * non e' uguale si ricicla            *
      *                          *-------------------------------------*
           if        rf-dcc-cod-cli       not  = w-cod-cod-dcc-cli
                     go to aco-710.
      *                          *-------------------------------------*
      *                          * Selezione su codice dipendenza : se *
      *                          * a spaces si ricicla                 *
      *                          *-------------------------------------*
           if        rf-dcc-dpz-cli       =    spaces
                     go to aco-710.
       aco-740.
      *                          *-------------------------------------*
      *                          * Incremento numero records nel buf-  *
      *                          * fer                                 *
      *                          *-------------------------------------*
           add       1                    to   w-aux-cde-dcc-crb      .
      *                          *-------------------------------------*
      *                          * Se oltre il massimo : come per fine *
      *                          * file                                *
      *                          *-------------------------------------*
           if        w-aux-cde-dcc-crb    >    30
                     go to aco-800.
       aco-750.
      *                          *-------------------------------------*
      *                          * Bufferizzazione record nel buffer   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice dipendenza               *
      *                              *---------------------------------*
           move      rf-dcc-dpz-cli       to   w-aux-cde-dcc-bco
                                              (w-aux-cde-dcc-crb)     .
      *                              *---------------------------------*
      *                              * Ragione sociale                 *
      *                              *---------------------------------*
           move      rf-dcc-rag-soc       to   w-aux-cde-dcc-brs
                                              (w-aux-cde-dcc-crb)     .
      *                              *---------------------------------*
      *                              * Indirizzo                       *
      *                              *---------------------------------*
           move      rf-dcc-via-dcc       to   w-aux-cde-dcc-bvd
                                              (w-aux-cde-dcc-crb)     .
      *                              *---------------------------------*
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      rf-dcc-loc-dcc       to   w-aux-cde-dcc-bld
                                              (w-aux-cde-dcc-crb)     .
       aco-760.
      *                          *-------------------------------------*
      *                          * Riciclo nella lettura               *
      *                          *-------------------------------------*
           go to aco-710.
       aco-800.
      *                      *-----------------------------------------*
      *                      * Esame dei risultati della lettura con   *
      *                      * bufferizzazione dei max 30 records      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del numero   *
      *                          * di records bufferizzati             *
      *                          *-------------------------------------*
           if        w-aux-cde-dcc-crb    =    zero
                     go to aco-810
           else  if  w-aux-cde-dcc-crb    >    30
                     go to aco-820
           else  if  w-aux-cde-dcc-crb    =    1
                     go to aco-830
           else      go to aco-850.
       aco-810.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a zero                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione puntini in area *
      *                              * di accettazione                 *
      *                              *---------------------------------*
           if        w-cod-cod-dcc-rln    =    zero
                     go to aco-812.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-cod-cod-dcc-rln    to   v-lin                  .
           move      w-cod-cod-dcc-rps    to   v-pos                  .
           move      all   "."            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-812.
      *                              *---------------------------------*
      *                              * A reimpostazione codice         *
      *                              *---------------------------------*
           go to     aco-150.
       aco-820.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati maggiore di 30              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se programma di interrogazione  *
      *                              * gia' attivo : a reimpostazione  *
      *                              * codice senza alcuna segnalazio- *
      *                              * ne                              *
      *                              *---------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pdcc4011"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to aco-150.
      *                              *---------------------------------*
      *                              * Preparazione variabili di i.p.c.*
      *                              * per l'esecuzione della interro- *
      *                              * gazione con parametri precabla- *
      *                              * ti                              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo interrogazione : 'D'   *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "D"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Codice cliente              *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-cli"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-cod-cod-dcc-cli    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                                  *-----------------------------*
      *                                  * Ragione sociale             *
      *                                  *-----------------------------*
           move      "PV"                 to   s-ope                  .
           move      "rag-key"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      30                   to   s-car                  .
           move      w-aux-cde-dcc-dup    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                              *---------------------------------*
      *                              * Preparazione uscita per Find    *
      *                              * precablato                      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Tipo operazione a : esecu-  *
      *                                  * zione Find                  *
      *                                  *-----------------------------*
           move      "F+"                 to   w-cod-cod-dcc-ope      .
      *                                  *-----------------------------*
      *                                  * Simulazione tasto Find      *
      *                                  *-----------------------------*
           move      "FIND"               to   v-key                  .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-830.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati pari a 1                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione indice sul primo   *
      *                              * elemento del buffer             *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-dcc-bix      .
       aco-840.
      *                              *---------------------------------*
      *                              * Valore selezionato in uscita    *
      *                              *---------------------------------*
           move      w-aux-cde-dcc-bco
                    (w-aux-cde-dcc-bix)   to   w-cod-cod-dcc-cod      .
      *                              *---------------------------------*
      *                              * Visualizzazione valore selezio- *
      *                              * nato                            *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           move      w-cod-cod-dcc-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Normalizzazione function-key    *
      *                              *---------------------------------*
           move      spaces               to   v-key                  .
      *                              *---------------------------------*
      *                              * Tipo operazione a : non-conti-  *
      *                              * nuazione                        *
      *                              *---------------------------------*
           move      "AC"                 to   w-cod-cod-dcc-ope      .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     aco-999.
       aco-850.
      *                          *-------------------------------------*
      *                          * Se numero records trovati e buffe-  *
      *                          * rizzati compreso tra 1 e 30, si e-  *
      *                          * segue la selezione nel box locale   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione numero pagine    *
      *                              * nel buffer                      *
      *                              *---------------------------------*
           move      w-aux-cde-dcc-crb    to   w-aux-cde-dcc-cpb      .
           subtract  1                    from w-aux-cde-dcc-cpb      .
           divide    4                    into w-aux-cde-dcc-cpb      .
           add       1                    to   w-aux-cde-dcc-cpb      .
      *                              *---------------------------------*
      *                              * Inizializzazione numero record  *
      *                              * nel buffer attualmente trattato *
      *                              *---------------------------------*
           move      1                    to   w-aux-cde-dcc-c01      .
      *                              *---------------------------------*
      *                              * Salvataggio immagine video      *
      *                              *---------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione box vuoto       *
      *                              *---------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      68                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      "        Selezionare la dipendenza desiderata      
      -              "  "                 to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   aco-950              thru aco-959                .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-860.
      *                              *---------------------------------*
      *                              * Determinazione numero linea a   *
      *                              * video in funzione del numero    *
      *                              * elemento in tabella trattato,   *
      *                              * di indice (w-aux-cde-dcc-c01)   *
      *                              *---------------------------------*
           divide    4                    into w-aux-cde-dcc-c01
                                        giving w-aux-cde-dcc-c05
                                     remainder w-aux-cde-dcc-nli      .
           if        w-aux-cde-dcc-nli    =    zero
                     move  4              to   w-aux-cde-dcc-nli      .
           multiply  3                    by   w-aux-cde-dcc-nli      .
           add       04                   to   w-aux-cde-dcc-nli      .
       aco-870.
      *                              *---------------------------------*
      *                              * Accettazione di una function    *
      *                              * key                             *
      *                              *---------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-cde-dcc-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-cde-dcc-c01    <    w-aux-cde-dcc-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-aux-cde-dcc-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-cde-dcc-cpa    <    w-aux-cde-dcc-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-cde-dcc-nli    to   v-lin                  .
           move      22                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                              *---------------------------------*
      *                              * Deviazione a seconda della fun- *
      *                              * ction key impostata             *
      *                              *---------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to aco-880
           else if   v-key                =    "UP  "
                     go to aco-890
           else if   v-key                =    "DOWN"
                     go to aco-900
           else if   v-key                =    "EXIT"
                     go to aco-910
           else if   v-key                =    "NXSC"
                     go to aco-920
           else if   v-key                =    "PRSC"
                     go to aco-930
           else      go to aco-870.
       aco-880.
      *                              *---------------------------------*
      *                              * Se Do oppure Slct               *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Preparazione indice sull'e- *
      *                                  * lemento del buffer selezio- *
      *                                  * nato                        *
      *                                  *-----------------------------*
           move      w-aux-cde-dcc-c01    to   w-aux-cde-dcc-bix      .
      *                                  *-----------------------------*
      *                                  * Ad uscita dopo preparazione *
      *                                  * e visualizzazione del valo- *
      *                                  * re selezionato              *
      *                                  *-----------------------------*
           go to     aco-840.
       aco-890.
      *                              *---------------------------------*
      *                              * Se Up                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-dcc-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sulla prima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-nli    =    07
                     go to aco-940
           else      go to aco-860.
       aco-900.
      *                              *---------------------------------*
      *                              * Se Down o Return                *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se si e' gia' sull'ultima   *
      *                                  * linea si ricicla all'impo-  *
      *                                  * stazione                    *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-c01    =    w-aux-cde-dcc-crb
                     go to aco-870.
      *                                  *-----------------------------*
      *                                  * Incremento indice su ele-   *
      *                                  * mento attualmente trattato  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-dcc-c01      .
      *                                  *-----------------------------*
      *                                  * Se non si era sull'ultima   *
      *                                  * linea trattata si ricicla   *
      *                                  * sulla linea precedente, al- *
      *                                  * trimenti si fa' precedere   *
      *                                  * la visualizzazione della    *
      *                                  * nuova pagina trattata       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-nli    =    16
                     go to aco-940
           else      go to aco-860.
       aco-910.
      *                              *---------------------------------*
      *                              * Se Exit                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Ripristino immagine video   *
      *                                  *-----------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * A reimpostazione ragione    *
      *                                  * sociale                     *
      *                                  *-----------------------------*
           go to     aco-630.
       aco-920.
      *                              *---------------------------------*
      *                              * Se Nxsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-dcc-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-dcc-cpa    to   w-aux-cde-dcc-c01      .
           multiply  4                    by   w-aux-cde-dcc-c01      .
           subtract  3                    from w-aux-cde-dcc-c01      .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-940.
       aco-930.
      *                              *---------------------------------*
      *                              * Se Prsc                         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decremento numero pagina a  *
      *                                  * video attualmente trattata  *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-dcc-cpa      .
      *                                  *-----------------------------*
      *                                  * Ricalcolo indice primo ele- *
      *                                  * mento della pagina video    *
      *                                  * attualmente trattata        *
      *                                  *-----------------------------*
           move      w-aux-cde-dcc-cpa    to   w-aux-cde-dcc-c01      .
           multiply  4                    by   w-aux-cde-dcc-c01      .
           subtract  3                    from w-aux-cde-dcc-c01      .
      *                                  *-----------------------------*
      *                                  * A visualizzazione nuova pa- *
      *                                  * gina video                  *
      *                                  *-----------------------------*
           go to     aco-940.
       aco-940.
      *                              *---------------------------------*
      *                              * Visualizzazione pagina video    *
      *                              * contenente il record attual-    *
      *                              * mente trattato                  *
      *                              *---------------------------------*
           perform   aco-950              thru aco-959                .
      *                              *---------------------------------*
      *                              * A reimpostazione function key   *
      *                              *---------------------------------*
           go to     aco-860.
       aco-950.
      *                              *---------------------------------*
      *                              * Subroutine interna di visualiz- *
      *                              * zazione pagina video contenente *
      *                              * il record attualmente trattato  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione numero pagi- *
      *                                  * na attualmente trattata     *
      *                                  *-----------------------------*
           move      w-aux-cde-dcc-c01    to   w-aux-cde-dcc-c02      .
           add       3                    to   w-aux-cde-dcc-c02      .
           divide    4                    into w-aux-cde-dcc-c02      .
           move      w-aux-cde-dcc-c02    to   w-aux-cde-dcc-cpa      .
      *                                  *-----------------------------*
      *                                  * Determinazione indice primo *
      *                                  * elemento ed ultimo elemento *
      *                                  * della pagina attualmente    *
      *                                  * trattata                    *
      *                                  *-----------------------------*
           subtract  1                    from w-aux-cde-dcc-c02      .
           multiply  4                    by   w-aux-cde-dcc-c02      .
           add       1                    to   w-aux-cde-dcc-c02      .
           add       3 
                     w-aux-cde-dcc-c02  giving w-aux-cde-dcc-c03      .
           move      w-aux-cde-dcc-c03    to   w-aux-cde-dcc-c04      .
           if        w-aux-cde-dcc-c03    >    w-aux-cde-dcc-crb
                     move  w-aux-cde-dcc-crb
                                          to   w-aux-cde-dcc-c03      .
      *                                  *-----------------------------*
      *                                  * Inizializzazione indice per *
      *                                  * numero linea a video rela-  *
      *                                  * tiva al primo elemento      *
      *                                  *-----------------------------*
           move      07                   to   w-aux-cde-dcc-c05      .
       aco-951.
      *                                  *-----------------------------*
      *                                  * Visualizzazione linea       *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Codice dipendenza       *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-aux-cde-dcc-bco
                    (w-aux-cde-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Ragione sociale         *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-aux-cde-dcc-brs
                    (w-aux-cde-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Via                     *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-cde-dcc-bvd
                    (w-aux-cde-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                      *-------------------------*
      *                                      * Localita'               *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      w-aux-cde-dcc-bld
                    (w-aux-cde-dcc-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                                  *-----------------------------*
      *                                  * Incremento numero elemento  *
      *                                  * trattato                    *
      *                                  *-----------------------------*
           add       1                    to   w-aux-cde-dcc-c02      .
      *                                  *-----------------------------*
      *                                  * Incremento numero linea a   *
      *                                  * video                       *
      *                                  *-----------------------------*
           add       3                    to   w-aux-cde-dcc-c05      .
      *                                  *-----------------------------*
      *                                  * Se non si e' oltre l'ultimo *
      *                                  * elemento reale si ricicla   *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-c02    not  > w-aux-cde-dcc-c03
                     go to aco-951.
       aco-952.
      *                                  *-----------------------------*
      *                                  * Se si e' oltre l'ultimo e-  *
      *                                  * lemento della pagina si va' *
      *                                  * al trattamento finale       *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-c02    >    w-aux-cde-dcc-c04
                     go to aco-955.
      *                                  *-----------------------------*
      *                                  * Altrimenti si pongono com-  *
      *                                  * pletamente a spaces le li-  *
      *                                  * nee video residue all'in-   *
      *                                  * terno del box               *
      *                                  *-----------------------------*
           if        w-aux-cde-dcc-crb    not  > 4
                     go to aco-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      52                   to   v-car                  .
           move      w-aux-cde-dcc-c05    to   v-lin                  .
           add       2                    to   v-lin                  .
           move      15                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-aux-cde-dcc-c02      .
           add       3                    to   w-aux-cde-dcc-c05      .
           go to     aco-952.
       aco-955.
      *                                  *-----------------------------*
      *                                  * Trattamento finale : visua- *
      *                                  * lizzazione del numero pagi- *
      *                                  * na attuale e del numero di  *
      *                                  * pagine totali               *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-cde-dcc-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cde-dcc-le1      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-aux-cde-dcc-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-aux-cde-dcc-le2      .
           move      spaces               to   w-aux-cde-dcc-ltp      .
           string    "Pagina "  delimited by   size
                     w-aux-cde-dcc-le1
                                delimited by   spaces
                     " di "     delimited by   size
                     w-aux-cde-dcc-le2
                                delimited by   spaces
                                          into w-aux-cde-dcc-ltp      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      32                   to   v-pos                  .
           move      w-aux-cde-dcc-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-959.
           exit.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-ble-str                      *
      *    *-----------------------------------------------------------*
       ble-000.
           move      spaces               to   w-ble-flg              .
           if        w-ble-str            =    spaces
                     go to ble-999.
           if        w-ble-chr (1)        =    spaces
                     move  "#"            to   w-ble-flg
                     go to ble-999.
           move      1                    to   w-ble-ctr              .
       ble-100.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           not  = spaces
                     go to ble-100.
       ble-200.
           add       1                    to   w-ble-ctr              .
           if        w-ble-ctr            >    w-ble-max
                     go to ble-999.
           if        w-ble-chr
                    (w-ble-ctr)           =    spaces
                     go to ble-200.
           move      "#"                  to   w-ble-flg              .
       ble-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *-----------------------------------------------------------*
       not-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      w-cod-cod-dcc-cod    to   w-not-cli-dpz-str      .
           move      zero                 to   w-not-cli-ctr-002      .
       not-050.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-cod    =    spaces
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Test se presente codice programma           *
      *                  *---------------------------------------------*
           if        w-cod-cod-dcc-prg    =    spaces
                     go to not-900.
      *                  *---------------------------------------------*
      *                  * Test se stesso cliente                      *
      *                  *                                             *
      *                  * Da rivedere                                 *
      *                  *---------------------------------------------*
______*    if        w-cod-cod-dcc-cli    =    w-not-cli-cod-cli and
______*              w-cod-cod-dcc-cod    =    w-not-cli-dpz-cli
______*              go to not-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione cliente in corso di tratta- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      w-cod-cod-dcc-cli    to   w-not-cli-cod-cli      .
           move      w-cod-cod-dcc-cod    to   w-not-cli-dpz-cli      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione annotazioni per il cliente  *
      *                  *---------------------------------------------*
           perform   not-buf-000          thru not-buf-999            .
           if        w-not-cli-ctr-ele    =    zero
                     go to not-900.
       not-100.
      *              *-------------------------------------------------*
      *              * Inizio scansione tabella bufferizzata           *
      *              *-------------------------------------------------*
           move      zero                 to   w-not-cli-inx-ele      .
       not-200.
      *              *-------------------------------------------------*
      *              * Incremento e test su contatore                  *
      *              *-------------------------------------------------*
           add       1                    to   w-not-cli-inx-ele      .
           if        w-not-cli-inx-ele    >    w-not-cli-ctr-ele
                     go to not-900.
           if        w-not-cli-inx-ele    >    w-not-cli-max-ele
                     go to not-900.
       not-400.
      *              *-------------------------------------------------*
      *              * Reperimento della annotazione in base al suo    *
      *              * codice diretto o generico                       *
      *              *-------------------------------------------------*
           perform   not-ann-000          thru not-ann-999            .
       not-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione note                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-not-cli-ctr-002      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero linee necessarie      *
      *                  *---------------------------------------------*
           move      11                   to   w-not-cli-ctr-001      .
           move      zero                 to   w-not-cli-num-lin      .
       not-520.
           subtract  1                    from w-not-cli-ctr-001      .
           if        w-not-cli-ctr-001    <    1
                     go to not-540.
           if        rf-dcx-ann-rig
                    (w-not-cli-ctr-001)   =    spaces
                     go to not-520.
           move      w-not-cli-ctr-001    to   w-not-cli-num-lin      .
       not-540.
      *                  *---------------------------------------------*
      *                  * Test sul numero di linee determinate        *
      *                  *---------------------------------------------*
           if        w-not-cli-num-lin    =    zero
                     go to not-900.
       not-600.
      *                  *---------------------------------------------*
      *                  * Costruzione box                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Determinazione eventuale fattore di     *
      *                      * sottrazione se raggiunto fine video     *
      *                      *-----------------------------------------*
           move      zero                 to   w-not-cli-num-lis      .
      *
           move      w-cod-cod-dcc-lin    to   w-not-cli-ctr-003      .
           add       4                    to   w-not-cli-ctr-003      .
           add       w-not-cli-num-lin    to   w-not-cli-ctr-003      .
      *
           if        w-not-cli-ctr-003    >    24
                     subtract  24         from w-not-cli-ctr-003
                                        giving w-not-cli-num-lis      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           subtract  3                    from v-pos                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lto                  .
           add       4                    to   v-lto                  .
           add       w-not-cli-num-lin    to   v-lto                  .
           subtract  w-not-cli-num-lis    from v-lto                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pto                  .
           add       40                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Separazione prompt nel box              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           add       2                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           subtract  1                    from v-pos                  .
      *
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           add       5                    to   v-pos                  .
      *
           move      "Digitare 'S' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-610.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe annotazione           *
      *                  *---------------------------------------------*
           move      zero                 to   w-not-cli-ctr-001      .
       not-620.
           add       1                    to   w-not-cli-ctr-001      .
           if        w-not-cli-ctr-001    >    w-not-cli-num-lin
                     go to not-630.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           add       1                    to   v-lin                  .
           add       w-not-cli-ctr-001    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           subtract  1                    from v-pos                  .
           move      rf-dcx-ann-rig
                    (w-not-cli-ctr-001)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     not-620.
       not-630.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-640.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione pre presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
      *
           move      w-cod-cod-dcc-lin    to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-not-cli-num-lin    to   v-lin                  .
           subtract  w-not-cli-num-lis    from v-lin                  .
      *
           move      w-cod-cod-dcc-pos    to   v-pos                  .
           add       38                   to   v-pos                  .
      *
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione per presa visione                  *
      *              *-------------------------------------------------*
           if        v-alf                not  = "S"
                     go to not-640.
       not-700.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       not-800.
      *              *-------------------------------------------------*
      *              * Riciclo a nota successiva                       *
      *              *-------------------------------------------------*
           go to     not-200.
       not-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-999.
       not-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *                                                           *
      *    * Bufferizzazione                                           *
      *    *-----------------------------------------------------------*
       not-buf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-not-cli-ctr-ele      .
       not-buf-100.
      *              *-------------------------------------------------*
      *              * Start su [dcx] per ricerca eventuali note       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CLIPRG    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-cod-cod-dcc-cli    to   rf-dcx-cod-cli         .
           move      w-not-cli-dpz-str    to   rf-dcx-dpz-cli         .
           move      w-cod-cod-dcc-prg    to   rf-dcx-cod-prg         .
           move      zero                 to   rf-dcx-cod-ann         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito start                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-buf-900.
       not-buf-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale [dcx]                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Test su esito lettura                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to not-buf-900.
       not-buf-300.
      *              *-------------------------------------------------*
      *              * Test max su [dcx]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-cli       not  = w-cod-cod-dcc-cli
                     go to not-buf-900.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza cliente           *
      *                  *---------------------------------------------*
           if        rf-dcx-dpz-cli       not  = w-not-cli-dpz-str
                     go to not-buf-900.
      *                  *---------------------------------------------*
      *                  * Test su codice programma                    *
      *                  *---------------------------------------------*
           if        rf-dcx-cod-prg       not  = w-cod-cod-dcc-prg
                     go to not-buf-900.
       not-buf-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [dcx]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data attuale                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su eventuale data iniziale             *
      *                  *---------------------------------------------*
           if        rf-dcx-dat-ini       =    zero
                     go to not-buf-420.
           if        rf-dcx-dat-ini       not  < s-dat
                     go to not-buf-200.
       not-buf-420.
      *                  *---------------------------------------------*
      *                  * Test su eventuale data finale               *
      *                  *---------------------------------------------*
           if        rf-dcx-dat-fin       =    zero
                     go to not-buf-600.
           if        rf-dcx-dat-fin       <    s-dat
                     go to not-buf-200.
       not-buf-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           add       1                    to   w-not-cli-ctr-ele      .
           if        w-not-cli-ctr-ele    >    w-not-cli-max-ele
                     go to not-buf-900.
           move      w-not-cli-dpz-str    to   w-not-cli-cod-dpz
                                              (w-not-cli-ctr-ele)     .
           move      rf-dcx-cod-ann       to   w-not-cli-cod-ann
                                              (w-not-cli-ctr-ele)     .
           if        rf-dcx-cod-ang       not  numeric
                     move  zero           to   rf-dcx-cod-ang         .
           move      rf-dcx-cod-ang       to   w-not-cli-cod-ang
                                              (w-not-cli-ctr-ele)     .
       not-buf-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     not-buf-200.
       not-buf-900.
      *              *-------------------------------------------------*
      *              * Test se necessaria una seconda start            *
      *              *-------------------------------------------------*
           if        w-not-cli-dpz-str    not  = "*   "
                     move  "*   "         to   w-not-cli-dpz-str
                     go to not-buf-100
           else      go to not-buf-999.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-buf-999.
       not-buf-999.
           exit.

      *    *===========================================================*
      *    * Annotazioni cliente                                       *
      *    *                                                           *
      *    * Reperimento annotazione                                   *
      *    *-----------------------------------------------------------*
       not-ann-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della presenza di un     *
      *              * codice annotazione generica                     *
      *              *-------------------------------------------------*
           if        w-not-cli-cod-ang
                    (w-not-cli-inx-ele)   =    zero
                     go to not-ann-500.
       not-ann-100.
      *              *-------------------------------------------------*
      *              * Annotazione generica                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcx]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcx]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      zero                 to   rf-dcx-cod-cli         .
           move      spaces               to   rf-dcx-dpz-cli         .
           move      w-not-cli-cod-ang
                    (w-not-cli-inx-ele)   to   rf-dcx-cod-ann         .
           move      spaces               to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     not-ann-900.
       not-ann-500.
      *              *-------------------------------------------------*
      *              * Annotazione specifica                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione [dcx]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Lettura [dcx]                               *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CLIANN    "         to   f-key                  .
           move      w-cod-cod-dcc-cli    to   rf-dcx-cod-cli         .
           move      w-not-cli-cod-dpz
                    (w-not-cli-inx-ele)   to   rf-dcx-dpz-cli         .
           move      w-not-cli-cod-ann
                    (w-not-cli-inx-ele)   to   rf-dcx-cod-ann         .
           move      w-cod-cod-dcc-prg    to   rf-dcx-cod-prg         .
           move      "pgm/dcc/fls/ioc/obj/iofdcx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcx                 .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     not-ann-900.
       not-ann-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     not-ann-999.
       not-ann-999.
           exit.


