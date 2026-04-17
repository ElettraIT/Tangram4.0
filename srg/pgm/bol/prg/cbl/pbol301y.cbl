       Identification Division.
       Program-Id.                                 pbol301y           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/04/25    *
      *                       Ultima revisione:    NdK del 21/04/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo di espansione della bolla per        *
      *                    visualizzare i documenti collegati          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-esp-idv-bit-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-esp-idv-bit-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-esp-idv-bit-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-esp-idv-bit-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ES"  Espansione                                               *
      *                                                                *
      *              Input  : w-esp-idv-bit-ope : "ES"                 *
      *                                                                *
      *                       w-esp-idv-bit-prt : protocollo bolla     *
      *                       w-esp-idv-bit-num : numero documento     *
      *                       w-esp-idv-bit-dat : data documento       *
      *                       w-esp-idv-bit-tmo : tipo documento       *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       N.B.: L'operazione di espansione non salva la 'start'    *
      *             sulle righe bolla nel caso in cui venga effettuata *
      *             una interrogazione sulle righe [bir]               *
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
      *        * [idv]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfidv"                          .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .
      *        *-------------------------------------------------------*
      *        * Deviatori per l'expand                                *
      *        *-------------------------------------------------------*
           05  w-cnt-swc-esp.
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo a tutte le righe  *
      *            *---------------------------------------------------*
               10  w-cnt-swc-esp-gen     pic  9(01)       value zero .
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo alla singola riga *
      *            *---------------------------------------------------*
               10  w-cnt-swc-esp-rig     pic  9(01)       value zero .
      *            *---------------------------------------------------*
      *            * Deviatore per l'expand relativo al 'find' su pro- *
      *            * dotto                                             *
      *            *                                                   *
      *            *  - 'zero' : Reset                                 *
      *            *  - '1'    : Utilizzo                              *
      *            *  - '>1'   : Disattivo                             *
      *            *---------------------------------------------------*
               10  w-cnt-swc-fnd-pro     pic  9(05)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione documenti cor-   *
      *    * relati                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dirdven0.dtl"                   .

      *    *===========================================================*
      *    * Work per linea corpo a video                              *
      *    *-----------------------------------------------------------*
       01  w-lin.
      *        *-------------------------------------------------------*
      *        * Immagine di una riga di corpo su video                *
      *        *-------------------------------------------------------*
           05  w-lin-imm.
      *            *---------------------------------------------------*
      *            * Linee di display per ogni riga corpo in scroll    *
      *            *---------------------------------------------------*
               10  w-lin-imm-dsp.
                   15  w-lin-imm-dsp-lin occurs 01
                                          pic  x(78)                  .
      *            *---------------------------------------------------*
      *            * Riga corpo in scroll rappresentata linearmente    *
      *            *---------------------------------------------------*
               10  w-lin-imm-scr redefines
                   w-lin-imm-dsp.
      *                *-----------------------------------------------*
      *                * Tipo documento                                *
      *                *-----------------------------------------------*
                   15  w-lin-imm-tip-doc  pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * 1. separatore                                 *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr1  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo documento                    *
      *                *-----------------------------------------------*
                   15  w-lin-imm-des-doc  pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * 2. separatore                                 *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr2  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Numero documento                              *
      *                *-----------------------------------------------*
                   15  w-lin-imm-num-doc  pic  x(06)                  .
      *                *-----------------------------------------------*
      *                * 3. separatore                                 *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr3  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  w-lin-imm-dat-doc  pic  x(08)                  .
      *                *-----------------------------------------------*
      *                * 4. separatore                                 *
      *                *-----------------------------------------------*
                   15  w-lin-imm-seg-tr4  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Note documento                                *
      *                *-----------------------------------------------*
                   15  w-lin-imm-sts-doc  pic  x(20)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Work per trattamento numero documento                 *
      *        *-------------------------------------------------------*
           05  w-wrk-num-doc              pic  9(11)                  .
           05  w-wrk-num-doc-r            redefines
               w-wrk-num-doc                                          .
               10  w-wrk-ndo-saa          pic  9(03)                  .
               10  w-wrk-ndo-dpz          pic  9(02)                  .
               10  w-wrk-ndo-prg          pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per contatori generici                           *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(05)                  .
           05  w-wrk-ctr-002              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutine di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-aux.
      *        *-------------------------------------------------------*
      *        * Work per espansione record righe bolla                *
      *        *-------------------------------------------------------*
           05  w-aux-rec-bir.
               10  w-aux-rec-bir-rtr      pic  x(01)                  .
               10  w-aux-rec-bir-c01      pic  9(05)                  .
               10  w-aux-rec-bir-c02      pic  9(03)                  .
               10  w-aux-rec-bir-c03      pic  9(05)                  .
               10  w-aux-rec-bir-c04      pic  9(05)                  .
               10  w-aux-rec-bir-c05      pic  9(05)                  .
               10  w-aux-rec-bir-c06      pic  9(05)                  .
               10  w-aux-rec-bir-cix      pic  9(03)                  .
               10  w-aux-rec-bir-nli      pic  9(05)                  .
               10  w-aux-rec-bir-crb      pic  9(05)                  .
               10  w-aux-rec-bir-max      pic  9(05) value 300        .
               10  w-aux-rec-bir-cpb      pic  9(05)                  .
               10  w-aux-rec-bir-cpa      pic  9(05)                  .
               10  w-aux-rec-bir-buf
                               occurs 300.
                   15  w-aux-rec-bir-tip  pic  9(02)                  .
                   15  w-aux-rec-bir-tmo  pic  x(05)                  .
                   15  w-aux-rec-bir-prt  pic  9(11)                  .
                   15  w-aux-rec-bir-num  pic  9(11)                  .
                   15  w-aux-rec-bir-dat  pic  9(07)                  .
                   15  w-aux-rec-bir-sts  pic  x(20)                  .

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
      *    * Link-area per espansione su righe bolla                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/pbol301y.pgl"                   .

      ******************************************************************
       Procedure Division                using w-esp-idv-bit          .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-esp-idv-bit-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-esp-idv-bit-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-esp-idv-bit-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-esp-idv-bit-ope    =    "ES"
                     perform   esp-000    thru esp-999                .
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
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open modulo di determinazione documenti     *
      *                  * correlati                                   *
      *                  *---------------------------------------------*
           perform   det-ird-ven-opn-000  thru det-ird-ven-opn-999    .
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
       cls-120.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close modulo di determinazione documenti    *
      *                  * correlati                                   *
      *                  *---------------------------------------------*
           perform   det-ird-ven-cls-000  thru det-ird-ven-cls-999    .
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
                     move  spaces         to   w-esp-idv-bit-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe bolla                          *
      *    *-----------------------------------------------------------*
       esp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-rec-bir-crb      .
      *              *-------------------------------------------------*
      *              * Normalizzazione deviatore per 'find' su pro-    *
      *              * dotto                                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-swc-fnd-pro      .
       esp-100.
      *              *-------------------------------------------------*
      *              * Lettura buffer [idv]                            *
      *              *-------------------------------------------------*
           move      "D<"                 to   d-ird-ven-tip-ope      .
           move      21                   to   d-ird-ven-tip-dsr      .
           move      w-esp-idv-bit-prt    to   d-ird-ven-prt-dsr      .
           perform   det-ird-ven-cll-000  thru det-ird-ven-cll-999    .
       esp-200.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-aux-rec-bir-crb      .
      *              *-------------------------------------------------*
      *              * Test su numero documenti correlati              *
      *              *-------------------------------------------------*
           if        w-aux-rec-bir-crb    >    d-ird-ven-num-ele
                     move  d-ird-ven-num-ele
                                          to   w-aux-rec-bir-crb
                     go to esp-500.
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-aux-rec-bir-crb    >    w-aux-rec-bir-max
                     go to esp-500.
       esp-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga in corso di trattamento    *
      *              *-------------------------------------------------*
           move      d-ird-ven-tip-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-tip
                                              (w-aux-rec-bir-crb)     .
           move      d-ird-ven-tmo-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-tmo
                                              (w-aux-rec-bir-crb)     .
           move      d-ird-ven-prt-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-prt
                                              (w-aux-rec-bir-crb)     .
           move      d-ird-ven-num-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-num
                                              (w-aux-rec-bir-crb)     .
           move      d-ird-ven-dat-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-dat
                                              (w-aux-rec-bir-crb)     .
           move      d-ird-ven-sts-dtr
                    (w-aux-rec-bir-crb)   to   w-aux-rec-bir-sts
                                              (w-aux-rec-bir-crb)     .
       esp-430.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura                               *
      *              *-------------------------------------------------*
           go to     esp-200.
       esp-500.
      *              *-------------------------------------------------*
      *              * Controllo numero records letti con lo stesso    *
      *              * valore                                          *
      *              *-------------------------------------------------*
           if        w-aux-rec-bir-crb    =    zero
                     go to esp-999.
      *                  *---------------------------------------------*
      *                  * Determinazione numero pagine nel buffer     *
      *                  *---------------------------------------------*
           move      w-aux-rec-bir-crb    to   w-aux-rec-bir-cpb      .
           subtract  1                    from w-aux-rec-bir-cpb      .
           divide    6                    into w-aux-rec-bir-cpb      .
           add       1                    to   w-aux-rec-bir-cpb      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero record nel buffer   *
      *                  * attualmente trattato                        *
      *                  *---------------------------------------------*
           move      1                    to   w-aux-rec-bir-c01      .
       esp-520.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand                       *
      *              *-------------------------------------------------*
           perform   esp-box-000          thru esp-box-999            .
       esp-540.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente         *
      *              * il record attualmente trattato                  *
      *              *-------------------------------------------------*
           perform   esp-930              thru esp-959                .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-550.
      *              *-------------------------------------------------*
      *              * Determinazione numero linea a video             *
      *              *-------------------------------------------------*
           move      w-aux-rec-bir-c01    to   w-aux-rec-bir-nli      .
       esp-555.
           if        w-aux-rec-bir-nli    >    6
                     subtract  6          from w-aux-rec-bir-nli
                     go to esp-555.
      *                  *---------------------------------------------*
      *                  * Incremento numero linea a video per posi-   *
      *                  * zionamento verticale                        *
      *                  *---------------------------------------------*
           add       09                   to   w-aux-rec-bir-nli      .
       esp-560.
      *                  *---------------------------------------------*
      *                  * Espansione record attualmente trattato      *
      *                  *---------------------------------------------*
       esp-575.
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Se il programma chiamante ha passato in     *
      *                  * link il codice prodotto e questa e' la      *
      *                  * prima chiamata di espansione, si esegue una *
      *                  * ricerca precablata                          *
      *                  *---------------------------------------------*
       esp-577.
      *                  *---------------------------------------------*
      *                  * Accettazione del mark-point                 *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-aux-rec-bir-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-aux-rec-bir-c01    <    w-aux-rec-bir-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           if        w-aux-rec-bir-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-aux-rec-bir-cpa    <    w-aux-rec-bir-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           if        w-aux-rec-bir-cpa    >    1
                     move  "BACK"         to   v-pfk (09)             .
           if        w-aux-rec-bir-cpa    <    w-aux-rec-bir-cpb
                     move  "TAB "         to   v-pfk (10)             .
           move      "EXPD"               to   v-pfk (15)             .
           move      "[4] "               to   v-pfk (16)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aux-rec-bir-nli    to   v-lin                  .
           move      17                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-580.
           if        v-key                =    spaces or
                     v-key                =    "RTRN"
                     go to esp-582
           else if   v-key                =    "UP  "
                     go to esp-584
           else if   v-key                =    "DOWN"
                     go to esp-586
           else if   v-key                =    "EXIT" or
                     v-key                =    "[4] "
                     go to esp-602
           else if   v-key                =    "NXSC"
                     go to esp-592
           else if   v-key                =    "PRSC"
                     go to esp-594
           else if   v-key                =    "BACK"
                     go to esp-598
           else if   v-key                =    "TAB "
                     go to esp-600
           else if   v-key                =    "EXPD"
                     go to esp-606
           else if   v-key                =    "FIND"
                     go to esp-620
           else      go to esp-575.
       esp-582.
      *              *-------------------------------------------------*
      *              * Se Return                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inibito                                     *
      *                  *---------------------------------------------*
           go to     esp-575.
       esp-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decremento contatore                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-bir-c01      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video : la    *
      *                  * prima                                       *
      *                  *---------------------------------------------*
           if        w-aux-rec-bir-nli    =    10
                     go to esp-590
           else      go to esp-550.
       esp-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Eventuale incremento contatore              *
      *                  *---------------------------------------------*
           if        w-aux-rec-bir-c01    <    w-aux-rec-bir-crb
                     add   1              to   w-aux-rec-bir-c01
                     go to esp-588
           else      go to esp-575.
       esp-588.
      *                  *---------------------------------------------*
      *                  * Controllo su linea assoluta a video :       *
      *                  * l'ultima                                    *
      *                  *---------------------------------------------*
           if        w-aux-rec-bir-nli    =    15
                     go to esp-590
           else      go to esp-550.
       esp-590.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina video contenente     *
      *                  * il record attualmente trattato              *
      *                  *---------------------------------------------*
           perform   esp-930              thru esp-959                .
      *                  *---------------------------------------------*
      *                  * A determinazione numero linea               *
      *                  *---------------------------------------------*
           go to     esp-550.
       esp-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Incremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-bir-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Decremento del contatore per le pagine      *
      *                  *---------------------------------------------*
           subtract  1                    from w-aux-rec-bir-cpa      .
      *                  *---------------------------------------------*
      *                  * A trattamento contatore per le pagine       *
      *                  *---------------------------------------------*
           go to     esp-596.
       esp-596.
      *              *-------------------------------------------------*
      *              * Trattamento contatore per le pagine             *
      *              *-------------------------------------------------*
           move      w-aux-rec-bir-cpa    to   w-aux-rec-bir-c01      .
           multiply  6                    by   w-aux-rec-bir-c01      .
           subtract  5                    from w-aux-rec-bir-c01      .
           go to     esp-590.
       esp-598.
      *              *-------------------------------------------------*
      *              * Se Back                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-rec-bir-cpa      .
           go to     esp-596.
       esp-600.
      *              *-------------------------------------------------*
      *              * Se Tab                                          *
      *              *-------------------------------------------------*
           move      w-aux-rec-bir-cpb    to   w-aux-rec-bir-cpa      .
           go to     esp-596.
       esp-602.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione deviatore per Expand        *
      *                  *---------------------------------------------*
           move      0                    to   w-cnt-swc-esp-gen      .
      *                  *---------------------------------------------*
      *                  * Restore                                     *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     esp-800.
       esp-606.
      *              *-------------------------------------------------*
      *              * Se Expand                                       *
      *              *-------------------------------------------------*
       esp-620.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
       esp-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-999.
       esp-930.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-aux-rec-bir-c01    to   w-aux-rec-bir-c02      .
           add       5                    to   w-aux-rec-bir-c02      .
           divide    6                    into w-aux-rec-bir-c02      .
           move      w-aux-rec-bir-c02    to   w-aux-rec-bir-cpa      .
           subtract  1                    from w-aux-rec-bir-c02      .
           multiply  6                    by   w-aux-rec-bir-c02      .
           add       1                    to   w-aux-rec-bir-c02      .
           add       5
                     w-aux-rec-bir-c02  giving w-aux-rec-bir-c03      .
           move      w-aux-rec-bir-c03    to   w-aux-rec-bir-c04      .
           if        w-aux-rec-bir-c03    >    w-aux-rec-bir-crb
                     move  w-aux-rec-bir-crb
                                          to   w-aux-rec-bir-c03      .
      *                  *---------------------------------------------*
      *                  * Posizionamento iniziale                     *
      *                  *---------------------------------------------*
           move      10                   to   w-aux-rec-bir-c05      .
       esp-940.
      *                  *---------------------------------------------*
      *                  * Preparazione riga da visualizzare           *
      *                  *---------------------------------------------*
           perform   esp-doc-000          thru esp-doc-999            .
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-bir-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      w-lin-imm-dsp        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-bir-c02      .
           add       1                    to   w-aux-rec-bir-c05      .
           if        w-aux-rec-bir-c02    not  > w-aux-rec-bir-c03
                     go to esp-940.
       esp-957.
           if        w-aux-rec-bir-c02    >    w-aux-rec-bir-c04
                     go to esp-958.
           if        w-aux-rec-bir-crb    not  > 6
                     go to esp-958.
      *                  *---------------------------------------------*
      *                  * Riga vuota con separatori                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      w-aux-rec-bir-c05    to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-rec-bir-c02      .
           add       1                    to   w-aux-rec-bir-c05      .
           go to     esp-957.
       esp-958.
      *              *-------------------------------------------------*
      *              * Literal 'pagina'                                *
      *              *-------------------------------------------------*
           perform   esp-ldp-000          thru esp-ldp-999            .
       esp-959.
           exit.
       esp-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe bolla                          *
      *    *                                                           *
      *    * Subroutine per la lettura estremi documento collegato     *
      *    *-----------------------------------------------------------*
       esp-doc-000.
      *              *-------------------------------------------------*
      *              * Assemblaggio tipo documento                     *
      *              *-------------------------------------------------*
           move      14                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           if        w-aux-rec-bir-tip
                    (w-aux-rec-bir-c02)   =    21
                     move  spaces         to   w-all-str-cat (1)
           else if   w-aux-rec-bir-tip
                    (w-aux-rec-bir-c02)   =    11
                     move  "..."          to   w-all-str-cat (1)
           else      move  "......."      to   w-all-str-cat (1)      .
      *
           move      w-aux-rec-bir-tmo
                    (w-aux-rec-bir-c02)   to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-lin-imm-tip-doc      .
       esp-doc-100.
      *              *-------------------------------------------------*
      *              * Descrizione tipo documento                      *
      *              *-------------------------------------------------*
           if        w-aux-rec-bir-tip
                    (w-aux-rec-bir-c02)   =    21
                     move  "Documento di trasporto"
                                          to   w-lin-imm-des-doc
           else if   w-aux-rec-bir-tip
                    (w-aux-rec-bir-c02)   =    11
                     move  "Ordine di spedizione "
                                          to   w-lin-imm-des-doc
           else if   w-aux-rec-bir-tip
                    (w-aux-rec-bir-c02)   =    01
                     move  "Ordine cliente       "
                                          to   w-lin-imm-des-doc
           else      move  "???"          to   w-lin-imm-des-doc      .
       esp-doc-200.
      *              *-------------------------------------------------*
      *              * Editing numero documento                        *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-bir-num
                    (w-aux-rec-bir-c02)   to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           move      06                   to   w-all-str-lun          .
           move      v-edt                to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
           move      w-all-str-alf        to   w-lin-imm-num-doc      .
       esp-doc-300.
      *              *-------------------------------------------------*
      *              * Editing data documento                          *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-aux-rec-bir-dat
                    (w-aux-rec-bir-c02)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-lin-imm-dat-doc      .
       esp-doc-400.
      *              *-------------------------------------------------*
      *              * Status documento                                *
      *              *-------------------------------------------------*
           move      w-aux-rec-bir-sts
                    (w-aux-rec-bir-c02)   to   w-lin-imm-sts-doc      .
      *              *-------------------------------------------------*
      *              * Separatori                                      *
      *              *-------------------------------------------------*
           move      "|"                  to   w-lin-imm-seg-tr1      .
           move      "|"                  to   w-lin-imm-seg-tr2      .
           move      "|"                  to   w-lin-imm-seg-tr3      .
           move      "|"                  to   w-lin-imm-seg-tr4      .
       esp-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-doc-999.
       esp-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe bolla                          *
      *    *                                                           *
      *    * Subroutine per la costruzione del Box                     *
      *    *-----------------------------------------------------------*
       esp-box-000.
      *              *-------------------------------------------------*
      *              * Box per espansione                              *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Fincatura superiore                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      " Collegamenti |      Tipo documento     |Numero|  
      -              "Data  |         Note        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura superiore              *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "--------------+-------------------------+------+--
      -              "------+---------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Griglie                                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "              |                         |      |  
      -              "      |                     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura di chiusura                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      78                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      02                   to   v-pos                  .
           move      "--------------+-------------------------+------+--
      -              "------+---------------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Editing numero documento                        *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-esp-idv-bit-num    to   w-wrk-num-doc          .
           move      w-wrk-ndo-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-all-str-cat (4)      .
      *              *-------------------------------------------------*
      *              * Editing data documento                          *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-esp-idv-bit-dat    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (6)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      42                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      "Documento :"        to   w-all-str-cat (1)      .
           move      w-esp-idv-bit-tmo    to   w-all-str-cat (2)      .
           move      "nr."                to   w-all-str-cat (3)      .
           move      "del"                to   w-all-str-cat (5)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      42                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-box-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-box-999.
       esp-box-999.
           exit.

      *    *===========================================================*
      *    * Routine di Expand su righe bolla                          *
      *    *                                                           *
      *    * Subroutine per il literal di pagina                       *
      *    *-----------------------------------------------------------*
       esp-ldp-000.
      *              *-------------------------------------------------*
      *              * Editing 'numero pagina'                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-bir-cpa    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Editing 'di pagine'                             *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-aux-rec-bir-cpb    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (4)      .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      17                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "Pagina"             to   w-all-str-cat (1)      .
           move      "di"                 to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       esp-ldp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     esp-ldp-999.
       esp-ldp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione documenti correlati        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dirdven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


