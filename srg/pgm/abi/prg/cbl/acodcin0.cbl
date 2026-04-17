       Identification Division.
       Program-Id.                                 acodcin0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    abi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 16/11/94    *
      *                       Ultima revisione:    NdK del 30/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo accettazione codice CIN italiano     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "AC"  Inizio accettazione                                      *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "AC"                 *
      *                                                                *
      *                       w-cod-cod-cin-abi : codice ABI           *
      *                                                                *
      *                       w-cod-cod-cin-cab : codice CAB           *
      *                                                                *
      *                       w-cod-cod-cin-ccc : codice C/C           *
      *                                                                *
      *                       w-cod-cod-cin-cod : codice CIN           *
      *                                                                *
      *                       w-cod-cod-cin-lin : linea codice         *
      *                                                                *
      *                       w-cod-cod-cin-pos : posizione codice     *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-ope : "A+"                 *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "A+"  Continuazione accettazione                               *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "A+"                 *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-ope : "A+" = continuare    *
      *                                           "AC" = completata    *
      *                                                                *
      *                       w-cod-cod-cin-cod : codice CIN           *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "VC"  Verifica di congruenza sul codice CIN                    *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "VC"                 *
      *                                                                *
      *                       w-cod-cod-cin-abi : codice ABI           *
      *                                                                *
      *                       w-cod-cod-cin-cab : codice CAB           *
      *                                                                *
      *                       w-cod-cod-cin-ccc : codice C/C           *
      *                                                                *
      *                       w-cod-cod-cin-cod : codice CIN           *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-ope : "VC" = Test Ok       *
      *                                           "##" = Test Ko       *
      *                                                                *
      *                       w-cod-cod-cin-cod : Codice CIN congruen- *
      *                                           te determinato       *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "VE"  Verifica di congruenza sul codice CIN europeo            *
      *       (si tratta del Check Digit del codice IBAN)              *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "VE"                 *
      *                                                                *
      *                       w-cod-cod-cin-naz : codice Nazione       *
      *                                                                *
      *                       w-cod-cod-cin-abi : codice ABI           *
      *                                                                *
      *                       w-cod-cod-cin-cab : codice CAB           *
      *                                                                *
      *                       w-cod-cod-cin-ccc : codice C/C           *
      *                                                                *
      *                       w-cod-cod-cin-cod : codice CIN           *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-ope : "VE" = Test Ok       *
      *                                           "##" = Test Ko       *
      *                                                                *
      *                       w-cod-cod-cin-chd : Codice CIN europeo   *
      *                                           (o Check Digit)      *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "EI"  Editing IBAN                                             *
      *                                                                *
      *              Input  : w-cod-cod-cin-ope : "EI"                 *
      *                                                                *
      *                       w-cod-cod-cin-naz : codice Nazione       *
      *                                                                *
      *                       w-cod-cod-cin-chd : codice Check         *
      *                                                                *
      *                       w-cod-cod-cin-cod : codice CIN           *
      *                                                                *
      *                       w-cod-cod-cin-abi : codice ABI           *
      *                                                                *
      *                       w-cod-cod-cin-cab : codice CAB           *
      *                                                                *
      *                       w-cod-cod-cin-ccc : codice C/C           *
      *                                                                *
      *                                                                *
      *              Output : w-cod-cod-cin-edt : IBAN editato         *
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
      *    * Work locale                                               *
      *    *-----------------------------------------------------------*
       01  w-aux-cod-cin.
           05  w-aux-cod-cin-inx          pic  9(02)                  .
           05  w-aux-cod-cin-tot          pic  9(04)                  .
           05  w-aux-cod-cin-div          pic  9(04)                  .
           05  w-aux-cod-cin-rem          pic  9(02)                  .
           05  w-aux-cod-cin-cin          pic  x(01)                  .
           05  w-aux-cod-cin-ax0.
               10  w-aux-cod-cin-ax1      pic  9(05)                  .
               10  w-aux-cod-cin-ax2      pic  9(05)                  .
               10  w-aux-cod-cin-ax3      pic  x(12)                  .
           05  w-aux-cod-cin-ay0 redefines
               w-aux-cod-cin-ax0.
               10  w-aux-cod-cin-ay1
                               occurs 22  pic  x(01)                  .
           05  w-aux-cod-cin-cd0          pic  x(27)                  .
           05  w-aux-cod-cin-cd1 redefines
               w-aux-cod-cin-cd0.
               10  w-aux-cod-cin-cda      pic  x(02)                  .
               10  w-aux-cod-cin-cdb      pic  x(02)                  .
               10  w-aux-cod-cin-cdc      pic  x(01)                  .
               10  w-aux-cod-cin-cdd      pic  9(05)                  .
               10  w-aux-cod-cin-cde      pic  9(05)                  .
               10  w-aux-cod-cin-cdf      pic  x(12)                  .
           05  w-aux-cod-cin-cd3 redefines
               w-aux-cod-cin-cd0.
               10  w-aux-cod-cin-cdx
                               occurs 27  pic  x(01)                  .
           05  w-aux-cod-cin-cwk          pic  x(41)                  .
           05  w-aux-cod-cin-chk          pic  9(02)                  .
           05  w-aux-cod-cin-nch          pic  9(02)                  .
           05  w-aux-cod-cin-m97          pic  9(02)       value 97   .
           05  w-aux-cod-cin-m98          pic  9(02)       value 98   .
           05  w-aux-cod-cin-num          pic  9(03)                  .
           05  w-aux-cod-cin-res          pic  9(03)                  .
           05  w-aux-cod-cin-max          pic  9(02)                  .
           05  w-aux-cod-cin-wkv          pic  9(02)                  .
           05  w-aux-cod-cin-wkr          pic  9(03)                  .
           05  w-aux-cod-cin-wkw          pic  9(03)                  .
           05  w-aux-cod-cin-wkd          pic  9(03)                  .
           05  w-aux-cod-cin-chd          pic  x(02)                  .
           05  w-aux-cod-cin-chh redefines
               w-aux-cod-cin-chd.
               10  w-aux-cod-cin-chn      pic  9(02)                  .

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
      *    * Link-area per accettazione codice CIN                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acodcin0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-cod-cod-cin
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
           if        w-cod-cod-cin-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-cod-cod-cin-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-cod-cod-cin-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-cod-cod-cin-ope    =    "AC"
                     perform   acc-000    thru acc-999
           else if   w-cod-cod-cin-ope    =    "A+" or
                     w-cod-cod-cin-ope    =    "F+"
                     perform   aco-000    thru aco-999
           else if   w-cod-cod-cin-ope    =    "VC"
                     perform   ver-000    thru ver-999
           else if   w-cod-cod-cin-ope    =    "VE"
                     perform   vcd-000    thru vcd-999
           else if   w-cod-cod-cin-ope    =    "EI"
                     perform   edi-000    thru edi-999                .
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
                     move  spaces         to   w-cod-cod-cin-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Inizio accettazione                                       *
      *    *-----------------------------------------------------------*
       acc-000.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-abi    to   w-cod-cod-cin-s01      .
           move      w-cod-cod-cin-cab    to   w-cod-cod-cin-s02      .
           move      w-cod-cod-cin-ccc    to   w-cod-cod-cin-s03      .
           move      w-cod-cod-cin-cod    to   w-cod-cod-cin-s04      .
           move      v-edm                to   w-cod-cod-cin-edm      .
           move      v-ufk                to   w-cod-cod-cin-ufk      .
      *              *-------------------------------------------------*
      *              * Accettazione alfanumerica                       *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-cin-ufk    to   v-ufk                  .
           move      w-cod-cod-cin-lin    to   v-lin                  .
           move      w-cod-cod-cin-pos    to   v-pos                  .
           move      w-cod-cod-cin-s04    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Tipo operazione a : continuazione               *
      *              *-------------------------------------------------*
           move      "A+"                 to   w-cod-cod-cin-ope      .
       acc-999.
           exit.

      *    *===========================================================*
      *    * Continuazione accettazione                                *
      *    *-----------------------------------------------------------*
       aco-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di rientro        *
      *              *-------------------------------------------------*
           if        w-cod-cod-cin-ope    =    "A+"
                     go to aco-200
           else if   w-cod-cod-cin-ope    =    "F+"
                     go to aco-400
           else      go to aco-200.
       aco-200.
      *              *-------------------------------------------------*
      *              * Se rientro 'A+', da impostazione normale        *
      *              *-------------------------------------------------*
       aco-210.
      *                  *---------------------------------------------*
      *                  * Tipo operazione a: non-continuazione        *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-cod-cin-ope      .
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di salvataggio     *
      *                  *---------------------------------------------*
           move      v-alf                to   w-cod-cod-cin-s04      .
       aco-220.
      *                  *---------------------------------------------*
      *                  * Se Exit o Delt: non si esegue la rivisua-   *
      *                  * lizzazione del valore impostato             *
      *                  *---------------------------------------------*
           if        v-key                not  = "EXIT" and
                     v-key                not  = "DELT"
                     go to aco-240.
       aco-230.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione del valore impostato      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-cin-lin    to   v-lin                  .
           move      w-cod-cod-cin-pos    to   v-pos                  .
           move      w-cod-cod-cin-s04    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-240.
      *                  *---------------------------------------------*
      *                  * Valore impostato in area di uscita          *
      *                  *---------------------------------------------*
           move      w-cod-cod-cin-s04    to   w-cod-cod-cin-cod      .
       aco-250.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-400.
      *              *-------------------------------------------------*
      *              * Se rientro 'F+', da digitazione tasto Find      *
      *              *-------------------------------------------------*
       aco-410.
      *                  *---------------------------------------------*
      *                  * Tipo operazione a : non-continuazione       *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-cod-cin-ope      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       aco-420.
      *                  *---------------------------------------------*
      *                  * Determinazione automatica del valore del    *
      *                  * codice CIN in funzione di :                 *
      *                  *  - Codice ABI                               *
      *                  *  - Codice CAB                               *
      *                  *  - Codice C/C                               *
      *                  *---------------------------------------------*
           perform   det-cod-cin-000      thru det-cod-cin-999        .
       aco-430.
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione del valore determinato    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      w-cod-cod-cin-lin    to   v-lin                  .
           move      w-cod-cod-cin-pos    to   v-pos                  .
           move      w-cod-cod-cin-s04    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       aco-440.
      *                  *---------------------------------------------*
      *                  * Valore determinato in area di uscita        *
      *                  *---------------------------------------------*
           move      w-cod-cod-cin-s04    to   w-cod-cod-cin-cod      .
       aco-450.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aco-999.
       aco-999.
           exit.

      *    *===========================================================*
      *    * Verifica di congruenza del codice CIN                     *
      *    *-----------------------------------------------------------*
       ver-000.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-abi    to   w-cod-cod-cin-s01      .
           move      w-cod-cod-cin-cab    to   w-cod-cod-cin-s02      .
           move      w-cod-cod-cin-ccc    to   w-cod-cod-cin-s03      .
           move      w-cod-cod-cin-cod    to   w-cod-cod-cin-s04      .
      *              *-------------------------------------------------*
      *              * Determinazione automatica del valore del codice *
      *              * CIN in funzione di :                            *
      *              *  - Codice ABI                                   *
      *              *  - Codice CAB                                   *
      *              *  - Codice C/C                                   *
      *              *-------------------------------------------------*
           perform   det-cod-cin-000      thru det-cod-cin-999        .
      *              *-------------------------------------------------*
      *              * Se il codice CIN passato e' congruente con il   *
      *              * codice CIN determinato : uscita immediata       *
      *              *-------------------------------------------------*
           if        w-cod-cod-cin-s04    =    w-cod-cod-cin-cod
                     go to ver-999.
      *              *-------------------------------------------------*
      *              * Preparazione tipo operazione ad errore          *
      *              *-------------------------------------------------*
           move      "##"                 to   w-cod-cod-cin-ope      .
      *              *-------------------------------------------------*
      *              * Preparazione codice CIN determinato             *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-s04    to   w-cod-cod-cin-cod      .
       ver-999.
           exit.

      *    *===========================================================*
      *    * Editing codice IBAN                                       *
      *    *-----------------------------------------------------------*
       edi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cod-cod-cin-edt      .
       edi-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se manca anche solo uno dei seguenti ele-   *
      *                  * menti :                                     *
      *                  *  - Codice ABI                               *
      *                  *  - Codice CAB                               *
      *                  *  - Codice C/C                               *
      *                  *  - CIN italiano                             *
      *                  *  - CIN europeo                              *
      *                  * si normalizza il valore                     *
      *                  *---------------------------------------------*
           if        w-cod-cod-cin-abi    =    zero   or
                     w-cod-cod-cin-cab    =    zero   or
                     w-cod-cod-cin-ccc    =    spaces or
                     w-cod-cod-cin-cod    =    spaces or
                     w-cod-cod-cin-chd    =    spaces
                     go to edi-900.
       edi-200.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-cod-cod-cin-naz    to   w-all-str-cat (1)      .
           move      w-cod-cod-cin-chd    to   w-all-str-cat (2)      .
           move      w-cod-cod-cin-cod    to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Editing ABI                                     *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-cod-cod-cin-abi    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (2)      .
      *              *-------------------------------------------------*
      *              * Editing CAB                                     *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-cod-cod-cin-cab    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (3)      .
      *              *-------------------------------------------------*
      *              * Completamento                                   *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-cod-cod-cin-ccc    to   w-all-str-cat (4)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Valore editato                                  *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-cod-cod-cin-edt      .
       edi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     edi-999.
       edi-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di determinazione automatica del valore del    *
      *    * codice CIN                                                *
      *    *                                                           *
      *    * Input  : w-cod-cod-cin-s01 = Codice ABI                   *
      *    *          w-cod-cod-cin-s02 = Codice CAB                   *
      *    *          w-cod-cod-cin-s03 = Codice C/C                   *
      *    *                                                           *
      *    * Output : w-cod-cod-cin-s04 = Codice CIN determinato       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-cod-cin-000.
      *              *-------------------------------------------------*
      *              * Preparazione campo unico di 22 caratteri, for-  *
      *              * mato da:                                        *
      *              *  - Codice ABI                                   *
      *              *  - Codice CAB                                   *
      *              *  - Codice C/C                                   *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-s01    to   w-aux-cod-cin-ax1      .
           move      w-cod-cod-cin-s02    to   w-aux-cod-cin-ax2      .
           move      w-cod-cod-cin-s03    to   w-aux-cod-cin-ax3      .
      *              *-------------------------------------------------*
      *              * Totalizzatore a zero                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-cod-cin-tot      .
       det-cod-cin-100.
      *              *-------------------------------------------------*
      *              * Totalizzatore caratteri con posizione dispari   *
      *              *-------------------------------------------------*
       det-cod-cin-110.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice                     *
      *                  *---------------------------------------------*
           move      01                   to   w-aux-cod-cin-inx      .
       det-cod-cin-120.
      *                  *---------------------------------------------*
      *                  * Sommatoria del carattere                    *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "0"
                     add   01             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "1"
                     add   00             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "2"
                     add   05             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "3"
                     add   07             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "4"
                     add   09             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "5"
                     add   13             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "6"
                     add   15             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "7"
                     add   17             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "8"
                     add   19             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "9"
                     add   21             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "A"
                     add   01             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "B"
                     add   00             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "C"
                     add   05             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "D"
                     add   07             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "E"
                     add   09             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "F"
                     add   13             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "G"
                     add   15             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "H"
                     add   17             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "I"
                     add   19             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "J"
                     add   21             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "K"
                     add   02             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "L"
                     add   04             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "M"
                     add   18             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "N"
                     add   20             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "O"
                     add   11             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "P"
                     add   03             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Q"
                     add   06             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "R"
                     add   08             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "S"
                     add   12             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "T"
                     add   14             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "U"
                     add   16             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "V"
                     add   10             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "W"
                     add   22             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "X"
                     add   25             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Y"
                     add   24             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Z"
                     add   23             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "-"
                     add   27             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "."
                     add   28             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    " "
                     add   26             to   w-aux-cod-cin-tot
                     go to det-cod-cin-130.
           add       26                   to   w-aux-cod-cin-tot      .
       det-cod-cin-130.
      *                  *---------------------------------------------*
      *                  * Incremento indice                           *
      *                  *---------------------------------------------*
           add       02                   to   w-aux-cod-cin-inx      .
      *                  *---------------------------------------------*
      *                  * Se posizione non maggiore di 22 si ricicla  *
      *                  * per la totalizzazione                       *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-inx    not  > 22
                     go to det-cod-cin-120.
       det-cod-cin-200.
      *              *-------------------------------------------------*
      *              * Totalizzatore caratteri con posizione pari      *
      *              *-------------------------------------------------*
       det-cod-cin-210.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice                     *
      *                  *---------------------------------------------*
           move      02                   to   w-aux-cod-cin-inx      .
       det-cod-cin-220.
      *                  *---------------------------------------------*
      *                  * Sommatoria del carattere                    *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "0"
                     add   00             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "1"
                     add   01             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "2"
                     add   02             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "3"
                     add   03             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "4"
                     add   04             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "5"
                     add   05             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "6"
                     add   06             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "7"
                     add   07             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "8"
                     add   08             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "9"
                     add   09             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "A"
                     add   00             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "B"
                     add   01             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "C"
                     add   02             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "D"
                     add   03             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "E"
                     add   04             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "F"
                     add   05             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "G"
                     add   06             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "H"
                     add   07             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "I"
                     add   08             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "J"
                     add   09             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "K"
                     add   10             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "L"
                     add   11             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "M"
                     add   12             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "N"
                     add   13             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "O"
                     add   14             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "P"
                     add   15             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Q"
                     add   16             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "R"
                     add   17             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "S"
                     add   18             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "T"
                     add   19             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "U"
                     add   20             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "V"
                     add   21             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "W"
                     add   22             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "X"
                     add   23             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Y"
                     add   24             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "Z"
                     add   25             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "-"
                     add   26             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    "."
                     add   27             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           if        w-aux-cod-cin-ay1
                    (w-aux-cod-cin-inx)   =    " "
                     add   28             to   w-aux-cod-cin-tot
                     go to det-cod-cin-230.
           add       28                   to   w-aux-cod-cin-tot      .
       det-cod-cin-230.
      *                  *---------------------------------------------*
      *                  * Incremento indice                           *
      *                  *---------------------------------------------*
           add       02                   to   w-aux-cod-cin-inx      .
      *                  *---------------------------------------------*
      *                  * Se posizione non maggiore di 22 si ricicla  *
      *                  * per la totalizzazione                       *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-inx    not  > 22
                     go to det-cod-cin-220.
       det-cod-cin-300.
      *              *-------------------------------------------------*
      *              * Divisione del totalizzatore per 26, allo scopo  *
      *              * di determinare il resto della divisione         *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-cod-cin-rem      .
           divide    26                   into w-aux-cod-cin-tot
                                        giving w-aux-cod-cin-div
                                     remainder w-aux-cod-cin-rem      .
       det-cod-cin-400.
      *              *-------------------------------------------------*
      *              * Determinazione del codice CIN in base al valore *
      *              * del resto                                       *
      *              *-------------------------------------------------*
           if        w-aux-cod-cin-rem    =    00
                     move  "A"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    01
                     move  "B"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    02
                     move  "C"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    03
                     move  "D"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    04
                     move  "E"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    05
                     move  "F"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    06
                     move  "G"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    07
                     move  "H"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    08
                     move  "I"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    09
                     move  "J"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    10
                     move  "K"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    11
                     move  "L"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    12
                     move  "M"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    13
                     move  "N"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    14
                     move  "O"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    15
                     move  "P"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    16
                     move  "Q"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    17
                     move  "R"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    18
                     move  "S"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    19
                     move  "T"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    20
                     move  "U"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    21
                     move  "V"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    22
                     move  "W"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    23
                     move  "X"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    24
                     move  "Y"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
           if        w-aux-cod-cin-rem    =    25
                     move  "Z"            to   w-aux-cod-cin-cin
                     go to det-cod-cin-500.
       det-cod-cin-500.
      *              *-------------------------------------------------*
      *              * Codice CIN determinato in uscita                *
      *              *-------------------------------------------------*
           move      w-aux-cod-cin-cin    to   w-cod-cod-cin-s04      .
       det-cod-cin-999.
           exit.

      *    *===========================================================*
      *    * Verifica di congruenza del codice CIN europeo             *
      *    *-----------------------------------------------------------*
       vcd-000.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri significativi originali   *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-abi    to   w-cod-cod-cin-s01      .
           move      w-cod-cod-cin-cab    to   w-cod-cod-cin-s02      .
           move      w-cod-cod-cin-ccc    to   w-cod-cod-cin-s03      .
           move      w-cod-cod-cin-cod    to   w-cod-cod-cin-s04      .
           move      w-cod-cod-cin-naz    to   w-cod-cod-cin-s05      .
           move      w-cod-cod-cin-chd    to   w-cod-cod-cin-s06      .
      *              *-------------------------------------------------*
      *              * Determinazione automatica del valore del codice *
      *              * CIN in funzione di :                            *
      *              *                                                 *
      *              *  - Codice ABI                                   *
      *              *  - Codice CAB                                   *
      *              *  - Codice C/C                                   *
      *              *  - Codice CIN italiano                          *
      *              *  - Codice Nazione                               *
      *              *-------------------------------------------------*
           perform   det-chd-cin-000      thru det-chd-cin-999        .
      *              *-------------------------------------------------*
      *              * Se il codice CIN passato e' congruente con il   *
      *              * codice CIN determinato : uscita immediata       *
      *              *-------------------------------------------------*
           if        w-cod-cod-cin-s06    =    w-cod-cod-cin-chd
                     go to vcd-999.
      *              *-------------------------------------------------*
      *              * Preparazione tipo operazione ad errore          *
      *              *-------------------------------------------------*
           move      "##"                 to   w-cod-cod-cin-ope      .
      *              *-------------------------------------------------*
      *              * Preparazione codice CIN determinato             *
      *              *-------------------------------------------------*
           move      w-cod-cod-cin-s06    to   w-cod-cod-cin-chd      .
       vcd-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di determinazione automatica del valore del    *
      *    * codice CIN europeo per la composizione dell'IBAN          *
      *    *                                                           *
      *    * Input  : w-cod-cod-cin-s01 = Codice ABI                   *
      *    *          w-cod-cod-cin-s02 = Codice CAB                   *
      *    *          w-cod-cod-cin-s03 = Codice C/C                   *
      *    *          w-cod-cod-cin-s04 = Codice CIN italiano          *
      *    *          w-cod-cod-cin-s05 = Codice Nazione               *
      *    *                                                           *
      *    * Output : w-cod-cod-cin-s06 = Codice CIN determinato       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-chd-cin-000.
      *              *-------------------------------------------------*
      *              * Operazioni di controllo preliminare             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se contiene spazi                      *
      *                  *---------------------------------------------*
           if        w-cod-cod-cin-s03    =    spaces
                     go to det-chd-cin-050.
           move      w-cod-cod-cin-s03    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           if        w-all-str-lun        =    12
                     go to det-chd-cin-050.
           move      12                   to   w-all-str-lun          .
           move      w-cod-cod-cin-s03    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
           move      w-all-str-alf        to   w-cod-cod-cin-s03      .
           inspect   w-cod-cod-cin-s03 
                                   replacing  all " "
                                          by  "0"                     .
       det-chd-cin-050.
      *              *-------------------------------------------------*
      *              * Preparazione campo unico di 27 caratteri, for-  *
      *              * mato da:                                        *
      *              *                                                 *
      *              *  - Codice CIN italiano                          *
      *              *  - Codice ABI                                   *
      *              *  - Codice CAB                                   *
      *              *  - Codice C/C                                   *
      *              *  - Codice Nazione                               *
      *              *  - Codice CIN europeo                           *
      *              *                                                 *
      *              * N.B.: i primi 4 caratteri vengono posti in      *
      *              *       fondo alla stringa                        *
      *              *-------------------------------------------------*
           move      27                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      w-cod-cod-cin-s04    to   w-all-str-cat (1)      .
           move      w-cod-cod-cin-s01    to   w-all-str-cat (2)      .
           move      w-cod-cod-cin-s02    to   w-all-str-cat (3)      .
           move      w-cod-cod-cin-s03    to   w-all-str-cat (4)      .
           move      w-cod-cod-cin-s05    to   w-all-str-cat (5)      .
           move      w-cod-cod-cin-s06    to   w-all-str-cat (6)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-aux-cod-cin-cd0      .
       det-chd-cin-100.
      *              *-------------------------------------------------*
      *              * Conversione delle lettere alfabetiche, in nume- *
      *              * ri                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-aux-cod-cin-cwk      .
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-cod-cin-inx      .
       det-chd-cin-120.
           add       1                    to   w-aux-cod-cin-inx      .
           if        w-aux-cod-cin-inx    >    27
                     go to det-chd-cin-200.
      *                  *---------------------------------------------*
      *                  * Test se numerico o alfanumerico             *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   numeric
                     go to det-chd-cin-160.
       det-chd-cin-140.
      *                  *---------------------------------------------*
      *                  * Carattere alfanumerico                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Conversione                             *
      *                      *                                         *
      *                      * Il criterio si basa sulla sequenza:     *
      *                      *                                         *
      *                      * (01234567890123456789012345)            *
      *                      *  ABCDEFGHIJKLMNOPQRSTUVWXYZ             *
      *                      *                                         *
      *                      * Le lettere alfabetiche, vengono tradot- *
      *                      * te in numeri, in base alla posizione    *
      *                      * nella sequenza, aumentata di 10.        *
      *                      * Ad esempio, la lettera 'A' produce il   *
      *                      * risultato '10' (0 + 10), la 'B', '11' e *
      *                      * cosi' via.                              *
      *                      *-----------------------------------------*
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "A"
                     move  "10"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "B"
                     move  "11"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "C"
                     move  "12"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "D"
                     move  "13"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "E"
                     move  "14"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "F"
                     move  "15"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "G"
                     move  "16"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "H"
                     move  "17"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "I"
                     move  "18"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "J"
                     move  "19"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "K"
                     move  "20"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "L"
                     move  "21"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "M"
                     move  "22"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "N"
                     move  "23"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "O"
                     move  "24"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "P"
                     move  "25"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "Q"
                     move  "26"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "R"
                     move  "27"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "S"
                     move  "28"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "T"
                     move  "29"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "U"
                     move  "30"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "V"
                     move  "31"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "W"
                     move  "32"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "X"
                     move  "33"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "Y"
                     move  "34"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
           if        w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   =    "Z"
                     move  "35"           to   w-all-str-cat (2)
                     go to det-chd-cin-150.
      *                      *-----------------------------------------*
      *                      * Se non trovato                          *
      *                      *-----------------------------------------*
           move      "00"                 to   w-all-str-cat (2)      .
                     go to det-chd-cin-150.
       det-chd-cin-150.
      *                      *-----------------------------------------*
      *                      * Ad assemblaggio                         *
      *                      *-----------------------------------------*
           go to     det-chd-cin-170.
       det-chd-cin-160.
      *                  *---------------------------------------------*
      *                  * Carattere gia' numerico                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In campo per assemblaggio               *
      *                      *-----------------------------------------*
           move      w-aux-cod-cin-cdx
                    (w-aux-cod-cin-inx)   to   w-all-str-cat (2)      .
      *                      *-----------------------------------------*
      *                      * Ad assemblaggio                         *
      *                      *-----------------------------------------*
           go to     det-chd-cin-170.
       det-chd-cin-170.
      *                  *---------------------------------------------*
      *                  * Assemblaggio                                *
      *                  *---------------------------------------------*
           move      41                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-aux-cod-cin-cwk    to   w-all-str-cat (1)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-aux-cod-cin-cwk      .
       det-chd-cin-180.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-chd-cin-120.
       det-chd-cin-200.
      *              *-------------------------------------------------*
      *              * Applicazione modulo 97                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza stringa determina- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           move      w-aux-cod-cin-cwk    to   w-all-str-alf          .
           perform   all-str-lun-000      thru all-str-lun-999        .
           move      w-all-str-lun        to   w-aux-cod-cin-max      .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-cod-cin-chk      .
           move      zero                 to   w-aux-cod-cin-num      .
           move      zero                 to   w-aux-cod-cin-res      .
           move      01                   to   w-aux-cod-cin-nch      .
      *                  *---------------------------------------------*
      *                  * Ciclo di 'abbassamento'                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-cod-cin-inx      .
       det-chd-cin-300.
           add       1                    to   w-aux-cod-cin-inx      .
           if        w-aux-cod-cin-inx    >    w-aux-cod-cin-max
                     go to det-chd-cin-500.
      *                  *---------------------------------------------*
      *                  * Abbassamento di 1 carattere e conversione   *
      *                  *---------------------------------------------*
           perform   det-chd-cin-cnv-000  thru det-chd-cin-cnv-999    .
      *                  *---------------------------------------------*
      *                  * Confronto con modulo divisore               *
      *                  *---------------------------------------------*
           move      w-aux-cod-cin-res    to   w-aux-cod-cin-wkr      .
           add       w-aux-cod-cin-num    to   w-aux-cod-cin-wkr      .
           if        w-aux-cod-cin-wkr    not  < w-aux-cod-cin-m97
                     go to  det-chd-cin-340.
       det-chd-cin-320.
      *                  *---------------------------------------------*
      *                  * Minore del modulo                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Resto                                   *
      *                      *-----------------------------------------*
           move      w-aux-cod-cin-wkr    to   w-aux-cod-cin-res      .
      *                      *-----------------------------------------*
      *                      * A incremento resto                      *
      *                      *-----------------------------------------*
           go to     det-chd-cin-400.
       det-chd-cin-340.
      *                  *---------------------------------------------*
      *                  * Maggiore o uguale al modulo                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Divisione                               *
      *                      *-----------------------------------------*
           divide    w-aux-cod-cin-m97    into w-aux-cod-cin-wkr
                                        giving w-aux-cod-cin-wkv
                                     remainder w-aux-cod-cin-res      .
      *                      *-----------------------------------------*
      *                      * A incremento resto                      *
      *                      *-----------------------------------------*
           go to     det-chd-cin-400.
       det-chd-cin-400.
      *                  *---------------------------------------------*
      *                  * Test sul contatore                          *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-inx    =    w-aux-cod-cin-max
                     move  w-aux-cod-cin-res
                                          to   w-aux-cod-cin-chk      .
      *                  *---------------------------------------------*
      *                  * Incremento resto                            *
      *                  *---------------------------------------------*
           move      w-aux-cod-cin-res    to   w-aux-cod-cin-wkw      .
           multiply  10                   by   w-aux-cod-cin-wkw      .
           move      w-aux-cod-cin-wkw    to   w-aux-cod-cin-res      .
       det-chd-cin-420.
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     det-chd-cin-300.
       det-chd-cin-500.
      *              *-------------------------------------------------*
      *              * Test se verifica o determinazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-cod-cod-cin-chd    =    "00"
                     go to det-chd-cin-540.
       det-chd-cin-520.
      *                  *---------------------------------------------*
      *                  * Verifica                                    *
      *                  *---------------------------------------------*
           if        w-aux-cod-cin-chk    =    01
                     move  w-cod-cod-cin-chd
                                          to   w-cod-cod-cin-s06
           else      move  spaces         to   w-cod-cod-cin-s06      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-chd-cin-900.
       det-chd-cin-540.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      w-aux-cod-cin-m98    to   w-aux-cod-cin-chn      .
           subtract  w-aux-cod-cin-chk    from w-aux-cod-cin-chn      .
           move      w-aux-cod-cin-chd    to   w-cod-cod-cin-s06      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-chd-cin-900.
       det-chd-cin-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chd-cin-999.
       det-chd-cin-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di determinazione automatica del valore del    *
      *    * codice CIN europeo per la composizione dell'IBAN          *
      *    *                                                           *
      *    * Subroutine di conversione in numero                       *
      *    *-----------------------------------------------------------*
       det-chd-cin-cnv-000.
      *              *-------------------------------------------------*
      *              * Conversione in numero                           *
      *              *-------------------------------------------------*
           move      "CV"                 to   v-ope                  .
           move      w-aux-cod-cin-nch    to   v-car                  .
           move      w-aux-cod-cin-cwk
                    (w-aux-cod-cin-inx :
                     w-aux-cod-cin-nch)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-num                to   w-aux-cod-cin-num      .
       det-chd-cin-cnv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-chd-cin-cnv-999.
       det-chd-cin-cnv-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

