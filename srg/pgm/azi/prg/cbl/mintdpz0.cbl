       Identification Division.
       Program-Id.                                 mintdpz0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/10/91    *
      *                       Ultima revisione:    NdK del 15/01/02    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo intestazione dipendenza su stampa    *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : s-ope : "OP"                             *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : s-ope : "CL"                             *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : s-ope : "C?"                             *
      *                                                                *
      *                                                                *
      *              Output : s-ope : spaces = Si                      *
      *                               "C?"   = No                      *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "ID"  Esecuzione intestazione per dipendenza su stampati       *
      *                                                                *
      *              Input  : s-ope : "ID"                             *
      *                                                                *
      *                       s-num : codice dipendenza in uso         *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "DD"  Ottenimento descrizione per dipendenza su stampati       *
      *                                                                *
      *              Input  : s-ope : "DD"                             *
      *                                                                *
      *                       s-num : codice dipendenza                *
      *                                                                *
      *                                                                *
      *              Output : s-alf : descrizione dipendenza           *
      *                                                                *
      *                       s-sts : status di uscita                 *
      *                               - Spaces : dipendenza esistente  *
      *                               - #      : non esistente         *
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
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)     value zero   .
      *        *-------------------------------------------------------*
      *        * Flag di determinazione numero dipendenze nell'azienda *
      *        * gia' eseguito                                         *
      *        * - Spaces : non ancora eseguito                        *
      *        * - #      : gia' eseguito                              *
      *        *-------------------------------------------------------*
           05  w-cnt-flg-dda              pic  x(01)     value spaces .
      *        *-------------------------------------------------------*
      *        * Numero dipendenze nell'azienda                        *
      *        * - 0 : Zero                                            *
      *        * - 1 : Una sola dipendenza                             *
      *        * - 2 : Piu' di una dipendenza                          *
      *        *-------------------------------------------------------*
           05  w-cnt-num-dda              pic  9(01)     value zero   .
      *        *-------------------------------------------------------*
      *        * Codice ultima dipendenza letta                        *
      *        *-------------------------------------------------------*
           05  w-cnt-cod-udl              pic  9(02)     value zero   .
      *        *-------------------------------------------------------*
      *        * Descrizione ultima dipendenza letta                   *
      *        *-------------------------------------------------------*
           05  w-cnt-des-udl              pic  x(20)     value spaces .
      *        *-------------------------------------------------------*
      *        * Flag lettura ultima dipendenza letta                  *
      *        * - Spaces : dipendenza esistente                       *
      *        * - #      : dipendenza non esistente                   *
      *        *-------------------------------------------------------*
           05  w-cnt-flg-udl              pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work-area per le personalizzazioni                        *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione per gestione dipendenze parallele   *
      *        *-------------------------------------------------------*
           05  w-prs-snx-gdp.
      *            *---------------------------------------------------*
      *            * Si/No gestione dipendenze parallele               *
      *            *---------------------------------------------------*
               10  w-prs-snx-gdp-snx      pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio codice dipendenza passato dal chiamante   *
      *        *-------------------------------------------------------*
           05  w-sav-cod-dpz              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per letture                                     *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Per lettura codice dipendenza                         *
      *        *-------------------------------------------------------*
           05  w-let-cod-dpz.
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-let-cod-dpz-cod          pic  9(02)              .
      *            *---------------------------------------------------*
      *            * Descrizione dipendenza                            *
      *            *---------------------------------------------------*
               10  w-let-cod-dpz-des          pic  x(20)              .
      *            *---------------------------------------------------*
      *            * Flag di lettura                                   *
      *            * - Spaces : dipendenza esistente                   *
      *            * - #      : dipendenza non esistente               *
      *            *---------------------------------------------------*
               10  w-let-cod-dpz-flg          pic  x(20)              .

      *    *===========================================================*
      *    * Work-area per literals                                    *
      *    *-----------------------------------------------------------*
       01  w-lit.
      *        *-------------------------------------------------------*
      *        * Per codice e descrizione dipendenza separati da ":"   *
      *        *-------------------------------------------------------*
           05  w-lit-dpz.
               10  w-lit-dpz-001.
                   15  w-lit-dpz-lit-dpz  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lit-dpz-cod-dpz  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lit-dpz-due-pun  pic  x(01)                  .
                   15  filler             pic  x(01)                  .
                   15  w-lit-dpz-des-dpz  pic  x(20)                  .
               10  w-lit-dpz-002 redefines
                   w-lit-dpz-001.
                   15  w-lit-dpz-chr-lit
                               occurs 36  pic  x(01)                  .
               10  w-lit-dpz-ctr-ncs      pic  9(02)                  .
               10  w-lit-dpz-pos-ini      pic  9(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      ******************************************************************
       Procedure Division                using s
                                               p                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        s-ope                =    "OP"
                     perform   opn-000    thru opn-999
           else if   s-ope                =    "CL"
                     perform   cls-000    thru cls-999
           else if   s-ope                =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   s-ope                =    "ID"
                     perform   int-000    thru int-999
           else if   s-ope                =    "DD"
                     perform   des-000    thru des-999                .
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
      *              * Lettura personalizzazione per Si/No gestione    *
      *              * dipendenze parallele, se necessario             *
      *              *-------------------------------------------------*
           perform   prs-snx-gdp-000      thru prs-snx-gdp-999        .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
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
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
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
                     move  spaces         to   s-ope                  .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per Si/No gestione dipendenze   *
      *    * parallele                                                 *
      *    *-----------------------------------------------------------*
       prs-snx-gdp-000.
      *              *-------------------------------------------------*
      *              * Test se lettura necessaria                      *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = spaces
                     go to prs-snx-gdp-999.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[snx-gdp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-gdp-snx
           else      move  spaces         to   w-prs-snx-gdp-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = "S"
                     move  "N"            to   w-prs-snx-gdp-snx      .
       prs-snx-gdp-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione intestazione per dipendenza su stampati        *
      *    *-----------------------------------------------------------*
       int-000.
      *              *-------------------------------------------------*
      *              * Salvataggio codice dipendenza passato dal chia- *
      *              * mante                                           *
      *              *-------------------------------------------------*
           move      s-num                to   w-sav-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Determinazione numero dipendenze esistenti nel- *
      *              * l'azienda se non gia' determinato               *
      *              *-------------------------------------------------*
           perform   det-dpz-azi-000      thru det-dpz-azi-999        .
      *              *-------------------------------------------------*
      *              * Lettura codice dipendenza per relativa descri-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      w-sav-cod-dpz        to   w-let-cod-dpz-cod      .
           perform   let-cod-dpz-000      thru let-cod-dpz-999        .
      *              *-------------------------------------------------*
      *              * Stampa intestazione per dipendenza              *
      *              *-------------------------------------------------*
           perform   stp-int-dpz-000      thru stp-int-dpz-999        .
       int-999.
           exit.

      *    *===========================================================*
      *    * Ottenimento descrizione per dipendenza su stampati        *
      *    *-----------------------------------------------------------*
       des-000.
      *              *-------------------------------------------------*
      *              * Salvataggio codice dipendenza passato dal chia- *
      *              * mante                                           *
      *              *-------------------------------------------------*
           move      s-num                to   w-sav-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Determinazione numero dipendenze esistenti nel- *
      *              * l'azienda se non gia' determinato               *
      *              *-------------------------------------------------*
           perform   det-dpz-azi-000      thru det-dpz-azi-999        .
      *              *-------------------------------------------------*
      *              * Lettura codice dipendenza per relativa descri-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      w-sav-cod-dpz        to   w-let-cod-dpz-cod      .
           perform   let-cod-dpz-000      thru let-cod-dpz-999        .
      *              *-------------------------------------------------*
      *              * Descrizione per codice dipendenza in area di u- *
      *              * scita                                           *
      *              *-------------------------------------------------*
           move      w-let-cod-dpz-des    to   s-alf                  .
      *              *-------------------------------------------------*
      *              * Flag di lettura codice dipendenza in area di u- *
      *              * scita                                           *
      *              *-------------------------------------------------*
           move      w-let-cod-dpz-flg    to   s-sts                  .
       des-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero dipendenze esistenti nell'azienda   *
      *    *-----------------------------------------------------------*
       det-dpz-azi-000.
      *              *-------------------------------------------------*
      *              * Se il numero dipendenze esistenti per l'azienda *
      *              * e' gia' stato determinato : uscita              *
      *              *-------------------------------------------------*
           if        w-cnt-flg-dda        not  = spaces
                     go to det-dpz-azi-999.
      *              *-------------------------------------------------*
      *              * Flag di determinazione eseguita in On           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-flg-dda          .
      *              *-------------------------------------------------*
      *              * Numero di dipendenze esistenti a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-num-dda          .
      *              *-------------------------------------------------*
      *              * Codice ultima dipendenza letta a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-cod-udl          .
      *              *-------------------------------------------------*
      *              * Descrizione ultima dipendenza letta a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-des-udl          .
      *              *-------------------------------------------------*
      *              * Flag lettura ultima dipendenza letta a spaces   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-flg-udl          .
       det-dpz-azi-200.
      *              *-------------------------------------------------*
      *              * Start su dipendenze dell'azienda                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-dpz-azi-999.
       det-dpz-azi-400.
      *              *-------------------------------------------------*
      *              * Next su tabella dipendenze                      *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-dpz-azi-999.
       det-dpz-azi-440.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *-------------------------------------------------*
           if        rf-ada-cod-dpz       <    01 or
                     rf-ada-cod-dpz       >    99
                     go to det-dpz-azi-400.
      *              *-------------------------------------------------*
      *              * Incremento numero dipendenze esistenti per l'a- *
      *              * zienda                                          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-num-dda          .
      *              *-------------------------------------------------*
      *              * Se non e' la prima dipendenza letta : uscita    *
      *              *-------------------------------------------------*
           if        w-cnt-num-dda        >    1
                     go to det-dpz-azi-999.
      *              *-------------------------------------------------*
      *              * Memorizzazione del codice e della descrizione   *
      *              * della prima dipendenza letta come ultimo codi-  *
      *              * ce e descrizione dipendenza letta               *
      *              *-------------------------------------------------*
           move      rf-ada-cod-dpz       to   w-cnt-cod-udl          .
           move      rf-ada-cod-mne       to   w-cnt-des-udl          .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura dipendenza successiva         *
      *              *-------------------------------------------------*
           go to     det-dpz-azi-400.
       det-dpz-azi-999.
           exit.

      *    *===========================================================*
      *    * Lettura codice dipendenza per relativa descrizione        *
      *    *-----------------------------------------------------------*
       let-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza da leggere a zero : uscita *
      *              * con descrizione e flag a spaces                 *
      *              *-------------------------------------------------*
           if        w-let-cod-dpz-cod    =    zero
                     move  spaces         to   w-let-cod-dpz-des
                     move  spaces         to   w-let-cod-dpz-flg
                     go to let-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza da leggere pari al codice  *
      *              * ultima dipendenza letto : uscita con la descri- *
      *              * zione bufferizzatae con il relativo flag        *
      *              *-------------------------------------------------*
           if        w-let-cod-dpz-cod    =    w-cnt-cod-udl
                     move  w-cnt-des-udl  to   w-let-cod-dpz-des
                     move  w-cnt-flg-udl  to   w-let-cod-dpz-flg
                     go to let-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Lettura archivio dipendenze                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ada]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dpz]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      w-let-cod-dpz-cod    to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-cod-dpz-600.
       let-cod-dpz-300.
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione dipendenza con il  *
      *                  * segnale di record non esistente             *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-cod-dpz-des      .
      *                  *---------------------------------------------*
      *                  * Preparazione flag di lettura a Ko           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-let-cod-dpz-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-cod-dpz-999.
       let-cod-dpz-600.
      *              *-------------------------------------------------*
      *              * Se dipendenza esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione descrizione dipendenza da ana- *
      *                  * grafica                                     *
      *                  *---------------------------------------------*
           move      rf-ada-cod-mne       to   w-let-cod-dpz-des      .
      *                  *---------------------------------------------*
      *                  * Preparazione flag di lettura a Ok           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cod-dpz-flg      .
       let-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Stampa intestazione per dipendenza                        *
      *    *-----------------------------------------------------------*
       stp-int-dpz-000.
      *              *-------------------------------------------------*
      *              * Se il numero dipendenze per l'azienda non e'    *
      *              * maggiore di 1 : uscita senza alcuna azione      *
      *              *-------------------------------------------------*
           if        w-cnt-num-dda        not  >    1
                     go to stp-int-dpz-999.
      *              *-------------------------------------------------*
      *              * Se il numero pagina e' maggiore di 1 : uscita   *
      *              * senza alcuna azione                             *
      *              *-------------------------------------------------*
           if        p-pag                >    1
                     go to stp-int-dpz-999.
       stp-int-dpz-100.
      *              *-------------------------------------------------*
      *              * Composizione del literal per la dipendenza e    *
      *              * determinazione del numero di caratteri signi-   *
      *              * ficativi del literal cosi' composto             *
      *              *-------------------------------------------------*
       stp-int-dpz-110.
      *                  *---------------------------------------------*
      *                  * Composizione del literal                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-lit-dpz-001          .
           move      "Dipendenza"         to   w-lit-dpz-lit-dpz      .
           move      w-sav-cod-dpz        to   w-lit-dpz-cod-dpz      .
           move      ":"                  to   w-lit-dpz-due-pun      .
           move      w-let-cod-dpz-des    to   w-lit-dpz-des-dpz      .
       stp-int-dpz-120.
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di caratteri      *
      *                  *---------------------------------------------*
           move      36                   to   w-lit-dpz-ctr-ncs      .
       stp-int-dpz-130.
           if        w-lit-dpz-chr-lit
                    (w-lit-dpz-ctr-ncs)   =    spaces
                     subtract  1          from w-lit-dpz-ctr-ncs
                     go to     stp-int-dpz-130.
       stp-int-dpz-200.
      *              *-------------------------------------------------*
      *              * Determinazione della posizione iniziale per la  *
      *              * stampa del literal della dipendenza             *
      *              *-------------------------------------------------*
           move      p-sel-als-sel        to   w-lit-dpz-pos-ini      .
           subtract  w-lit-dpz-ctr-ncs    from w-lit-dpz-pos-ini      .
           divide    2                    into w-lit-dpz-pos-ini      .
           add       1                    to   w-lit-dpz-pos-ini      .
       stp-int-dpz-500.
      *              *-------------------------------------------------*
      *              * Stampa effettiva                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lineette superiori                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-lit-dpz-ctr-ncs    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-lit-dpz-pos-ini    to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal vero e proprio                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-lit-dpz-ctr-ncs    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-lit-dpz-pos-ini    to   p-pos                  .
           move      w-lit-dpz-001        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Lineette inferiori                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-lit-dpz-ctr-ncs    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-lit-dpz-pos-ini    to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Posizionamento verticale fisso              *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-dpz-999.
           exit.
