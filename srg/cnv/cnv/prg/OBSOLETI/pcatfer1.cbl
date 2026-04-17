       Identification Division.
       Program-Id.                                 pcatfer1           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    fer                 *
      *                                   Fase:    catfer              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/05/95    *
      *                       Ultima revisione:    NdK del 28/09/98    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione per catalogo 'Archimede'        *
      *                                                                *
      *                    FERRAMENTA ITALIANA                         *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.	   N-d-K-Sia-PD .
       Object-Computer.	   N-d-K-Sia-PD .

       Special-Names.          Decimal-Point     Is Comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "fer"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "catfer"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcatfer1"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " Conversione archivi Catalogo ARCHIMEDE "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output su files *
      *    * di tipo line sequential                                   *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/g"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20       
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento conversione       *
      *        *-------------------------------------------------------*
           05  w-cnt-xcv.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-xcv-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-xcv-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-xcv-flg-sub      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hct]                                                 *
      *        *-------------------------------------------------------*
           copy      "fer/gpa/fls/rec/rfhct"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .

      *    *===========================================================*
      *    * Work-area richieste per conversione                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Pathname del file in input                            *
      *        *-------------------------------------------------------*
           05  rr-pth-inp                 pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo importazione                                     *
      *        *  - 'ART ' : Articoli                                  *
      *        *  - 'MART' : Meta-articoli                             *
      *        *  - 'SGRU' : Sottogruppi                               *
      *        *  - 'ASSO' : Associazione Sottogruppi - Meta-articoli  *
      *        *-------------------------------------------------------*
           05  rr-tip-imp                 pic  x(04)                  .

      *    *===========================================================*
      *    * Work area per la raccolta di una intera pagina di stampa  *
      *    *-----------------------------------------------------------*
       01  w-pds.
      *        *-------------------------------------------------------*
      *        * Numero di linee lette                                 *
      *        *-------------------------------------------------------*
           05  w-pds-nll                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero di records scritti [dcp]                       *
      *        *-------------------------------------------------------*
           05  w-pds-dcp                  pic  9(06)                  .

      *    *===========================================================*
      *    * Work area per il trattamento delle linee di stampa        *
      *    *-----------------------------------------------------------*
       01  w-lds.
      *        *-------------------------------------------------------*
      *        * Flag di fine file                                     *
      *        *-------------------------------------------------------*
           05  w-lds-fff                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Linea 01                                              *
      *        *-------------------------------------------------------*
           05  w-lds-001.
               10  filler    occurs 2048  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Aree di ridefinizione e di work                       *
      *        *-------------------------------------------------------*
           05  w-lds-rew.
      *            *---------------------------------------------------*
      *            * Codice prodotto                                   *
      *            *---------------------------------------------------*
               10  w-lds-rew-alf-pro      pic  x(05)                  .
               10  w-lds-rew-num-pro redefines
                   w-lds-rew-alf-pro      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Chiave numerica                                   *
      *            *---------------------------------------------------*
               10  w-lds-rew-key-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Dati aggiuntivi                                   *
      *            *---------------------------------------------------*
               10  w-lds-rew-des-uno      pic  x(60)                  .
               10  w-lds-rew-des-due      pic  x(60)                  .
               10  w-lds-rew-agg-uno      pic  9(07)                  .
               10  w-lds-rew-agg-due      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Categoria provvigione                                 *
      *        *-------------------------------------------------------*
           05  w-lds-rew-ctp.
               10  w-lds-rew-ctp-alf      pic  x(02)                  .
               10  w-lds-rew-ctp-num redefines
                   w-lds-rew-ctp-alf      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo importazione                          *
      *        *-------------------------------------------------------*
           05  w-exp-tip-imp.
               10  w-exp-tip-imp-num      pic  9(02)       value 04   .
               10  w-exp-tip-imp-lun      pic  9(02)       value 40   .
               10  w-exp-tip-imp-tbl.
                   15  filler             pic  x(40) value
                          "Articoli                                "  .
                   15  filler             pic  x(40) value
                          "Meta-articoli                           "  .
                   15  filler             pic  x(40) value
                          "Sottogruppi                             "  .
                   15  filler             pic  x(40) value
                          "Associazione Sottogruppi - Meta-articoli"  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione codice numerico prodotto           *
      *        *-------------------------------------------------------*
           05  w-det-num-pro.
      *            *---------------------------------------------------*
      *            * Codice alfanumerico prodotto                      *
      *            *---------------------------------------------------*
               10  w-det-num-pro-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Codice numerico prodotto                          *
      *            *---------------------------------------------------*
               10  w-det-num-pro-num      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per trasformazioni in uppercase                 *
      *    *-----------------------------------------------------------*
       01  w-upp.
           05  w-upp-des.
               10  w-upp-chr occurs 40    pic  x(01)                  .
           05  w-ctr                      pic  9(02)                  .
           05  w-upp-car.
               10  filler                 pic  x(26) value
                     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"                     .
           05  w-upp-crr redefines w-upp-car.
               10  w-upc occurs 26        pic  x(01)                  .
           05  w-low.
               10  filler                 pic  x(26) value
                     "abcdefghijklmnopqrstuvwxyz"                     .
           05  w-lor redefines w-low.
               10  w-loc occurs 26        pic  x(01)                  .
           05  w-ulc                      pic  9(02)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-600.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-600.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-300.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
       main-600.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di conversione                        *
      *                  *---------------------------------------------*
           perform   xcv-rou-pri-000      thru xcv-rou-pri-999        .
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione                                      *
      *    *-----------------------------------------------------------*
       xcv-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione marker di trattamento di al-   *
      *              * meno un elemento                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di fine file              *
      *              *-------------------------------------------------*
           move      spaces               to   w-lds-fff              .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-int      .
      *              *-------------------------------------------------*
      *              * Open files per la conversione                   *
      *              *-------------------------------------------------*
           perform   xcv-opn-fls-000      thru xcv-opn-fls-999        .
           if        w-cnt-xcv-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-xcv-flg-sub
                     go to  xcv-rou-pri-600.
       xcv-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura e bufferizzazione linea                 *
      *              *-------------------------------------------------*
           perform   xcv-lbu-lds-000      thru xcv-lbu-lds-999        .
           if        w-cnt-xcv-flg-sub    not  = spaces
                     go to  xcv-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Trattamento delle linee di stampa               *
      *              *-------------------------------------------------*
           perform   xcv-trt-lds-000      thru xcv-trt-lds-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-xcv-flg-int    not  = spaces
                     go to xcv-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Segnale di trattamento almeno un elemento ese-  *
      *              * guito                                           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-xcv-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura linee di stampa               *
      *              *-------------------------------------------------*
           go to     xcv-rou-pri-200.
       xcv-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Test se trattato almeno un elemento             *
      *              *-------------------------------------------------*
           if        w-cnt-xcv-mrk-uno    not  = spaces
                     go to xcv-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   xcv-nes-ela-000      thru xcv-nes-ela-999        .
           go to     xcv-rou-pri-600.
       xcv-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Messaggio di fine programma                     *
      *              *-------------------------------------------------*
           move      "FE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Close files per la conversione                  *
      *              *-------------------------------------------------*
           perform   xcv-cls-fls-000      thru xcv-cls-fls-999        .
       xcv-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo importazione                           *
      *                  *---------------------------------------------*
           perform   acc-tip-imp-000      thru acc-tip-imp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Pathname file in input                      *
      *                  *---------------------------------------------*
           perform   acc-pth-inp-000      thru acc-pth-inp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           move      spaces               to   rr-pth-inp             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo di importazione                            *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di importazione           :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Pathname file in input                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pathname del file in input     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo importazione          *
      *    *-----------------------------------------------------------*
       acc-tip-imp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-imp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-imp-lun    to   v-car                  .
           move      w-exp-tip-imp-num    to   v-ldt                  .
           move      "AMSA#"              to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-imp-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           if        rr-tip-imp           =    "ART "
                     move  01             to   v-num
           else if   rr-tip-imp           =    "MART"
                     move  02             to   v-num
           else if   rr-tip-imp           =    "SGRU"
                     move  03             to   v-num
           else if   rr-tip-imp           =    "ASSO"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-imp-999.
       acc-tip-imp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  "ART "         to   rr-tip-imp
           else if   v-num                =    02
                     move  "MART"         to   rr-tip-imp
           else if   v-num                =    03
                     move  "SGRU"         to   rr-tip-imp
           else if   v-num                =    04
                     move  "ASSO"         to   rr-tip-imp
           else      move  spaces         to   rr-tip-imp             .
       acc-tip-imp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-tip-imp           =    spaces
                     go to acc-tip-imp-100.
       acc-tip-imp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-imp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-imp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-imp-100.
       acc-tip-imp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo importazione                       *
      *    *-----------------------------------------------------------*
       vis-tip-imp-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-imp-lun    to   v-car                  .
           move      w-exp-tip-imp-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-imp-tbl    to   v-txt                  .
           move      05                   to   v-lin                  .
           move      34                   to   v-pos                  .
           if        rr-tip-imp           =    "ART "
                     move  01             to   v-num
           else if   rr-tip-imp           =    "MART"
                     move  02             to   v-num
           else if   rr-tip-imp           =    "SGRU"
                     move  03             to   v-num
           else if   rr-tip-imp           =    "ASSO"
                     move  04             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-imp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Pathanme file in input               *
      *    *-----------------------------------------------------------*
       acc-pth-inp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
           if        rr-pth-inp           not  = spaces
                     go to acc-pth-inp-100.
      *
           if        rr-tip-imp           =    "ART "
                     move  "/abd/asc/ART.prn"
                                          to   rr-pth-inp
           else if   rr-tip-imp           =    "MART"
                     move  "/abd/asc/MART.prn"
                                          to   rr-pth-inp
           else if   rr-tip-imp           =    "SGRU"
                     move  "/abd/asc/SGRU.prn"
                                          to   rr-pth-inp
           else if   rr-tip-imp           =    "ASSO"
                     move  "/abd/asc/ASSOC.prn"
                                          to   rr-pth-inp
           else      move  "/abd/asc/"    to   rr-pth-inp             .
       acc-pth-inp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           if        w-cnt-sts-imp-ric    not  = spaces
                     move  "DO  "         to   v-pfk (05)             .
           move      rr-pth-inp           to   v-alf                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-pth-inp-999.
       acc-pth-inp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-pth-inp             .
       acc-pth-inp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
           if        rr-pth-inp           =    spaces
                     go to acc-pth-inp-100.
       acc-pth-inp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pth-inp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-pth-inp-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-pth-inp-100.
       acc-pth-inp-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
      *              *-------------------------------------------------*
      *              * Controlli                                       *
      *              *-------------------------------------------------*
           if        rr-pth-inp           =    spaces
                     move  "#"            to   w-cnt-tdo-ric-flg
                     go to tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Open files                         *
      *    *-----------------------------------------------------------*
       xcv-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-sub      .
      *              *-------------------------------------------------*
      *              * Numero di linee lette : zero                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-nll              .
      *              *-------------------------------------------------*
      *              * Numero di records scritti : zero                *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-dcp              .
      *              *-------------------------------------------------*
      *              * Preparazione area per visualizzazione avanza-   *
      *              * mento esecuzione                                *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero linee  lette            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records scritti [hct]   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Apertura del file in input                      *
      *              *-------------------------------------------------*
           move      "OI"                 to   g-ope                  .
           move      "cvi "               to   g-nam                  .
           move      rr-pth-inp           to   g-pat                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        g-sts                =    e-not-err
                     go to xcv-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita da routine                 *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-xcv-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore sull'input              *
      *                  *---------------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     xcv-opn-fls-999.
       xcv-opn-fls-200.
      *              *-------------------------------------------------*
      *              * [hct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "fer/gpa/fls/ioc/obj/iofhct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to xcv-opn-fls-999.
      *              *-------------------------------------------------*
      *              * Se errori                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita da routine                 *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-xcv-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore sull'output             *
      *                  *---------------------------------------------*
           perform   msg-out-err-000      thru msg-out-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     xcv-opn-fls-999.
       xcv-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Close files                        *
      *    *-----------------------------------------------------------*
       xcv-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-sub      .
       xcv-cls-fls-100.
      *              *-------------------------------------------------*
      *              * [hct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "fer/gpa/fls/ioc/obj/iofhct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Chiusura del file in input                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo utilizzato                 *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mcvinp"                         .
       xcv-cls-fls-200.
       xcv-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Messaggio per nessuna registrazio- *
      *    *                        ne da elaborare                    *
      *    *-----------------------------------------------------------*
       xcv-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessuna registrazione da elaborare !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       xcv-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Lettura e bufferizzazione delle    *
      *    *                        linee relative ad un prodotto      *
      *    *-----------------------------------------------------------*
       xcv-lbu-lds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-sub      .
       xcv-lbu-lds-100.
      *              *-------------------------------------------------*
      *              * Lettura linea 01, con eventuale skip delle      *
      *              * linee non interessanti                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura linea                               *
      *                  *---------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to xcv-lbu-lds-200.
           if        g-sts                =    e-end-fil
                     move  "#"            to   w-cnt-xcv-flg-sub
                     go to xcv-lbu-lds-999
           else      go to xcv-lbu-lds-100.
       xcv-lbu-lds-200.
      *              *-------------------------------------------------*
      *              * Linea letta in area di comodo                   *
      *              *-------------------------------------------------*
           move      g-rec                to   w-lds-001              .
       xcv-lbu-lds-300.
      *              *-------------------------------------------------*
      *              * Test se da skippare                             *
      *              *-------------------------------------------------*
______*    if        w-lds-001 (1 : 20)   not  = spaces
______*              go to xcv-lbu-lds-100.
       xcv-lbu-lds-600.
      *                  *---------------------------------------------*
      *                  * Pre-uscita                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero linee lette           *
      *                      *-----------------------------------------*
           add       1                    to   w-pds-nll              .
      *                      *-----------------------------------------*
      *                      * Visualizzo numero linee lette           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      17                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-nll            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-lbu-lds-700.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     xcv-lbu-lds-999.
       xcv-lbu-lds-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Trattamento linee di stampa        *
      *    *-----------------------------------------------------------*
       xcv-trt-lds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-int      .
       xcv-trt-lds-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        rr-tip-imp           =    "ART "
                     go to xcv-trt-lds-120
           else if   rr-tip-imp           =    "MART"
                     go to xcv-trt-lds-140
           else if   rr-tip-imp           =    "SGRU"
                     go to xcv-trt-lds-160
           else if   rr-tip-imp           =    "ASSO"
                     go to xcv-trt-lds-180
           else      go to xcv-trt-lds-999.
       xcv-trt-lds-120.
      *                  *---------------------------------------------*
      *                  * Articoli                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contenuto linea                 *
      *                      *-----------------------------------------*
           if        w-lds-001 (001 : 07) =    "COD_ART"
                     go to xcv-trt-lds-900.
           if        w-lds-001 (001 : 13) =    spaces
                     go to xcv-trt-lds-900.
      *                      *-----------------------------------------*
      *                      * Estrazione dati                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Conversione codice                  *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-key-num      .
           move      "CV"                 to   v-ope                  .
           move      13                   to   v-car                  .
           move      w-lds-001 (001 : 13) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-key-num      .
      *                          *-------------------------------------*
      *                          * Test su codice                      *
      *                          *-------------------------------------*
           if        w-lds-rew-key-num    =    zero
                     go to xcv-trt-lds-900.
      *                          *-------------------------------------*
      *                          * Descrizione 1                       *
      *                          *-------------------------------------*
           move      w-lds-001 (014 : 60) to   w-lds-rew-des-uno      .
      *                          *-------------------------------------*
      *                          * Descrizione 2                       *
      *                          *-------------------------------------*
           move      w-lds-001 (074 : 60) to   w-lds-rew-des-due      .
      *                          *-------------------------------------*
      *                          * Conversione codice Meta-articolo    *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-uno      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (155 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-agg-uno      .
      *                          *-------------------------------------*
      *                          * Conversione ordine                  *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-due      .
           move      "CV"                 to   v-ope                  .
           move      04                   to   v-car                  .
           move      w-lds-001 (184 : 04) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-agg-due      .
      *                      *-----------------------------------------*
      *                      * A scrittura record                      *
      *                      *-----------------------------------------*
           go to     xcv-trt-lds-400.
       xcv-trt-lds-140.
      *                  *---------------------------------------------*
      *                  * Meta-articoli                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contenuto linea                 *
      *                      *-----------------------------------------*
           if        w-lds-001 (001 : 08) =    "COD_MART"
                     go to xcv-trt-lds-900.
           if        w-lds-001 (001 : 10) =    spaces
                     go to xcv-trt-lds-900.
      *                      *-----------------------------------------*
      *                      * Estrazione dati                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Conversione codice                  *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-key-num      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (001 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-key-num      .
      *                          *-------------------------------------*
      *                          * Test su codice                      *
      *                          *-------------------------------------*
           if        w-lds-rew-key-num    =    zero
                     go to xcv-trt-lds-900.
      *                          *-------------------------------------*
      *                          * Descrizione 1                       *
      *                          *-------------------------------------*
           move      w-lds-001 (011 : 60) to   w-lds-rew-des-uno      .
      *                          *-------------------------------------*
      *                          * Descrizione 2                       *
      *                          *-------------------------------------*
           move      w-lds-001 (071 : 60) to   w-lds-rew-des-due      .
      *                          *-------------------------------------*
      *                          * Conversione codice Immagine         *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-uno      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (131 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-agg-uno      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-due      .
      *                      *-----------------------------------------*
      *                      * A scrittura record                      *
      *                      *-----------------------------------------*
           go to     xcv-trt-lds-400.
       xcv-trt-lds-160.
      *                  *---------------------------------------------*
      *                  * Sottogruppi                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contenuto linea                 *
      *                      *-----------------------------------------*
           if        w-lds-001 (001 : 08) =    "COD_SGRU"
                     go to xcv-trt-lds-900.
           if        w-lds-001 (001 : 10) =    spaces
                     go to xcv-trt-lds-900.
      *                      *-----------------------------------------*
      *                      * Estrazione dati                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Conversione codice                  *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-key-num      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (001 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-key-num      .
      *                          *-------------------------------------*
      *                          * Test su codice                      *
      *                          *-------------------------------------*
           if        w-lds-rew-key-num    =    zero
                     go to xcv-trt-lds-900.
      *                          *-------------------------------------*
      *                          * Descrizione 1                       *
      *                          *-------------------------------------*
           move      w-lds-001 (011 : 35) to   w-lds-rew-des-uno      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      spaces               to   w-lds-rew-des-due      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-uno      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-due      .
      *                      *-----------------------------------------*
      *                      * A scrittura record                      *
      *                      *-----------------------------------------*
           go to     xcv-trt-lds-400.
       xcv-trt-lds-180.
      *                  *---------------------------------------------*
      *                  * Associazione Sottogruppi - Meta-articoli    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su contenuto linea                 *
      *                      *-----------------------------------------*
           if        w-lds-001 (001 : 08) =    "COD_SGRU"
                     go to xcv-trt-lds-900.
           if        w-lds-001 (001 : 10) =    spaces
                     go to xcv-trt-lds-900.
      *                      *-----------------------------------------*
      *                      * Estrazione dati                         *
      *                      *                                         *
      *                      * N.B.: Eccezione = si invertono il codi- *
      *                      *       sottogruppo con il codice Meta-   *
      *                      *       articolo per evitare duplicati    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Conversione codice                  *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-uno      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (001 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-agg-uno      .
      *                          *-------------------------------------*
      *                          * Test su codice                      *
      *                          *-------------------------------------*
           if        w-lds-rew-agg-uno    =    zero
                     go to xcv-trt-lds-900.
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      spaces               to   w-lds-rew-des-uno      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      spaces               to   w-lds-rew-des-due      .
      *                          *-------------------------------------*
      *                          * Conversione codice Meta-articolo    *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-key-num      .
           move      "CV"                 to   v-ope                  .
           move      10                   to   v-car                  .
           move      w-lds-001 (011 : 10) to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-num                to   w-lds-rew-key-num      .
      *                          *-------------------------------------*
      *                          * Vuoto                               *
      *                          *-------------------------------------*
           move      zero                 to   w-lds-rew-agg-due      .
      *                      *-----------------------------------------*
      *                      * A scrittura record                      *
      *                      *-----------------------------------------*
           go to     xcv-trt-lds-400.
       xcv-trt-lds-400.
      *              *-------------------------------------------------*
      *              * Scrittura record [hct]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [hct]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "fer/gpa/fls/ioc/obj/iofhct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *                  *---------------------------------------------*
      *                  * Composizione record [hct]                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Chiave                                  *
      *                      *-----------------------------------------*
           move      rr-tip-imp           to   rf-hct-ide-rec         .
           move      w-lds-rew-key-num    to   rf-hct-knu-rec         .
      *                      *-----------------------------------------*
      *                      * Dati                                    *
      *                      *-----------------------------------------*
           move      w-lds-rew-des-uno    to   rf-hct-des-uno         .
           move      w-lds-rew-des-due    to   rf-hct-des-due         .
           move      w-lds-rew-agg-uno    to   rf-hct-agg-uno         .
           move      w-lds-rew-agg-due    to   rf-hct-agg-due         .
           move      zero                 to   rf-hct-cod-zp1         .
           move      zero                 to   rf-hct-cod-zp2         .
           move      zero                 to   rf-hct-cod-zp3         .
           move      w-lds-001            to   rf-hct-dat-rec         .
           move      spaces               to   rf-hct-alx-exp         .
      *                  *---------------------------------------------*
      *                  * Scrittura record [hct]                      *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "fer/gpa/fls/ioc/obj/iofhct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hct                 .
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti [dcp]     *
      *                  *---------------------------------------------*
           add       1                    to   w-pds-dcp              .
      *                  *---------------------------------------------*
      *                  * Visualizzo records scritti                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-dcp            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     xcv-trt-lds-900.

      *              ______________ INUTILIZZATI _______________________
       xcv-trt-lds-700.
      *              *-------------------------------------------------*
      *              * Codice prodotto in aree di comodo               *
      *              *-------------------------------------------------*
           move      w-lds-001 (023 : 5)  to   w-lds-rew-alf-pro      .
           move      w-lds-001 (023 : 5)  to   w-det-num-pro-alf      .
      *              *-------------------------------------------------*
      *              * % provvigione in area di comodo                 *
      *              *-------------------------------------------------*
           move      w-lds-001 (129 : 2)  to   w-lds-rew-ctp-alf      .
           inspect   w-lds-rew-ctp-alf
                             replacing leading spaces by "0"          .
      *              *-------------------------------------------------*
      *              * Test su codice prodotto                         *
      *              *-------------------------------------------------*
           if        w-det-num-pro-alf    =    spaces
                     go to xcv-trt-lds-900.
      *              *-------------------------------------------------*
      *              * Test su % provvigione                           *
      *              *-------------------------------------------------*
           if        w-lds-rew-ctp-num    not  numeric
                     go to xcv-trt-lds-900.
           if        w-lds-rew-ctp-num    =    zero
                     go to xcv-trt-lds-900.
      *              *-------------------------------------------------*
      *              * Se codice prodotto non esistente : no tratta-   *
      *              * mento , con unlock preventivo                   *
      *              *-------------------------------------------------*
           perform   det-num-pro-000      thru det-num-pro-999        .
           if        w-det-num-pro-num    not  = zero
                     go to xcv-trt-lds-710.
      *
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           go to     xcv-trt-lds-900.
       xcv-trt-lds-710.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [dcp]                      *
      *              *-------------------------------------------------*
           move      w-lds-rew-ctp-num    to   rf-dcp-cat-pvg         .
      *              *-------------------------------------------------*
      *              * Update [dcp]                                    *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Unlock [dcp]                                    *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Incremento numero records scritti [dcp]         *
      *              *-------------------------------------------------*
           add       1                    to   w-pds-dcp              .
      *              *-------------------------------------------------*
      *              * Visualizzo records scritti                      *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      20                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-dcp            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-trt-lds-700.
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     xcv-trt-lds-900.
       xcv-trt-lds-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     xcv-trt-lds-999.
       xcv-trt-lds-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Messaggio di input i-o error       *
      *    *-----------------------------------------------------------*
       msg-inp-err-000.
      *              *-------------------------------------------------*
      *              * Eliminazione messaggio programma in esecuzione  *
      *              *-------------------------------------------------*
           move      "PX"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      64                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore all'interno del box     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prima riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Errore di i-o codice "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      g-sts                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      " su file in input.   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Seconda riga                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Terminazione forzata del programma.         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Terza riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "        Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       msg-inp-err-600.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to msg-inp-err-800.
           if        v-alf                not  = "OK"
                     go to msg-inp-err-600.
       msg-inp-err-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-inp-err-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Messaggio di output i-o error      *
      *    *-----------------------------------------------------------*
       msg-out-err-000.
      *              *-------------------------------------------------*
      *              * Eliminazione messaggio programma in esecuzione  *
      *              *-------------------------------------------------*
           move      "PX"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      64                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore all'interno del box     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prima riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Errore di i-o codice "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      40                   to   v-pos                  .
           move      g-sts                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      43                   to   v-pos                  .
           move      " su file in output.  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Seconda riga                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Terminazione forzata del programma.         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Terza riga                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "        Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione OK di presa visione            *
      *                  *---------------------------------------------*
           move      spaces               to   v-alf                  .
       msg-out-err-600.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      61                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to msg-out-err-800.
           if        v-alf                not  = "OK"
                     go to msg-out-err-600.
       msg-out-err-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       msg-out-err-999.
           exit.

      *    *===========================================================*
      *    * Trasformazione in uppercase                               *
      *    *-----------------------------------------------------------*
       trf-des-upp-000.
           move      zero                 to   w-ctr                  .
       trf-des-upp-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    40
                     go to  trf-des-upp-999.
           move      zero                 to   w-ulc                  .
           inspect   w-low            tallying w-ulc
                     for characters     before initial w-upp-chr
                                                      (w-ctr)         .
           if        w-ulc                <    26
                     add     1            to   w-ulc
                     move    w-upc(w-ulc) to   w-upp-chr(w-ctr)       .
           go to     trf-des-upp-100.
       trf-des-upp-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-asx-000/999'                                     *
      *    * 'all-str-adx-000/999'                                     *
      *    * 'all-str-cen-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'allineamento di una stringa a sx o dx o al *
      *    * centro                                                    *
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Valore della stringa da allinea- *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-all-str-alf = Valore della stringa allineata   *
      *    *                          a sinistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-cat-000/999' - (senza uno spazio di separazione) *
      *    * 'all-str-csb-000/999' - (con uno spazio di separazione)   *
      *    *                                                           *
      *    * Routines per il concatenamento di max 10 stringhe di max  *
      *    * 80 caratteri ciascuna con o senza uno spazio di separa-   *
      *    * zione tra una stringa e l'altra                           *
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-all-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-all-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Determinazione del codice numerico prodotto               *
      *    *-----------------------------------------------------------*
       det-num-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-num-pro-num      .
       det-num-pro-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [dcp] per codice alfanumerico *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ALFPRO    "         to   f-key                  .
           move      w-det-num-pro-alf    to   rf-dcp-alf-pro         .
           move      zero                 to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata                         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-900.
       det-num-pro-200.
      *              *-------------------------------------------------*
      *              * Get-next su archivio [dcp]                      *
      *              *-------------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Test se fine file                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-num-pro-900.
       det-num-pro-300.
      *              *-------------------------------------------------*
      *              * Test max su archivio [dcp]                      *
      *              *-------------------------------------------------*
           if        rf-dcp-alf-pro       not  = w-det-num-pro-alf
                     go to det-num-pro-900.
       det-num-pro-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico                 *
      *              *-------------------------------------------------*
           move      rf-dcp-num-pro       to   w-det-num-pro-num      .
       det-num-pro-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-num-pro-999.
       det-num-pro-999.
           exit.

