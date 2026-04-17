       Identification Division.
       Program-Id.                                 pdtp4000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    dtp                 *
      *                                Settore:    int                 *
      *                                   Fase:    dtp400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 26/11/91    *
      *                       Ultima revisione:    NdK del 08/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esplosioni ed implosioni a video            *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * La fase dtp400, nella sua globalita', esegue i tipi di inter-  *
      * rogazione elencati nella tabella seguente :                    *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      *  interrog.        Descrizione per il tipo interrogazione       *
      * ----------  -------------------------------------------------- *
      *                                                                *
      * ESCPRO      Esplosione scalare distinta base per prodotto di   *
      *             vendita                                            *
      *                                                                *
      * ESCSEM      Esplosione scalare distinta base per semilavorato  *
      *             vendita                                            *
      *                                                                *
      * ESCSDV      Esplosione scalare distinta base per sub-distinta  *
      *             virtuale                                           *
      *                                                                *
      * ERIPRO      Esplosione riepilogata distinta base per prodotto  *
      *             di vendita                                         *
      *                                                                *
      * ERISEM      Esplosione riepilogata distinta base per semilavo- *
      *             rato                                               *
      *                                                                *
      * ERISDV      Esplosione riepilogata distinta base per sub-      *
      *             distinta virtuale                                  *
      *                                                                *
      * ERIPRO      Esplosione riepilogata distinta base per prodotto  *
      *             di vendita                                         *
      *                                                                *
      * EULPRO      Esplosione lineare distinta base per prodotto di   *
      *             vendita                                            *
      *                                                                *
      * EULSEM      Esplosione lineare distinta base per semilavorato  *
      *             vendita                                            *
      *                                                                *
      * EULSDV      Esplosione lineare distinta base per sub-distinta  *
      *             virtuale                                           *
      *                                                                *
      * ISCSEM      Implosione scalare semilavorato                    *
      *                                                                *
      * ISCSDV      Implosione scalare sub-distinta virtuale           *
      *                                                                *
      * ISCMAP      Implosione scalare materia prima                   *
      *                                                                *
      * IRISEM      Implosione riepilogata semilavorato                *
      *                                                                *
      * IRISDV      Implosione riepilogata sub-distinta virtuale       *
      *                                                                *
      * IRIMAP      Implosione riepilogata materia prima               *
      *                                                                *
      * IULSEM      Implosione lineare semilavorato                    *
      *                                                                *
      * IULSDV      Implosione lineare sub-distinta virtuale           *
      *                                                                *
      * IULMAP      Implosione lineare materia prima                   *
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
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "dtp"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "int"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "dtp400"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pdtp4000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    ESPLOSIONI ED IMPLOSIONI A VIDEO    "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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

      *    *===========================================================*
      *    * Work-area per tipi interrogazione                         *
      *    *-----------------------------------------------------------*
       01  w-tin.
      *        *-------------------------------------------------------*
      *        * Tabella tipi interrogazione e dati ad essi associati  *
      *        *-------------------------------------------------------*
           05  w-tin-tbl-tin.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella ed   *
      *            * altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-tin-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per caricamento iniziale elementi          *
      *            *---------------------------------------------------*
               10  w-tin-ele-wci.
                   15  w-tin-ele-wci-des  pic  x(50)                  .
                   15  w-tin-ele-wci-ast  pic  x(01)                  .
                   15  w-tin-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-tin-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi selezionabili per impostazio-  *
      *            * ne, cioe' escludendo quelli finali asteriscati    *
      *            *---------------------------------------------------*
               10  w-tin-ele-nes          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-tin-ele-nep          pic  9(03)       value 14   .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali con elementi selezionabi- *
      *            * li                                                *
      *            *---------------------------------------------------*
               10  w-tin-ele-npt          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina attualmente visualizzata            *
      *            *---------------------------------------------------*
               10  w-tin-ele-pag          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-tin-ele-max          pic  9(03)       value 50   .
      *            *---------------------------------------------------*
      *            * Elementi per tipi interrogazione                  *
      *            *---------------------------------------------------*
               10  w-tin-ele-tin occurs 50.
      *                *-----------------------------------------------*
      *                * Codice numerico tipo interrogazione           *
      *                *-----------------------------------------------*
                   15  w-tin-num-tin      pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-tin-alf-tin      pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo interrogazione               *
      *                *-----------------------------------------------*
                   15  w-tin-des-tin      pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Overlay da richiamare per il tipo di interro- *
      *                * gazione                                       *
      *                *-----------------------------------------------*
                   15  w-tin-ovy-tin      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per visualizzazione pagina attuale              *
      *    *-----------------------------------------------------------*
       01  w-vis-pag.
      *        *-------------------------------------------------------*
      *        * Numero linea video per il numero pagina               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-000          pic  9(02)       value 06   .
      *        *-------------------------------------------------------*
      *        * Si/No pagina precedente alla pagina attuale           *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-pre          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No pagina seguente alla pagina attuale             *
      *        *-------------------------------------------------------*
           05  w-vis-pag-snx-seg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice primo elemento visualizzato nella pagina at-   *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-pel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice ultimo elemento visualizzato nella pagina at-  *
      *        * tuale                                                 *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-uel          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Indice per scansione su elementi da visualizzare      *
      *        *-------------------------------------------------------*
           05  w-vis-pag-inx-tbl          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per determinazione linea a video per elemento  *
      *        * in corso di trattamento                               *
      *        *-------------------------------------------------------*
           05  w-vis-pag-lin-acv          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per ammissibilita' tasto Slct                         *
      *        *-------------------------------------------------------*
           05  w-ipc-snx-slc.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa all'ammissibi- *
      *            * lita' del tasto Slct                              *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa all'am- *
      *            * missibilita' del tasto Slct                       *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per tipo di interrogazione                            *
      *        *-------------------------------------------------------*
           05  w-ipc-tip-int.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di in- *
      *            * terrogazione                                      *
      *            *---------------------------------------------------*
               10  w-ipc-tip-int-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al tipo *
      *            * di interrogazione, che rappresenta il codice al-  *
      *            * fanumerico per il tipo di interrogazione          *
      *            *---------------------------------------------------*
               10  w-ipc-tip-int-val      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per richiamo overlay per l'esecuzione effettiva *
      *    * del tipo di interrogazione                                *
      *    *-----------------------------------------------------------*
       01  w-ovy-exe.
      *        *-------------------------------------------------------*
      *        * Pathname per il richiamo della overlay                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pat.
      *            *---------------------------------------------------*
      *            * Prefisso comune                                   *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pre          pic  x(16)       value
                     "pgm/dtp/prg/obj/"                               .
      *            *---------------------------------------------------*
      *            * Postfisso variabile                               *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pos          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per il postfisso variabile                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-inx              pic  9(02)       value zero .
           05  w-ovy-exe-spv occurs 20    pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione campi accettazione campi    *
      *    * chiave trattati dal main                                  *
      *    *-----------------------------------------------------------*
       01  w-key.
      *        *-------------------------------------------------------*
      *        * Codice numerico tipo interrogazione                   *
      *        *-------------------------------------------------------*
           05  w-key-num-int              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico tipo interrogazione               *
      *        *-------------------------------------------------------*
           05  w-key-alf-int              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per codice tipo interrogazione            *
      *        *-------------------------------------------------------*
           05  w-key-des-int              pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Overlay per il tipo interrogazione                    *
      *        *-------------------------------------------------------*
           05  w-key-ovy-int              pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per sottoprogrammi attivi della fase            *
      *    *-----------------------------------------------------------*
       01  w-spg.
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, codice   *
      *        * alfanumerico del tipo di interrogazione               *
      *        *-------------------------------------------------------*
           05  w-spg-alf-gat              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, risposta *
      *        * - Spaces : No                                         *
      *        * - S      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-spg-snx-gat              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dei sottoprogrammi attivi                     *
      *        *-------------------------------------------------------*
           05  w-spg-tbl-spg.
      *            *---------------------------------------------------*
      *            * Indice per puntamento in tabella                  *
      *            *---------------------------------------------------*
               10  w-spg-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-spg-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-spg-ele-max          pic  9(03)       value 99   .
      *            *---------------------------------------------------*
      *            * Elementi per sottoprogrammi attivi                *
      *            *---------------------------------------------------*
               10  w-spg-ele-spg occurs 99.
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-spg-alf-tin      pic  x(10)                  .

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
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione ciclo per la fase 'dtp400'           *
      *              *-------------------------------------------------*
           perform   fas-dtp-400-000      thru fas-dtp-400-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Azzeramento tabella sottoprogrammi attivi       *
      *              *-------------------------------------------------*
           move      zero                 to   w-spg-ele-num          .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi in- *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           perform   loa-tbl-tin-000      thru loa-tbl-tin-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'ammissibilita' del tasto Slct                 *
      *              *-------------------------------------------------*
           perform   ipc-snx-slc-000      thru ipc-snx-slc-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo di interrogazione passato dal chiamante *
      *              *-------------------------------------------------*
           perform   ipc-tip-int-000      thru ipc-tip-int-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se variabile esistente op- *
      *              * pure no                                         *
      *              *-------------------------------------------------*
           if        w-ipc-tip-int-snx    =    "S"
                     go to pre-exe-pgm-200
           else      go to pre-exe-pgm-800.
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. passata dal chiamante    *
      *              * per il tipo di interrogazione esistente         *
      *              *-------------------------------------------------*
       pre-exe-pgm-250.
      *                  *---------------------------------------------*
      *                  * Se il valore della variabile e' a spaces :  *
      *                  * come per variabile non esistente            *
      *                  *---------------------------------------------*
           if        w-ipc-tip-int-val    =    spaces
                     go to pre-exe-pgm-800.
       pre-exe-pgm-300.
      *                  *---------------------------------------------*
      *                  * Ricerca del tipo interrogazione in tabella  *
      *                  * dei tipi interrogazione; se non trovato :   *
      *                  * come per variabile non esistente            *
      *                  *---------------------------------------------*
           move      zero                 to   w-tin-ele-inx          .
       pre-exe-pgm-325.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-num
                     go to pre-exe-pgm-800.
           if        w-tin-alf-tin
                    (w-tin-ele-inx)       not  = w-ipc-tip-int-val
                     go to pre-exe-pgm-325.
       pre-exe-pgm-350.
      *                  *---------------------------------------------*
      *                  * Se il nome della overlay per il richiamo e' *
      *                  * a spaces : come per variabile non esistente *
      *                  *---------------------------------------------*
           if        w-tin-ovy-tin
                    (w-tin-ele-inx)       =    spaces
                     go to pre-exe-pgm-800.
       pre-exe-pgm-375.
      *                  *---------------------------------------------*
      *                  * Simulazione impostazione valori             *
      *                  *---------------------------------------------*
           move      w-tin-ele-inx        to   w-key-num-int          .
           move      w-tin-alf-tin
                    (w-tin-ele-inx)       to   w-key-alf-int          .
           move      w-tin-des-tin
                    (w-tin-ele-inx)       to   w-key-des-int          .
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-key-ovy-int          .
      *                  *---------------------------------------------*
      *                  * Memorizzazione del nome della overlay per   *
      *                  * formare il pathname completo per il ri-     *
      *                  * chiamo del programma di esecuzione          *
      *                  *---------------------------------------------*
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-ovy-exe-pos          .
       pre-exe-pgm-400.
      *                  *---------------------------------------------*
      *                  * Richiamo del programma di esecuzione        *
      *                  *---------------------------------------------*
           perform   ric-pgm-exe-000      thru ric-pgm-exe-999        .
       pre-exe-pgm-450.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a non-spaces per interrompe- *
      *                  * re la normale esecuzione del programma e    *
      *                  * ritornare al chiamante                      *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. passata dal chiamante    *
      *              * per il tipo di interrogazione non esistente     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste un solo elemento selezionabile    *
      *                  * per impostazione si forza l'esecuzione      *
      *                  * del sottoprogramma relativo                 *
      *                  *---------------------------------------------*
           if        w-tin-ele-nes        =    1
                     move  1              to   w-tin-ele-inx
                     go to pre-exe-pgm-375.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a spaces per continuare la   *
      *                  * normale esecuzione del programma            *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi interrogazio- *
      *    * ne                                                        *
      *    *-----------------------------------------------------------*
       loa-tbl-tin-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preliminare numero effettivo di e-  *
      *              * lementi in tabella                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-tin-ele-num          .
      *              *-------------------------------------------------*
      *              * Azzeramento preliminare numero di elementi se-  *
      *              * lezionabili per impostazione, esclusi quindi    *
      *              * quelli finali asteriscati                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tin-ele-nes          .
       loa-tbl-tin-100.
      *              *-------------------------------------------------*
      *              * Caricamento elementi in tabella con incremento  *
      *              * del numero effettivo di elementi in tabella ed  *
      *              * eventualmente del numero di elementi finali a-  *
      *              * steriscati e quindi non selezionabili per impo- *
      *              * stazione                                        *
      *              *-------------------------------------------------*
      *
           move      "Esplosione scalare prodotto di vendita            
      -              " ESCPRO     pdtp400a  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione scalare semilavorato                   
      -              " ESCSEM     pdtp400b  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione scalare sub-distinta virtuale          
      -              " ESCSDV     pdtp400c  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione riepilogata prodotto di vendita        
      -              " ERIPRO     pdtp400d  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione riepilogata semilavorato               
      -              " ERISEM     pdtp400e  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione riepilogata sub-distinta virtuale      
      -              " ERISDV     pdtp400f  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione lineare prodotto di vendita            
      -              " EULPRO     pdtp400g  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione lineare semilavorato                   
      -              " EULSEM     pdtp400h  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Esplosione lineare sub-distinta virtuale          
      -              " EULSDV     pdtp400i  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione scalare semilavorato                   
      -              " ISCSEM     pdtp400m  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione scalare sub-distinta virtuale          
      -              " ISCSDV     pdtp400n  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione scalare materia prima                  
      -              " ISCMAP     pdtp400o  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione riepilogata semilavorato               
      -              " IRISEM     pdtp400p  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione riepilogata sub-distinta virtuale      
      -              " IRISDV     pdtp400q  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione riepilogata materia prima              
      -              " IRIMAP     pdtp400r  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione lineare semilavorato                   
      -              " IULSEM     pdtp400s  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione lineare sub-distinta virtuale          
      -              " IULSDV     pdtp400t  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
      *
           move      "Implosione lineare materia prima                  
      -              " IULMAP     pdtp400u  "
                                          to   w-tin-ele-wci          .
           perform   loa-tbl-ele-000      thru loa-tbl-ele-999        .
       loa-tbl-tin-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione elementi residui                *
      *              *-------------------------------------------------*
           move      w-tin-ele-num        to   w-tin-ele-inx          .
       loa-tbl-tin-225.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-max
                     go to loa-tbl-tin-300.
           move      zero                 to   w-tin-num-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-alf-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-des-tin
                                              (w-tin-ele-inx)         .
           move      spaces               to   w-tin-ovy-tin
                                              (w-tin-ele-inx)         .
           go to     loa-tbl-tin-225.
       loa-tbl-tin-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagine totali             *
      *              *-------------------------------------------------*
           move      w-tin-ele-nes        to   w-tin-ele-npt          .
           add       w-tin-ele-nep        to   w-tin-ele-npt          .
           subtract  1                    from w-tin-ele-npt          .
           divide    w-tin-ele-nep        into w-tin-ele-npt          .
       loa-tbl-tin-400.
      *              *-------------------------------------------------*
      *              * Numero pagina attualmente visualizzata a : 1    *
      *              *-------------------------------------------------*
           move      1                    to   w-tin-ele-pag          .
       loa-tbl-tin-999.
           exit.

      *    *===========================================================*
      *    * Caricamento elemento in tabella tipi interrogazione       *
      *    *-----------------------------------------------------------*
       loa-tbl-ele-000.
      *              *-------------------------------------------------*
      *              * Se tabella gia' satura : uscita                 *
      *              *-------------------------------------------------*
           if        w-tin-ele-num        =    w-tin-ele-max
                     go to loa-tbl-ele-999.
      *              *-------------------------------------------------*
      *              * Incremento numero effettivo di elementi in ta-  *
      *              * bella                                           *
      *              *-------------------------------------------------*
           add       1                    to   w-tin-ele-num          .
      *              *-------------------------------------------------*
      *              * Incremento se e' il caso il numero di elementi  *
      *              * selezionabili per impostazione, escluso quindi  *
      *              * quelli finali asteriscati                       *
      *              *-------------------------------------------------*
           if        w-tin-ele-wci-ast    =    spaces
                     add   1              to   w-tin-ele-nes          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice numerico tipo interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      w-tin-ele-num        to   w-tin-num-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice alfanumerico tipo inter- *
      *              * rogazione                                       *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-alf    to   w-tin-alf-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione descrizione tipo interrogazione *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-des    to   w-tin-des-tin
                                              (w-tin-ele-num)         .
      *              *-------------------------------------------------*
      *              * Bufferizzazione overlay da richiamare per il    *
      *              * tipo interrogazione                             *
      *              *-------------------------------------------------*
           move      w-tin-ele-wci-ovy    to   w-tin-ovy-tin
                                              (w-tin-ele-num)         .
       loa-tbl-ele-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per l'ammis-  *
      *    * sibilita' del tasto Slct                                  *
      *    *-----------------------------------------------------------*
       ipc-snx-slc-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'snx-slc' dal livel- *
      *              * lo precedente                                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-snx-slc-200
           else      go to ipc-snx-slc-400.
       ipc-snx-slc-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di interrogazione passato dal chiamante                   *
      *    *-----------------------------------------------------------*
       ipc-tip-int-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tip-int' dal livel- *
      *              * lo precedente                                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "-"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-tip-int-200
           else      go to ipc-tip-int-400.
       ipc-tip-int-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tip-int-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-tip-int-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-int-999.
       ipc-tip-int-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tip-int-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-tip-int-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tip-int-999.
       ipc-tip-int-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'dtp400'                     *
      *    *-----------------------------------------------------------*
       fas-dtp-400-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dtp-400-025.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
       fas-dtp-400-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale valori di accettazione *
      *              *-------------------------------------------------*
      *                   *--------------------------------------------*
      *                   * Codice numerico per il tipo interrogazione *
      *                   *--------------------------------------------*
           move      zero                 to   w-key-num-int          .
      *                   *--------------------------------------------*
      *                   * Codice alfanumerico per il tipo interroga- *
      *                   * zione                                      *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                   *--------------------------------------------*
      *                   * Descrizione per il tipo interrogazione     *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                   *--------------------------------------------*
      *                   * Nome overlay per il tipo interrogazione    *
      *                   *--------------------------------------------*
           move      spaces               to   w-key-ovy-int          .
       fas-dtp-400-075.
      *              *-------------------------------------------------*
      *              * Prompts per impostazione tipo interrogazione    *
      *              *-------------------------------------------------*
       fas-dtp-400-077.
      *                  *---------------------------------------------*
      *                  * Prompt effettivo per tipo interrogazione    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo interrogazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dtp-400-079.
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice numerico per tipo    *
      *                  * interrogazione                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dtp-400-081.
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione per tipo inter- *
      *                  * rogazione                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dtp-400-083.
      *                  *---------------------------------------------*
      *                  * Visualizzazione trattini di separazione     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dtp-400-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina attuale                  *
      *              *-------------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
       fas-dtp-400-125.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dtp-400-200.
      *              *-------------------------------------------------*
      *              * Accettazione codice numerico tipo interroga-    *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           if        w-tin-ele-num        >    zero
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tasto di funzione usa- *
      *              * to                                              *
      *              *-------------------------------------------------*
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to fas-dtp-400-300
           else if   v-key                =    "PRSC"
                     go to fas-dtp-400-400
           else if   v-key                =    "NXSC"
                     go to fas-dtp-400-450
           else if   v-key                =    "DOWN"
                     go to fas-dtp-400-500
           else if   v-key                =    "EXIT"
                     go to fas-dtp-400-250
           else      go to fas-dtp-400-200.
       fas-dtp-400-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-dtp-400-999.
       fas-dtp-400-300.
      *              *-------------------------------------------------*
      *              * Se Return o Do                                  *
      *              *-------------------------------------------------*
       fas-dtp-400-310.
      *                  *---------------------------------------------*
      *                  * Valore impostato in campo di destinazione   *
      *                  *---------------------------------------------*
           move      v-num                to   w-key-num-int          .
       fas-dtp-400-320.
      *                  *---------------------------------------------*
      *                  * Ricerca codice numerico impostato in ta-    *
      *                  * bella dei tipi interrogazione, e deviazione *
      *                  * a seconda dell'esito della ricerca          *
      *                  *---------------------------------------------*
           if        w-key-num-int        =    zero
                     go to fas-dtp-400-330.
           move      zero                 to   w-tin-ele-inx          .
       fas-dtp-400-322.
           add       1                    to   w-tin-ele-inx          .
           if        w-tin-ele-inx        >    w-tin-ele-nes
                     go to fas-dtp-400-340.
           if        w-tin-num-tin
                    (w-tin-ele-inx)       =    w-key-num-int
                     go to fas-dtp-400-350
           else      go to fas-dtp-400-322.
       fas-dtp-400-330.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato : a zero       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di interrogazione    *
      *                      *-----------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-340.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato non esistente  *
      *                  * in tabella dei tipi interrogazione          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a puntini                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Riciclo ad impostazione del codice nu-  *
      *                      * merico per il tipo di interrogazione    *
      *                      *-----------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-350.
      *                  *---------------------------------------------*
      *                  * Se codice numerico impostato trovato nella  *
      *                  * tabella dei tipi interrogazione             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo di in-  *
      *                      * terrogazione                            *
      *                      *-----------------------------------------*
           move      w-tin-alf-tin
                    (w-tin-ele-inx)       to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo di interroga-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-tin-des-tin
                    (w-tin-ele-inx)       to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo di interroga-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      w-tin-ovy-tin
                    (w-tin-ele-inx)       to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Memorizzazione del nome della overlay   *
      *                      * per formare il pathname completo per    *
      *                      * il richiamo del programma di esecuzione *
      *                      *-----------------------------------------*
           move      w-key-ovy-int        to   w-ovy-exe-pos          .
      *                      *-----------------------------------------*
      *                      * Se il nome della overlay e' a spaces :  *
      *                      * si ricicla ad accettazione del codice   *
      *                      * numerico per il tipo di interrogazione  *
      *                      *-----------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-dtp-400-200.
      *                      *-----------------------------------------*
      *                      * Altrimenti si va' al richiamo effettivo *
      *                      * del programma di esecuzione             *
      *                      *-----------------------------------------*
           go to     fas-dtp-400-700.
       fas-dtp-400-400.
      *              *-------------------------------------------------*
      *              * Se Prsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Decremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-450.
      *              *-------------------------------------------------*
      *              * Se Nxsc                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina attualmente visua- *
      *                  * lizzata                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina attuale              *
      *                  *---------------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-500.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
       fas-dtp-400-525.
      *                  *---------------------------------------------*
      *                  * Normalizzazione e visualizzazione valori di *
      *                  * accettazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico per il tipo interroga-  *
      *                      * zione a zero                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-key-num-int          .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico per il tipo inter-  *
      *                      * rogazione a spaces                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-alf-int          .
      *                      *-----------------------------------------*
      *                      * Descrizione per il tipo interrogazione  *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-des-int          .
      *                      *-----------------------------------------*
      *                      * Nome overlay per il tipo interrogazione *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           move      spaces               to   w-key-ovy-int          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo interrogazione                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * interrogazione                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fas-dtp-400-550.
      *                  *---------------------------------------------*
      *                  * Accettazione direttamente da corpo video    *
      *                  *---------------------------------------------*
       fas-dtp-400-555.
      *                      *-----------------------------------------*
      *                      * Inizializzazione indice per scansione   *
      *                      * su elementi visualizzati                *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       fas-dtp-400-560.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       fas-dtp-400-570.
      *                      *-----------------------------------------*
      *                      * Accettazione tasto di funzione alla li- *
      *                      * nea corrispondente all'elemento in esa- *
      *                      * me                                      *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           if        w-vis-pag-inx-tbl    <    w-tin-ele-nes
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-vis-pag-snx-pre    =    "S"
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-vis-pag-snx-seg    =    "S"
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-dtp-400-580.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tasto di fun-  *
      *                      * zione usato                             *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to fas-dtp-400-630
           else if   v-key                =    "PRSC"
                     go to fas-dtp-400-600
           else if   v-key                =    "NXSC"
                     go to fas-dtp-400-610
           else if   v-key                =    "UP  "
                     go to fas-dtp-400-620
           else if   v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to fas-dtp-400-650
           else if   v-key                =    "EXIT"
                     go to fas-dtp-400-660
           else      go to fas-dtp-400-570.
       fas-dtp-400-600.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Decremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-610.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Incremento numero pagina attualmen- *
      *                          * te visualizzata                     *
      *                          *-------------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                          *-------------------------------------*
      *                          * Visualizzazione pagina attuale      *
      *                          *-------------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                          *-------------------------------------*
      *                          * Indice di scansione al primo ele-   *
      *                          * mento della pagina                  *
      *                          *-------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Riciclo ad accettazione tasto di    *
      *                          * funzione                            *
      *                          *-------------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-620.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
       fas-dtp-400-622.
      *                          *-------------------------------------*
      *                          * Se si e' al primo elemento in asso- *
      *                          * luto della tabella si ricicla ad    *
      *                          * accettazione codice numerico per il *
      *                          * tipo interrogazione                 *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    1
                     go to fas-dtp-400-200.
       fas-dtp-400-624.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-pel
                     go to fas-dtp-400-626
           else      go to fas-dtp-400-628.
       fas-dtp-400-626.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' il primo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Decremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           subtract  1                    from w-tin-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione all'ultimo  *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-uel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-628.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' il    *
      *                          * primo della pagina                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Decremento indice di scansione  *
      *                              *---------------------------------*
           subtract  1                    from w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-630.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
       fas-dtp-400-632.
      *                          *-------------------------------------*
      *                          * Se si e' all'ultimo elemento in as- *
      *                          * soluto della tabella si ricicla ad  *
      *                          * accettazione tasto di funzione      *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-tin-ele-num
                     go to fas-dtp-400-570.
       fas-dtp-400-634.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del numero d'- *
      *                          * ordine dell'elemento in esame nel-  *
      *                          * l'ambito della pagina               *
      *                          *-------------------------------------*
           if        w-vis-pag-inx-tbl    =    w-vis-pag-inx-uel
                     go to fas-dtp-400-636
           else      go to fas-dtp-400-638.
       fas-dtp-400-636.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame e' l'ultimo  *
      *                          * della pagina                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Video in Off                    *
      *                              *---------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Incremento numero pagina at-    *
      *                              * tualmente visualizzata          *
      *                              *---------------------------------*
           add       1                    to   w-tin-ele-pag          .
      *                              *---------------------------------*
      *                              * Visualizzazione pagina attuale  *
      *                              *---------------------------------*
           perform   vis-pag-att-000      thru vis-pag-att-999        .
      *                              *---------------------------------*
      *                              * Indice di scansione al primo    *
      *                              * elemento della pagina           *
      *                              *---------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Video in On                     *
      *                              *---------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-638.
      *                          *-------------------------------------*
      *                          * Se l'elemento in esame non e' l'ul- *
      *                          * timo della pagina                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento indice di scansione  *
      *                              *---------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                              *---------------------------------*
      *                              * Riciclo ad accettazione tasto   *
      *                              * di funzione                     *
      *                              *---------------------------------*
           go to     fas-dtp-400-560.
       fas-dtp-400-650.
      *                      *-----------------------------------------*
      *                      * Se Do o Slct o Return                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice numerico per il tipo inter-  *
      *                          * rogazione                           *
      *                          *-------------------------------------*
           move      w-tin-num-tin
                    (w-vis-pag-inx-tbl)   to   w-key-num-int          .
      *                          *-------------------------------------*
      *                          * Codice alfanumerico per il tipo     *
      *                          * interrogazione                      *
      *                          *-------------------------------------*
           move      w-tin-alf-tin
                    (w-vis-pag-inx-tbl)   to   w-key-alf-int          .
      *                          *-------------------------------------*
      *                          * Descrizione per il tipo interroga-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-tin-des-tin
                    (w-vis-pag-inx-tbl)   to   w-key-des-int          .
      *                          *-------------------------------------*
      *                          * Nome overlay per il tipo interroga- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-tin-ovy-tin
                    (w-vis-pag-inx-tbl)   to   w-key-ovy-int          .
      *                          *-------------------------------------*
      *                          * Memorizzazione del nome della over- *
      *                          * lay per formare il pathname comple- *
      *                          * to per il richiamo del programma di *
      *                          * esecuzione                          *
      *                          *-------------------------------------*
           move      w-key-ovy-int        to   w-ovy-exe-pos          .
      *                          *-------------------------------------*
      *                          * Se il nome della overlay e' a spa-  *
      *                          * ces : si ricicla ad accettazione    *
      *                          * tasto di funzione                   *
      *                          *-------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to fas-dtp-400-570.
      *                          *-------------------------------------*
      *                          * Altrimenti si va' al richiamo ef-   *
      *                          * fettivo del programma di esecuzione *
      *                          *-------------------------------------*
           go to     fas-dtp-400-700.
       fas-dtp-400-660.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ritorno all'accettazione del codice *
      *                          * numerico per il tipo interrogazione *
      *                          *-------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-700.
      *              *-------------------------------------------------*
      *              * Richiamo del programma di esecuzione            *
      *              *-------------------------------------------------*
           perform   ric-pgm-exe-000      thru ric-pgm-exe-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile 'snx-slc' dallo stesso  *
      *              * livello di profondita' applicativa              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'esito della lettura *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to fas-dtp-400-725
           else      go to fas-dtp-400-750.
       fas-dtp-400-725.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fas-dtp-400-999.
       fas-dtp-400-750.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice numerico per tipo    *
      *                  * interrogazione                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-key-num-int        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione per tipo inter- *
      *                  * rogazione                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad accettazione codice numerico per *
      *                  * il tipo di interrogazione                   *
      *                  *---------------------------------------------*
           go to     fas-dtp-400-200.
       fas-dtp-400-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina attualmente visualizzata           *
      *    *-----------------------------------------------------------*
       vis-pag-att-000.
      *              *-------------------------------------------------*
      *              * Determinazioni iniziali                         *
      *              *-------------------------------------------------*
       vis-pag-att-010.
      *                  *---------------------------------------------*
      *                  * Si/No pagina precedente alla pagina attuale *
      *                  *---------------------------------------------*
           if        w-tin-ele-pag        >    1
                     move  "S"            to   w-vis-pag-snx-pre
           else      move  "N"            to   w-vis-pag-snx-pre      .
       vis-pag-att-020.
      *                  *---------------------------------------------*
      *                  * Si/No pagina seguente alla pagina attuale   *
      *                  *---------------------------------------------*
           if        w-tin-ele-pag        <    w-tin-ele-npt
                     move  "S"            to   w-vis-pag-snx-seg
           else      move  "N"            to   w-vis-pag-snx-seg      .
       vis-pag-att-030.
      *                  *---------------------------------------------*
      *                  * Indice del primo elemento visualizzato nel- *
      *                  * la pagina attuale                           *
      *                  *---------------------------------------------*
           move      w-tin-ele-pag        to   w-vis-pag-inx-pel      .
           subtract  1                    from w-vis-pag-inx-pel      .
           multiply  w-tin-ele-nep        by   w-vis-pag-inx-pel      .
           add       1                    to   w-vis-pag-inx-pel      .
       vis-pag-att-040.
      *                  *---------------------------------------------*
      *                  * Indice dell'ultimo elemento visualizzato    *
      *                  * nella pagina attuale                        *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-uel      .
           add       w-tin-ele-nep        to   w-vis-pag-inx-uel      .
           subtract  1                    from w-vis-pag-inx-uel      .
           if        w-vis-pag-inx-uel    >    w-tin-ele-nes
                     move  w-tin-ele-nes  to   w-vis-pag-inx-uel      .
       vis-pag-att-050.
      *              *-------------------------------------------------*
      *              * Abblencamento area totale occupata              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      w-vis-pag-lin-000    to   v-lto                  .
           add       w-tin-ele-nep        to   v-lto                  .
           add       1                    to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-100.
      *              *-------------------------------------------------*
      *              * Linea iniziale per il numero pagina             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esiste una sola pagina non si visualizza *
      *                  * il numero pagina                            *
      *                  *---------------------------------------------*
           if        w-tin-ele-npt        not  > 1
                     go to vis-pag-att-200.
      *                  *---------------------------------------------*
      *                  * Visualizzazione literal                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pagina"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero pagina               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           move      08                   to   v-pos                  .
           move      w-tin-ele-pag        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-pag-att-200.
      *              *-------------------------------------------------*
      *              * Linee effettive di tipi interrogazione          *
      *              *-------------------------------------------------*
       vis-pag-att-300.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice per scansione su e- *
      *                  * lementi da visualizzare                     *
      *                  *---------------------------------------------*
           move      w-vis-pag-inx-pel    to   w-vis-pag-inx-tbl      .
       vis-pag-att-400.
      *                  *---------------------------------------------*
      *                  * Trattamento elemento in esame               *
      *                  *---------------------------------------------*
       vis-pag-att-410.
      *                      *-----------------------------------------*
      *                      * Determinazione linea a video per ele-   *
      *                      * mento in esame                          *
      *                      *-----------------------------------------*
           move      w-vis-pag-inx-tbl    to   w-vis-pag-lin-acv      .
           subtract  w-vis-pag-inx-pel    from w-vis-pag-lin-acv      .
           add       w-vis-pag-lin-000    to   w-vis-pag-lin-acv      .
           add       1                    to   w-vis-pag-lin-acv      .
       vis-pag-att-420.
      *                      *-----------------------------------------*
      *                      * Se codice numerico per il tipo interro- *
      *                      * gazione a zero : no visualizzazione     *
      *                      *-----------------------------------------*
           if        w-tin-num-tin
                    (w-vis-pag-inx-tbl)   =    zero
                     go to vis-pag-att-500.
       vis-pag-att-430.
      *                      *-----------------------------------------*
      *                      * Visualizzazione codice numerico per il  *
      *                      * tipo interrogazione                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      16                   to   v-pos                  .
           move      w-tin-num-tin
                    (w-vis-pag-inx-tbl)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-440.
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal '-'             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      21                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-450.
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione per il tipo *
      *                      * di interrogazione                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           move      23                   to   v-pos                  .
           move      w-tin-des-tin
                    (w-vis-pag-inx-tbl)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-att-500.
      *                  *---------------------------------------------*
      *                  * Incremento indice per scansione su elementi *
      *                  * da visualizzare                             *
      *                  *---------------------------------------------*
           add       1                    to   w-vis-pag-inx-tbl      .
      *                  *---------------------------------------------*
      *                  * Se oltre ultimo elemento da visualizzare    *
      *                  * per la pagina si va' alla conclusione, al-  *
      *                  * trimenti si ricicla al trattamento dell'e-  *
      *                  * lemento in esame                            *
      *                  *---------------------------------------------*
           if        w-vis-pag-inx-tbl    >    w-vis-pag-inx-uel
                     go to vis-pag-att-900
           else      go to vis-pag-att-400.
       vis-pag-att-900.
      *              *-------------------------------------------------*
      *              * Linea finale per il literal 'segue'             *
      *              *-------------------------------------------------*
       vis-pag-att-905.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se esiste o no una pa- *
      *                  * gina seguente alla pagina attuale           *
      *                  *---------------------------------------------*
           if        w-vis-pag-snx-seg    =    "S"
                     go to vis-pag-att-910
           else      go to vis-pag-att-915.
       vis-pag-att-910.
      *                  *---------------------------------------------*
      *                  * Se esiste una pagina seguente alla pagina   *
      *                  * attuale                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione literal 'segue'         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-000    to   v-lin                  .
           add       w-tin-ele-nep        to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Segue"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-915.
      *                  *---------------------------------------------*
      *                  * Se non esiste una pagina seguente alla pa-  *
      *                  * gina attuale                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del numero di pa-  *
      *                      * gine totali                             *
      *                      *-----------------------------------------*
           if        w-tin-ele-npt        >    1
                     go to vis-pag-att-925
           else      go to vis-pag-att-920.
       vis-pag-att-920.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali non superio- *
      *                      * re a 1                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-925.
      *                      *-----------------------------------------*
      *                      * Se numero di pagine totali superiore a  *
      *                      * 1                                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione literal 'fine '     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-vis-pag-lin-acv    to   v-lin                  .
           add       1                    to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fine "              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     vis-pag-att-999.
       vis-pag-att-999.
           exit.

      *    *===========================================================*
      *    * Richiamo del programma di esecuzione                      *
      *    *-----------------------------------------------------------*
       ric-pgm-exe-000.
      *              *-------------------------------------------------*
      *              * Se nome overlay a spaces : uscita               *
      *              *-------------------------------------------------*
           if        w-ovy-exe-pos        =    spaces
                     go to ric-pgm-exe-999.
       ric-pgm-exe-200.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. relativa    *
      *              * all'ammissibilita' del tasto di funzione Slct,  *
      *              * per lo stesso livello di profondita' applica-   *
      *              * tiva                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se variabile di i.p.c. eventualmente passa- *
      *                  * ta dal chiamante non esistente : no scrit-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        w-ipc-snx-slc-snx    not  = "S"
                     go to ric-pgm-exe-400.
      *                  *---------------------------------------------*
      *                  * Se valore della variabile di i.p.c. even-   *
      *                  * tualmente passata dal chiamante a No : no   *
      *                  * scrittura                                   *
      *                  *---------------------------------------------*
           if        w-ipc-snx-slc-val    not  = "S"
                     go to ric-pgm-exe-400.
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ric-pgm-exe-400.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile di i.p.c. relativa    *
      *              * al tipo di chiamante per il sottoprogramma, se  *
      *              * il main-program o un altro sottoprogramma       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva                         *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tdc-mos"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "M"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       ric-pgm-exe-600.
      *              *-------------------------------------------------*
      *              * Richiamo programma                              *
      *              *-------------------------------------------------*
           add       1                    to   w-ovy-exe-inx          .
           move      w-ovy-exe-pos        to   w-ovy-exe-spv
                                              (w-ovy-exe-inx)         .
           call      w-ovy-exe-pat       using i-ide
                                               w-ovy-exe
                                               w-tin
                                               w-spg                  .
      *              *-------------------------------------------------*
      *              * Cancellazione programma                         *
      *              *-------------------------------------------------*
           move      w-ovy-exe-spv
                    (w-ovy-exe-inx)       to   w-ovy-exe-pos          .
           cancel    w-ovy-exe-pat                                    .
           subtract  1                    from w-ovy-exe-inx          .
       ric-pgm-exe-999.
           exit.

