       Identification Division.
       Program-Id.                                 pcnvztf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    zat                 *
      *                                   Fase:    cnvztf              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 14/11/97    *
      *                       Ultima revisione:    NdK del 28/12/97    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Conversione anagrafica fornitori            *
      *                                                                *
      *                    ZATTI                                       *
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

       Special-Names.	   Decimal-Point	Is Comma .

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
                     "zat"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "cnvztf"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pcnvztf0"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " Conversione anagrafiche fornitori ZATTI"       .

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
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .

      *    *===========================================================*
      *    * Work-area richieste per conversione                       *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Pathname del file in input                            *
      *        *-------------------------------------------------------*
           05  rr-pth-inp                 pic  x(40)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo accodamento per file in output        *
      *        *-------------------------------------------------------*
           05  w-exp-out-ext.
               10  w-exp-out-ext-num      pic  9(02)       value 2    .
               10  w-exp-out-ext-lun      pic  9(02)       value 40   .
               10  w-exp-out-ext-tbl.
                   15  filler             pic  x(40) value
                            "Accodamento al file gia' esistente      ".
                   15  filler             pic  x(40) value
                            "Creazione di un file in output nuovo    ".

      *    *===========================================================*
      *    * Work area per la raccolta di una intera pagina di stampa  *
      *    *-----------------------------------------------------------*
       01  w-pds.
      *        *-------------------------------------------------------*
      *        * Tipo di divisione della pagina                        *
      *        * - F : a numero di linee fisso                         *
      *        * - V : a numero di linee variabile, con riconoscimento *
      *        *       dell'inizio pagina                              *
      *        *-------------------------------------------------------*
           05  w-pds-tdp                  pic  x(01)       value "F"  .
      *        *-------------------------------------------------------*
      *        * Numero di linee per ogni pagina di stampa, solo se    *
      *        * tipo di divisione pagina 'F'                          *
      *        *-------------------------------------------------------*
           05  w-pds-max                  pic  9(02)       value 01   .
      *        *-------------------------------------------------------*
      *        * Marker di fine file                                   *
      *        *-------------------------------------------------------*
           05  w-pds-mff                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Marker di inizio pagina in sospeso                    *
      *        *-------------------------------------------------------*
           05  w-pds-mip                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Linea di inizio pagina in sospeso                     *
      *        *-------------------------------------------------------*
           05  w-pds-lip.
               10  filler occurs 640      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee lette                                 *
      *        *-------------------------------------------------------*
           05  w-pds-nll                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee ignorate                              *
      *        *-------------------------------------------------------*
           05  w-pds-nli                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero di pagine lette                                *
      *        *-------------------------------------------------------*
           05  w-pds-npl                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero di records scritti                             *
      *        *-------------------------------------------------------*
           05  w-pds-cof                  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Numero di linee lette per la pagina di stampa         *
      *        *-------------------------------------------------------*
           05  w-pds-ctr                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Indice di comodo sulla pagina di stampa               *
      *        *-------------------------------------------------------*
           05  w-pds-inx                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Pagina di stampa                                      *
      *        *-------------------------------------------------------*
           05  w-pds-pag.
      *            *---------------------------------------------------*
      *            * Max 96 linee di stampa                            *
      *            *---------------------------------------------------*
               10  w-pds-rig occurs 96.
      *                *-----------------------------------------------*
      *                * Max 132 caratteri per ogni linea di stampa    *
      *                *-----------------------------------------------*
                   15  w-pds-chr occurs 132
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per estrazioni                                   *
      *        *-------------------------------------------------------*
           05  w-pds-est.
      *            *---------------------------------------------------*
      *            * Numero linea                                      *
      *            *---------------------------------------------------*
               10  w-pds-lin              pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero posizione                                  *
      *            *---------------------------------------------------*
               10  w-pds-pos              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza                                         *
      *            *---------------------------------------------------*
               10  w-pds-lun              pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo campo                                        *
      *            * - A : Alfanumerico                                *
      *            * - N : Numerico intero                             *
      *            *---------------------------------------------------*
               10  w-pds-tip              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore alfanumerico                               *
      *            *---------------------------------------------------*
               10  w-pds-alf              pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico                                   *
      *            *---------------------------------------------------*
               10  w-pds-num              pic s9(13)v9(05)   trailing
                                                             separate
                                                             character.
      *            *---------------------------------------------------*
      *            * Aree di work locali per l'estrazione              *
      *            *---------------------------------------------------*
               10  w-pds-w01.
                   15  w-pds-w02 occurs 641
                                          pic  x(01)                  .
               10  w-pds-w03              pic  9(03)                  .
               10  w-pds-w04.
                   15  w-pds-w05 occurs 25.
                       20  w-pds-w06      pic  x(01)                  .
                       20  w-pds-w07 redefines
                           w-pds-w06      pic  9(01)                  .
               10  w-pds-w08              pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per la ridefinizione delle linee di stampa      *
      *    *-----------------------------------------------------------*
       01  w-lds.
      *        *-------------------------------------------------------*
      *        * Linea base                                            *
      *        *-------------------------------------------------------*
           05  w-lds-000.
               10  filler    occurs 640   pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per ciclo scansione pagina                            *
      *        *-------------------------------------------------------*
           05  w-det-csc-pag.
      *            *---------------------------------------------------*
      *            * Contatori                                         *
      *            *---------------------------------------------------*
               10  w-det-csc-pag-c01      pic  9(03)                  .
               10  w-det-csc-pag-c02      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione comune corrispondente in base *
      *        * a Cap e descrizione                                   *
      *        *-------------------------------------------------------*
           05  w-det-cap-cfl.
               10  w-det-cap-cfl-flg      pic  x(01)                  .
               10  w-det-cap-cfl-ctr      pic  9(05)                  .
               10  w-det-cap-cfl-cap      pic  x(05)                  .
               10  w-det-cap-cfl-cfl      pic  x(30)                  .
               10  w-det-cap-cfl-cmn      pic  9(05)                  .
               10  w-det-cap-cfl-fzn      pic  9(03)                  .
               10  w-det-cap-cfl-lct      pic  9(03)                  .
               10  w-det-cap-cfl-prv      pic  x(03)                  .
               10  w-det-cap-cfl-ele      pic  x(01)                  .
               10  w-det-cap-cfl-cmp      pic  x(30)                  .
               10  w-det-cap-cfl-cmd      pic  x(30)                  .
               10  w-det-cap-cfl-wrk.
                   15  w-det-cap-cfl-wcp.
                       20  w-det-cap-cfl-wwc
                                          pic  x(02)                  .
                       20  w-det-cap-cfl-wws
                                          pic  x(01)                  .
                       20  w-det-cap-cfl-wwp
                                          pic  x(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-det-cap-cfl-wds  pic  x(25)                  .
                   15  filler             pic  x(01)                  .
                   15  w-det-cap-cfl-wpr  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione importo spesa 'Porto'         *
      *        *-------------------------------------------------------*
           05  w-det-sps-prt.
               10  w-det-sps-prt-cmd      pic  x(02)                  .
               10  w-det-sps-prt-cmd-r    redefines
                   w-det-sps-prt-cmd.
                   15  w-det-sps-prt-cnu  pic  9(02)                  .
               10  w-det-sps-prt-imp      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione se ragione sociale > 60 chr.  *
      *        *-------------------------------------------------------*
           05  w-det-rag-soc.
               10  w-det-rag-soc-rag      pic  x(40)                  .
               10  w-det-rag-soc-rov      pic  x(20)                  .

      *    *===========================================================*
      *    * Work area per la definizione record [cli]                 *
      *    *-----------------------------------------------------------*
       01  w-cli.
           05  w-cli-cod-cof              pic  9(05)                  .
           05  w-cli-cod-cof-edt          pic  x(05)                  .
           05  w-cli-rag-key              pic  x(40)                  .
           05  w-cli-rag-soc.
               10  w-cli-rag-soc-001      pic  x(30)                  .
               10  w-cli-rag-soc-002      pic  x(25)                  .
           05  w-cli-via-cli              pic  x(40)                  .
           05  w-cli-loc-cli              pic  x(40)                  .
           05  w-cli-cod-naz              pic  x(02)                  .
           05  w-cli-cod-cmn              pic  9(05)                  .
           05  w-cli-cod-fzn              pic  9(03)                  .
           05  w-cli-cod-lct              pic  9(03)                  .
           05  w-cli-pre-tel              pic  x(20)                  .
           05  w-cli-num-tel              pic  x(20)                  .
           05  w-cli-num-fax              pic  x(20)                  .
           05  w-cli-num-tlx              pic  x(20)                  .
           05  w-cli-nom-int              pic  x(30)                  .
           05  w-cli-cod-iva              pic  x(02)                  .
           05  w-cli-prt-iva              pic  9(11)                  .
           05  w-cli-cod-fis              pic  x(16)                  .
           05  w-cli-snx-a13              pic  x(01)                  .
           05  w-cli-cod-cge              pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per la definizione record [dcc]                 *
      *    *-----------------------------------------------------------*
       01  w-dcc.
           05  w-dcc-rag-dcc              pic  x(40)                  .
           05  w-dcc-rs1-doc              pic  x(30)                  .
           05  w-dcc-rs2-doc              pic  x(30)                  .
           05  w-dcc-via-dcc              pic  x(40)                  .
           05  w-dcc-loc-dcc              pic  x(40)                  .
           05  w-dcc-cod-cmn              pic  9(05)                  .
           05  w-dcc-cod-fzn              pic  9(03)                  .
           05  w-dcc-cod-lct              pic  9(03)                  .
           05  w-dcc-cod-zon              pic  9(02)                  .
           05  w-dcc-cod-cat              pic  9(02)                  .
           05  w-dcc-cod-vet              pic  9(03)                  .
           05  w-dcc-cod-vt2              pic  9(03)                  .
           05  w-dcc-num-tel              pic  x(20)                  .
           05  w-dcc-num-fax              pic  x(20)                  .
           05  w-dcc-num-tlx              pic  x(20)                  .
           05  w-dcc-nom-int              pic  x(30)                  .
           05  w-dcc-cod-age              pic  9(03)                  .
           05  w-dcc-abn-vtt              pic  x(10)                  .
           05  w-dcc-des-fop              pic  x(25)                  .
           05  w-dcc-cod-fop              pic  9(02)                  .
           05  w-dcc-cod-abi              pic  9(05)                  .
           05  w-dcc-cod-cab              pic  9(05)                  .
           05  w-dcc-not-g01              pic  x(30)                  .
           05  w-dcc-not-g02              pic  x(30)                  .
           05  w-dcc-not-g03              pic  x(30)                  .
           05  w-dcc-noi-fnt              pic  x(15)                  .
           05  w-dcc-tip-esm              pic  9(02)                  .
           05  w-dcc-ggg-alt              pic  9(02)                  .
           05  w-dcc-mmm-e01              pic  9(02)                  .
           05  w-dcc-mmm-e02              pic  9(02)                  .
           05  w-dcc-add-spi              pic  x(01)                  .
           05  w-dcc-add-spb              pic  x(01)                  .
           05  w-dcc-rag-bft              pic  x(01)                  .
           05  w-dcc-via-fat              pic  x(30)                  .
           05  w-dcc-loc-fat              pic  x(30)                  .
           05  w-dcc-via-rib              pic  x(30)                  .
           05  w-dcc-loc-rib              pic  x(30)                  .
           05  w-dcc-des-abi              pic  x(30)                  .
           05  w-dcc-cod-prt              pic  x(02)                  .
           05  w-dcc-fid-cli              pic  x(15)                  .
           05  w-dcc-lic-es1              pic  x(30)                  .
           05  w-dcc-lic-es2              pic  x(30)                  .
           05  w-dcc-lic-es3              pic  x(30)                  .
           05  w-dcc-lic-es4              pic  x(30)                  .
           05  w-dcc-lic-es5              pic  x(30)                  .
           05  w-dcc-lic-sca              pic  x(06)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
       01  w-all-str.
      *        *-------------------------------------------------------*
      *        * Lunghezza della stringa da allineare                  *
      *        *-------------------------------------------------------*
           05  w-all-str-lun              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Valore della stringa da allineare, e allineata o con- *
      *        * catenata                                              *
      *        *-------------------------------------------------------*
           05  w-all-str-alf.
               10  filler    occurs 240   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero delle stringhe da concatenare                  *
      *        *-------------------------------------------------------*
           05  w-all-str-num              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Valore delle stringhe da concatenare                  *
      *        *-------------------------------------------------------*
           05  w-all-str-cat  occurs 10   pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Work-area locale, per indici, contatori, ecc.         *
      *        *-------------------------------------------------------*
           05  w-all-str-wst.
               10  filler    occurs 240   pic  x(01)                  .
           05  w-all-str-i01              pic  9(03)                  .
           05  w-all-str-i02              pic  9(03)                  .
           05  w-all-str-i03              pic  9(03)                  .
           05  w-all-str-inx              pic  9(02)                  .
           05  w-all-str-pnt              pic  9(03)                  .
           05  w-all-str-max              pic  9(03)                  .

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
      *              * Inizializzazione marker di trattamento della    *
      *              * prima pagina                                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-mrk-uno      .
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
      *              * Lettura e bufferizzazione di una pagina stampa  *
      *              *-------------------------------------------------*
           perform   xcv-lbu-pds-000      thru xcv-lbu-pds-999        .
           if        w-cnt-xcv-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-xcv-flg-sub
                     go to  xcv-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Trattamento della pagina di stampa              *
      *              *-------------------------------------------------*
           perform   xcv-trt-pds-000      thru xcv-trt-pds-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-xcv-flg-int    not  = spaces
                     go to xcv-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Segnale di trattamento prima pagina eseguito    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-xcv-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura pagina di stampa              *
      *              *-------------------------------------------------*
           go to     xcv-rou-pri-200.
       xcv-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Test se trattata almeno una pagina              *
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
      *    * Routine di estrazione valore campo da buffer pagina       *
      *    *-----------------------------------------------------------*
       xcv-est-val-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo campo           *
      *              *-------------------------------------------------*
           if        w-pds-tip            =    "A"
                     go to xcv-est-val-100
           else if   w-pds-tip            =    "N"
                     go to xcv-est-val-200
           else      go to xcv-est-val-999.
       xcv-est-val-100.
      *              *-------------------------------------------------*
      *              * Estrazione valore alfanumerico                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea interessata in area di lavoro         *
      *                  *---------------------------------------------*
           move      w-pds-rig
                    (w-pds-lin)           to   w-pds-w01              .
      *                  *---------------------------------------------*
      *                  * Normalizzazione risultato                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-pds-alf              .
      *                  *---------------------------------------------*
      *                  * Terminatore per unstring                    *
      *                  *---------------------------------------------*
           move      w-pds-pos            to   w-pds-w03              .
           add       w-pds-lun            to   w-pds-w03              .
           move      high-value           to   w-pds-w02
                                              (w-pds-w03)             .
      *                  *---------------------------------------------*
      *                  * Pointer iniziale per unstring               *
      *                  *---------------------------------------------*
           move      w-pds-pos            to   w-pds-w03              .
      *                  *---------------------------------------------*
      *                  * Estrazione vera e propria                   *
      *                  *---------------------------------------------*
           unstring  w-pds-w01  delimited by   high-value
                                          into w-pds-alf
                                  with pointer w-pds-w03              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     xcv-est-val-999.
       xcv-est-val-200.
      *              *-------------------------------------------------*
      *              * Estrazione valore numerico intero               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea interessata in area di lavoro         *
      *                  *---------------------------------------------*
           move      w-pds-rig
                    (w-pds-lin)           to   w-pds-w01              .
      *                  *---------------------------------------------*
      *                  * Estrazione rappresentazione alfanumerica    *
      *                  *---------------------------------------------*
           move      spaces               to   w-pds-w04              .
           move      w-pds-pos            to   w-pds-w03              .
           add       w-pds-lun            to   w-pds-w03              .
           move      high-value           to   w-pds-w02
                                              (w-pds-w03)             .
           move      w-pds-pos            to   w-pds-w03              .
           unstring  w-pds-w01  delimited by   high-value
                                          into w-pds-w04
                                  with pointer w-pds-w03              .
      *                  *---------------------------------------------*
      *                  * Conversione rappresentazione alfanumerica   *
      *                  * in rappresentazione numerica                *
      *                  *---------------------------------------------*
           move      zero                 to   w-pds-num              .
           move      zero                 to   w-pds-w08              .
       xcv-est-val-300.
           add       1                    to   w-pds-w08              .
           if        w-pds-w08            >    25
                     go to xcv-est-val-999.
           if        w-pds-w06
                    (w-pds-w08)           <    "0" or
                     w-pds-w06
                    (w-pds-w08)           >    "9"
                     go to xcv-est-val-300.
           multiply  10                   by   w-pds-num              .
           add       w-pds-w07
                    (w-pds-w08)           to   w-pds-num              .
           go to     xcv-est-val-300.
       xcv-est-val-999.
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
      *                  * Pathname file in input                      *
      *                  *---------------------------------------------*
           perform   acc-pth-inp-000      thru acc-pth-inp-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
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
      *              * Pathname file in input                          *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Pathname del file in input     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-ric-sel-999.
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
           move      "/abd/asc/ZAT/FORNIT.TXT"
                                          to   rr-pth-inp             .
       acc-pth-inp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      35                   to   v-pos                  .
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
      *              * Normalizzazione marker di fine file             *
      *              *-------------------------------------------------*
           move      spaces               to   w-pds-mff              .
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di inizio pagina in so-  *
      *              * speso                                           *
      *              *-------------------------------------------------*
           move      "0"                  to   w-pds-mip              .
      *              *-------------------------------------------------*
      *              * Numero di linee lette : zero                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-nll              .
      *              *-------------------------------------------------*
      *              * Numero di linee ignorate : zero                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-nli              .
      *              *-------------------------------------------------*
      *              * Numero di pagine lette : zero                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-npl              .
      *              *-------------------------------------------------*
      *              * Numero di records scritti : zero                *
      *              *-------------------------------------------------*
           move      zero                 to   w-pds-cof              .
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
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero linee  ignorate         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero pagine lette            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero records scritti         :"
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
      *              * Contatore per linee cliente                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-csc-pag-c01      .
           move      zero                 to   w-det-csc-pag-c02      .
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * [gxc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
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
      *    * Ciclo di conversione : Lettura e bufferizzazione di una   *
      *    *                        pagina di stampa                   *
      *    *-----------------------------------------------------------*
       xcv-lbu-pds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-sub      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo di divisione    *
      *              * della pagina                                    *
      *              *-------------------------------------------------*
           if        w-pds-tdp            =    "F"
                     go to xcv-lbu-pds-100
           else      go to xcv-lbu-pds-400.
       xcv-lbu-pds-100.
      *              *-------------------------------------------------*
      *              * Se pagina a formato fisso                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se marker di fine file in On           *
      *                  *---------------------------------------------*
           if        w-pds-mff            not  = spaces
                     move  "#"            to   w-cnt-xcv-flg-sub
                     go to xcv-lbu-pds-999.
      *                  *---------------------------------------------*
      *                  * Lettura della pagina di stampa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero linee lette per la pagina : zero *
      *                      *-----------------------------------------*
           move      zero                 to   w-pds-ctr              .
       xcv-lbu-pds-125.
      *                      *-----------------------------------------*
      *                      * Lettura linea                           *
      *                      *-----------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
      *                      *-----------------------------------------*
      *                      * Test se errori                          *
      *                      *-----------------------------------------*
           if        g-sts                =    e-not-err
                     go to xcv-lbu-pds-200.
           if        g-sts                =    e-end-fil
                     go to xcv-lbu-pds-175.
       xcv-lbu-pds-150.
      *                      *-----------------------------------------*
      *                      * Se errore di i-o                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Status di uscita da routine         *
      *                          *-------------------------------------*
           move      "#"                  to   w-cnt-xcv-flg-sub      .
      *                          *-------------------------------------*
      *                          * Messaggio di errore sull'input      *
      *                          *-------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     xcv-lbu-pds-999.
       xcv-lbu-pds-175.
      *                      *-----------------------------------------*
      *                      * Se fine file                            *
      *                      *-----------------------------------------*
           if        w-pds-ctr            =    zero
                     move  "#"            to   w-cnt-xcv-flg-sub
                     go to xcv-lbu-pds-999
           else      move  "#"            to   w-pds-mff
                     go to xcv-lbu-pds-999.
       xcv-lbu-pds-200.
      *                      *-----------------------------------------*
      *                      * Se nessun errore                        *
      *                      *-----------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento numero linee lette   *
      *                              *---------------------------------*
           add       1                    to   w-pds-nll              .
      *                              *---------------------------------*
      *                              * Visualizzo numero linee lette   *
      *                              *---------------------------------*
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
      *                          *-------------------------------------*
      *                          * Test se prima linea letta           *
      *                          *-------------------------------------*
           if        w-pds-ctr            >    zero
                     go to xcv-lbu-pds-225.
      *                          *-------------------------------------*
      *                          * Se prima linea letta nella pagina   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Normalizzazione area pagina     *
      *                              *---------------------------------*
           move      spaces               to   w-pds-pag              .
      *                              *---------------------------------*
      *                              * Incremento numero pagine lette  *
      *                              *---------------------------------*
           add       1                    to   w-pds-npl              .
      *                              *---------------------------------*
      *                              * Visualizzo numero pagine lette  *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-npl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-lbu-pds-225.
      *                          *-------------------------------------*
      *                          * Se non prima linea letta            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento numero linee lette   *
      *                              *---------------------------------*
           add       1                    to   w-pds-ctr              .
      *                              *---------------------------------*
      *                              * Bufferizzazione linea letta     *
      *                              *---------------------------------*
           move      g-rec                to   w-pds-rig
                                              (w-pds-ctr)             .
       xcv-lbu-pds-250.
      *                          *-------------------------------------*
      *                          * Test se raggiunta l'ultima linea    *
      *                          *-------------------------------------*
           if        w-pds-ctr            =    w-pds-max
                     go to xcv-lbu-pds-300.
       xcv-lbu-pds-275.
      *                          *-------------------------------------*
      *                          * Se non raggiunta l'ultima linea     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Riciclo a lettura linea         *
      *                              *---------------------------------*
           go to     xcv-lbu-pds-125.
       xcv-lbu-pds-300.
      *                          *-------------------------------------*
      *                          * Se raggiunta l'ultima linea         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     xcv-lbu-pds-999.
       xcv-lbu-pds-400.
      *              *-------------------------------------------------*
      *              * Se pagina a formato variabile, con riconosci-   *
      *              * mento dell'inizio pagina                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero linee lette per la pagina : zero     *
      *                  *---------------------------------------------*
           move      zero                 to   w-pds-ctr              .
      *                  *---------------------------------------------*
      *                  * Test se flag di inizio pagina in sospeso a  *
      *                  * "9"                                         *
      *                  *---------------------------------------------*
           if        w-pds-mip            not  = "9"
                     go to xcv-lbu-pds-415.
      *                  *---------------------------------------------*
      *                  * Se flag di inizio pagina in sospeso a "9"   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di fine file                       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-xcv-flg-sub      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     xcv-lbu-pds-999.
       xcv-lbu-pds-415.
      *                  *---------------------------------------------*
      *                  * Test se flag di inizio pagina in sospeso a  *
      *                  * "2"                                         *
      *                  *---------------------------------------------*
           if        w-pds-mip            not  = "2"
                     go to xcv-lbu-pds-425.
      *                  *---------------------------------------------*
      *                  * Se flag di inizio pagina in sospeso a "2"   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di inizio pagina in sospeso a "0"  *
      *                      *-----------------------------------------*
           move      "0"                  to   w-pds-mip              .
      *                      *-----------------------------------------*
      *                      * Simulazione di lettura per ripresa dal  *
      *                      * buffer di salvataggio                   *
      *                      *-----------------------------------------*
           move      w-pds-lip            to   g-rec                  .
           move      e-not-err            to   g-sts                  .
      *                      *-----------------------------------------*
      *                      * Continuazione a dopo lettura            *
      *                      *-----------------------------------------*
           go to     xcv-lbu-pds-450.
       xcv-lbu-pds-425.
      *                  *---------------------------------------------*
      *                  * Lettura linea                               *
      *                  *---------------------------------------------*
           move      "GN"                 to   g-ope                  .
           call      "swd/mod/prg/obj/mcvinp"
                                         using g                      .
       xcv-lbu-pds-450.
      *                  *---------------------------------------------*
      *                  * Test se errori                              *
      *                  *---------------------------------------------*
           if        g-sts                =    e-not-err
                     go to xcv-lbu-pds-600.
           if        g-sts                =    e-end-fil
                     go to xcv-lbu-pds-500.
       xcv-lbu-pds-475.
      *                  *---------------------------------------------*
      *                  * Se errore di i-o                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status di uscita da routine             *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-xcv-flg-sub      .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore sull'input          *
      *                      *-----------------------------------------*
           perform   msg-inp-err-000      thru msg-inp-err-999        .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     xcv-lbu-pds-999.
       xcv-lbu-pds-500.
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        w-pds-ctr            =    zero
                     move  "#"            to   w-cnt-xcv-flg-sub
                     go to xcv-lbu-pds-999
           else      move  "9"            to   w-pds-mip
                     go to xcv-lbu-pds-999.
       xcv-lbu-pds-600.
      *                  *---------------------------------------------*
      *                  * Se nessun errore                            *
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
      *                      *-----------------------------------------*
      *                      * Test se linea di inizio pagina          *
      *                      *-----------------------------------------*
       xcv-lbu-pds-800.
      *                      *-----------------------------------------*
      *                      * Se linea di inizio pagina               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su flag di inizio pagina       *
      *                          *-------------------------------------*
           if        w-pds-mip            =    "0"
                     go to xcv-lbu-pds-810
           else      go to xcv-lbu-pds-825.
       xcv-lbu-pds-810.
      *                          *-------------------------------------*
      *                          * Se flag di inizio pagina a "0"      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Flag di inizio pagina a "1"     *
      *                              *---------------------------------*
           move      "1"                  to   w-pds-mip              .
      *                              *---------------------------------*
      *                              * Normalizzazione area pagina     *
      *                              *---------------------------------*
           move      spaces               to   w-pds-pag              .
      *                              *---------------------------------*
      *                              * Incremento numero pagine        *
      *                              *---------------------------------*
           add       1                    to   w-pds-npl              .
      *                              *---------------------------------*
      *                              * Visualizzo numero pagine        *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      19                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-npl            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-lbu-pds-815.
      *                              *---------------------------------*
      *                              * Test se entro max linee         *
      *                              *---------------------------------*
           if        w-pds-ctr            <    96
                     go to xcv-lbu-pds-820.
      *                              *---------------------------------*
      *                              * Se no                           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero linee i-  *
      *                                  * gnorate                     *
      *                                  *-----------------------------*
           add       1                    to   w-pds-nli              .
      *                                  *-----------------------------*
      *                                  * Visualizzo numero linee i-  *
      *                                  * gnorate                     *
      *                                  *-----------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-nli            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Riciclo a lettura ignorando *
      *                                  * la linea letta              *
      *                                  *-----------------------------*
           go to     xcv-lbu-pds-425.
       xcv-lbu-pds-820.
      *                              *---------------------------------*
      *                              * Se si'                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Incremento numero linee     *
      *                                  * lette per la pagina         *
      *                                  *-----------------------------*
           add       1                    to   w-pds-ctr              .
      *                                  *-----------------------------*
      *                                  * Bufferizzo linea letta      *
      *                                  *-----------------------------*
           move      g-rec                to   w-pds-rig
                                              (w-pds-ctr)             .
      *                                  *-----------------------------*
      *                                  * Riciclo a lettura           *
      *                                  *-----------------------------*
           go to     xcv-lbu-pds-425.
       xcv-lbu-pds-825.
      *                          *-------------------------------------*
      *                          * Se flag di inizio pagina a "1"      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Flag di inizio pagina a "2"     *
      *                              *---------------------------------*
           move      "2"                  to   w-pds-mip              .
      *                              *---------------------------------*
      *                              * Salvataggio linea letta         *
      *                              *---------------------------------*
           move      g-rec                to   w-pds-lip              .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     xcv-lbu-pds-999.
       xcv-lbu-pds-900.
      *                      *-----------------------------------------*
      *                      * Se non linea di inizio pagina           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su flag di inizio pagina       *
      *                          *-------------------------------------*
           if        w-pds-mip            =    "0"
                     go to xcv-lbu-pds-910
           else      go to xcv-lbu-pds-915.
       xcv-lbu-pds-910.
      *                          *-------------------------------------*
      *                          * Se flag di inizio pagina a "0"      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Incremento numero linee ignora- *
      *                              * te                              *
      *                              *---------------------------------*
           add       1                    to   w-pds-nli              .
      *                              *---------------------------------*
      *                              * Visualizzo numero linee ignora- *
      *                              * te                              *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      18                   to   v-lin                  .
           move      35                   to   v-pos                  .
           move      w-pds-nli            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Riciclo a lettura ignorando la  *
      *                              * linea letta                     *
      *                              *---------------------------------*
           go to     xcv-lbu-pds-425.
       xcv-lbu-pds-915.
      *                          *-------------------------------------*
      *                          * Se flag di inizio pagina a "1"      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * A bufferizzazione linea letta   *
      *                              *---------------------------------*
           go to     xcv-lbu-pds-815.
       xcv-lbu-pds-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di conversione : Trattamento pagina di stampa       *
      *    *-----------------------------------------------------------*
       xcv-trt-pds-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-xcv-flg-int      .
       xcv-trt-pds-100.
      *              *-------------------------------------------------*
      *              * Da buffer a file di output                      *
      *              *-------------------------------------------------*
           move      w-pds-pag            to   w-lds-000              .
      *              *-------------------------------------------------*
      *              * Trattamento pagina                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se incontrata una intestazione         *
      *                  *---------------------------------------------*
           if        w-lds-000 (001:20)   =    "--------------------"
                     go to xcv-trt-pds-900.
           if        w-lds-000 (001:20)   =    "Codice Ragione Socia"
                     go to xcv-trt-pds-900.
           if        w-lds-000 (001:20)   =    "       Indirizzo    "
                     go to xcv-trt-pds-900.
           if        w-lds-000 (001:20)   =    "       Indirizzo Con"
                     go to xcv-trt-pds-900.
           if        w-lds-000 (001:20)   =    " DITTA :   1  ZATTI "
                     go to xcv-trt-pds-900.
           if        w-lds-000 (052:20)   =    ">>>   ARCHIVIO FORNI"
                     go to xcv-trt-pds-900.
      *                  *---------------------------------------------*
      *                  * Test se inizio cliente                      *
      *                  *---------------------------------------------*
           if        w-lds-000 (001:05)   not  = spaces and
                     w-lds-000 (006:01)   =    spaces 
                     go to xcv-trt-pds-120.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           if        w-det-csc-pag-c02    =    1
                     add 1                to   w-det-csc-pag-c01      .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del contatore        *
      *                  *---------------------------------------------*
           if        w-det-csc-pag-c01    =    2
                     go to xcv-trt-pds-200.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     xcv-trt-pds-900.
       xcv-trt-pds-120.
      *                  *---------------------------------------------*
      *                  * Inizio cliente                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Contatore per linee cliente             *
      *                      *-----------------------------------------*
           move      1                    to   w-det-csc-pag-c01      .
           move      1                    to   w-det-csc-pag-c02      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-lds-000 (001:05)   to   w-cli-cod-cof          .
           if        w-cli-cod-cof        not  numeric
                     move  zero           to   w-cli-cod-cof          .
      *                          *-------------------------------------*
      *                          * Se codice a zero : a linea succes-  *
      *                          * siva                                *
      *                          *-------------------------------------*
           if        w-cli-cod-cof        =    zero
                     go to xcv-trt-pds-600.
      *                          *-------------------------------------*
      *                          * Editing per messaggi a rullino      *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-cli-cod-cof        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-cli-cod-cof-edt      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-lds-000 (007:30)   to   w-cli-rag-soc-001      .
           move      w-lds-000 (037:25)   to   w-cli-rag-soc-002      .
      *                          *-------------------------------------*
      *                          * Per commerciale                     *
      *                          *-------------------------------------*
           move      w-cli-rag-soc-001    to   w-dcc-rs1-doc          .
           move      w-cli-rag-soc-002    to   w-dcc-rs2-doc          .
      *                          *-------------------------------------*
      *                          * Assemblaggio                        *
      *                          *-------------------------------------*
           move      60                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      w-cli-rag-soc-001    to   w-all-str-cat (1)      .
           move      w-cli-rag-soc-002    to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-cli-rag-soc          .
           move      w-cli-rag-soc        to   w-dcc-rag-dcc          .
      *                      *-----------------------------------------*
      *                      * Partita Iva                             *
      *                      *-----------------------------------------*
           move      w-lds-000 (062:11)   to   w-cli-prt-iva          .
      *                      *-----------------------------------------*
      *                      * Banca appoggio                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrittivo ABI - CAB               *
      *                          *-------------------------------------*
           move      w-lds-000 (074:35)   to   w-dcc-des-abi          .
      *                          *-------------------------------------*
      *                          * CAB                                 *
      *                          *-------------------------------------*
           move      w-lds-000 (111:05)   to   w-dcc-cod-cab          .
      *                          *-------------------------------------*
      *                          * ABI                                 *
      *                          *-------------------------------------*
           move      w-lds-000 (118:05)   to   w-dcc-cod-abi          .
      *                      *-----------------------------------------*
      *                      * Zona                                    *
      *                      *-----------------------------------------*
           move      w-lds-000 (124:02)   to   w-dcc-cod-zon          .
      *                      *-----------------------------------------*
      *                      * Agente                                  *
      *                      *-----------------------------------------*
           move      w-lds-000 (129:02)   to   w-dcc-cod-age          .
           if        w-dcc-cod-age        not  numeric
                     move  zero           to   w-dcc-cod-age          .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     xcv-trt-pds-900.
       xcv-trt-pds-200.
      *                  *---------------------------------------------*
      *                  * Segue cliente 1                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      w-lds-000 (007:30)   to   w-cli-via-cli          .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-cli-via-cli        to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-cli-via-cli          .
           move      w-cli-via-cli        to   w-dcc-via-dcc          .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      w-lds-000 (039:34)   to   w-cli-loc-cli          .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-cli-loc-cli        to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-cli-loc-cli          .
           move      w-cli-loc-cli        to   w-dcc-loc-dcc          .
      *                      *-----------------------------------------*
      *                      * Telefono                                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Prefisso                            *
      *                          *-------------------------------------*
           move      w-lds-000 (074:05)   to   w-cli-pre-tel          .
           inspect   w-cli-pre-tel   replacing all "/"
                                          by   spaces                 .
      *                          *-------------------------------------*
      *                          * Numero                              *
      *                          *-------------------------------------*
           move      w-lds-000 (079:09)   to   w-cli-num-tel          .
           inspect   w-cli-num-tel   replacing all "/"
                                          by   spaces                 .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra numero      *
      *                          *-------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      w-cli-num-tel        to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-cli-num-tel          .
      *                          *-------------------------------------*
      *                          * Assemblaggio prefisso / numero      *
      *                          *-------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-cli-pre-tel        to   w-all-str-cat (1)      .
           if        w-cli-pre-tel        =    spaces or
                     w-cli-num-tel        =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
           move      w-cli-num-tel        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-cli-num-tel          .
           move      w-cli-num-tel        to   w-dcc-num-tel          .
       xcv-trt-pds-240.
      *                      *-----------------------------------------*
      *                      * Fax                                     *
      *                      *-----------------------------------------*
           move      w-lds-000 (089:09)   to   w-cli-num-fax          .
           inspect   w-cli-num-fax   replacing all "/"
                                          by   spaces                 .
           if        w-cli-num-fax        =    spaces
                     move  spaces         to   w-dcc-num-fax
                     go to xcv-trt-pds-260.
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      w-cli-num-fax        to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-cli-num-fax          .
      *                          *-------------------------------------*
      *                          * Assemblaggio prefisso / numero      *
      *                          *-------------------------------------*
           move      30                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-cli-pre-tel        to   w-all-str-cat (1)      .
           if        w-cli-pre-tel        =    spaces or
                     w-cli-num-fax        =    spaces
                     move  spaces         to   w-all-str-cat (2)
           else      move  "/"            to   w-all-str-cat (2)      .
           move      w-cli-num-fax        to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-cli-num-fax          .
           move      w-cli-num-fax        to   w-dcc-num-fax          .
       xcv-trt-pds-260.
      *                      *-----------------------------------------*
      *                      * Banca appoggio                          *
      *                      *                                         *
      *                      * ___ uscita provvisoria                  *
      *                      *-----------------------------------------*
           go to     xcv-trt-pds-280.
      *                          *-------------------------------------*
      *                          * ABI                                 *
      *                          *-------------------------------------*
           move      w-lds-000 (077:05)   to   w-dcc-cod-abi          .
      *                          *-------------------------------------*
      *                          * CAB                                 *
      *                          *-------------------------------------*
           move      w-lds-000 (083:05)   to   w-dcc-cod-cab          .
       xcv-trt-pds-280.
      *                      *-----------------------------------------*
      *                      * Forma di pagamento, descrittivo         *
      *                      *-----------------------------------------*
           move      w-lds-000 (108:25)   to   w-dcc-des-fop          .
      *                      *-----------------------------------------*
      *                      * A scritture                             *
      *                      *-----------------------------------------*
           go to     xcv-trt-pds-400.
       xcv-trt-pds-300.
      *                  *---------------------------------------------*
      *                  * Segue cliente 2 (dipendenza)                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indirizzo consegna merci                *
      *                      * Rullino a video                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-dcc-not-g01          .
           move      spaces               to   w-dcc-not-g02          .
           move      spaces               to   w-dcc-not-g03          .
           if        w-lds-000            =    spaces
                     go to xcv-trt-pds-400.
           move      w-lds-000 (007:30)   to   w-dcc-not-g01          .
           move      w-lds-000 (037:30)   to   w-dcc-not-g02          .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      all "*"              to   m-msg                  .
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Fornitore codice : "
                                delimited by size
                     w-cli-cod-cof-edt
                                delimited by spaces
                     " - "      delimited by size
                     w-cli-rag-soc
                                delimited by size
                                          into m-msg                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      " - indirizzo aggiuntivo da inserire"
                                          to m-msg                    .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       xcv-trt-pds-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           move      "IT"                 to   w-cli-cod-naz          .
      *                      *-----------------------------------------*
      *                      * Telex                                   *
      *                      *-----------------------------------------*
           move      spaces               to   w-cli-num-tlx          .
           move      spaces               to   w-dcc-num-tlx          .
      *                      *-----------------------------------------*
      *                      * Interlocutori                           *
      *                      *-----------------------------------------*
           move      spaces               to   w-cli-nom-int          .
           move      spaces               to   w-dcc-nom-int          .
      *                      *-----------------------------------------*
      *                      * Nostro codice come fornitore            *
      *                      *-----------------------------------------*
           move      spaces               to   w-dcc-noi-fnt          .
      *                      *-----------------------------------------*
      *                      * Codice categoria                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcc-cod-cat          .
      *                      *-----------------------------------------*
      *                      * Codice vettore abituale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-dcc-cod-vet          .
           move      zero                 to   w-dcc-cod-vt2          .
           move      spaces               to   w-dcc-abn-vtt          .
       xcv-trt-pds-460.
      *                      *-----------------------------------------*
      *                      * Tentativo di lettura delle coordinate   *
      *                      * geografiche in base al Cap ed alla de-  *
      *                      * nominazione del comune pulita con con-  *
      *                      * trollo sul codice provincia             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se codice nazione : 'IT'       *
      *                          *-------------------------------------*
           if        w-cli-cod-naz        not  = "IT"
                     go to xcv-trt-pds-475.
           move      w-cli-loc-cli        to   w-det-cap-cfl-wrk      .
      *                          *-------------------------------------*
      *                          * Riasemblaggio localita'             *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-det-cap-cfl-wcp    to   w-all-str-cat (1)      .
           move      w-det-cap-cfl-wds    to   w-all-str-cat (2)      .
           move      w-det-cap-cfl-wpr    to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-cli-loc-cli          .
           move      w-cli-loc-cli        to   w-dcc-loc-dcc          .
      *                          *-------------------------------------*
      *                          * Riasemblaggio localita'             *
      *                          *-------------------------------------*
           if        w-det-cap-cfl-wws    =    "1"
                     move  "00"           to   w-det-cap-cfl-wwp      .
           move      w-det-cap-cfl-wcp    to   w-det-cap-cfl-cap      .
           move      w-det-cap-cfl-wds    to   w-det-cap-cfl-cfl      .
           move      w-det-cap-cfl-wpr    to   w-det-cap-cfl-prv      .
           perform   det-cap-cfl-000      thru det-cap-cfl-999        .
      *                          *-------------------------------------*
      *                          * Test su esito della lettura         *
      *                          *-------------------------------------*
           if        w-det-cap-cfl-flg    not  = spaces
                     go to xcv-trt-pds-475.
       xcv-trt-pds-465.
      *                          *-------------------------------------*
      *                          * Se trovato                          *
      *                          *-------------------------------------*
           move      w-det-cap-cfl-cmn    to   w-cli-cod-cmn          .
           move      w-det-cap-cfl-fzn    to   w-cli-cod-fzn          .
           move      w-det-cap-cfl-lct    to   w-cli-cod-lct          .
           move      w-det-cap-cfl-cmn    to   w-dcc-cod-cmn          .
           move      w-det-cap-cfl-fzn    to   w-dcc-cod-fzn          .
           move      w-det-cap-cfl-lct    to   w-dcc-cod-lct          .
      *                              *---------------------------------*
      *                              * A controlli                     *
      *                              *---------------------------------*
           go to     xcv-trt-pds-500.
       xcv-trt-pds-475.
      *                          *-------------------------------------*
      *                          * Se non trovato nulla                *
      *                          *-------------------------------------*
           move      zero                 to   w-cli-cod-cmn          .
           move      zero                 to   w-cli-cod-fzn          .
           move      zero                 to   w-cli-cod-lct          .
           move      zero                 to   w-dcc-cod-cmn          .
           move      zero                 to   w-dcc-cod-fzn          .
           move      zero                 to   w-dcc-cod-lct          .
      *                              *---------------------------------*
      *                              * A controlli                     *
      *                              *---------------------------------*
           go to     xcv-trt-pds-500.
       xcv-trt-pds-500.
      *              *-------------------------------------------------*
      *              * Controlli pre-scritture                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul codice                             *
      *                  *---------------------------------------------*
           if        w-cli-cod-cof        =    zero
                     go to xcv-trt-pds-600.
      *                  *---------------------------------------------*
      *                  * Test su ragione sociale                     *
      *                  *---------------------------------------------*
           if        w-cli-rag-soc        =    spaces
                     go to xcv-trt-pds-600.
       xcv-trt-pds-510.
      *                  *---------------------------------------------*
      *                  * Descrittivo A.B.I. - C.A.B.                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-dcc-des-abi        =    spaces
                     go to xcv-trt-pds-520.
           if        w-dcc-cod-abi        not  = zero
                     go to xcv-trt-pds-520.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      all "*"              to   m-msg                  .
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Fornitore codice : "
                                delimited by size
                     w-cli-cod-cof-edt
                                delimited by spaces
                     " - "      delimited by size
                     w-cli-rag-soc
                                delimited by size
                                          into m-msg                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      " - banca di appoggio non individuabile :"
                                          to m-msg                    .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    " - Descrittivo : "
                                delimited by size
                     w-dcc-des-abi
                                delimited by size
                                          into m-msg                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       xcv-trt-pds-520.
      *                  *---------------------------------------------*
      *                  * Forma di pagamento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-dcc-cod-fop        not  = zero
                     go to xcv-trt-pds-530.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      all "*"              to   m-msg                  .
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Fornitore codice : "
                                delimited by size
                     w-cli-cod-cof-edt
                                delimited by spaces
                     " - "      delimited by size
                     w-cli-rag-soc
                                delimited by size
                                          into m-msg                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      " - Forma di pagamento non identificata !"
                                          to m-msg                    .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       xcv-trt-pds-530.
      *                  *---------------------------------------------*
      *                  * Ragione sociale oltre 60 caratteri          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           move      w-cli-rag-soc        to   w-det-rag-soc          .
           if        w-det-rag-soc-rov    =    spaces
                     go to xcv-trt-pds-540.
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      all "*"              to   m-msg                  .
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Fornitore codice : "
                                delimited by size
                     w-cli-cod-cof-edt
                                delimited by spaces
                     " - "      delimited by size
                     w-cli-rag-soc
                                delimited by size
                                          into m-msg                  .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      " - Ragione sociale oltre 60 caratteri !"
                                          to m-msg                    .
      *                      *-----------------------------------------*
      *                      * Emissione messaggio per il rullino      *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       xcv-trt-pds-540.
      *                  *---------------------------------------------*
      *                  * Fido cliente                                *
      *                  *---------------------------------------------*
       xcv-trt-pds-550.
      *                  *---------------------------------------------*
      *                  * Estremi esenzione                           *
      *                  *---------------------------------------------*
       xcv-trt-pds-580.
      *              *-------------------------------------------------*
      *              * Scritture                                       *
      *              *-------------------------------------------------*
           go to     xcv-trt-pds-584.
       xcv-trt-pds-582.
      *                  *---------------------------------------------*
      *                  * Cliente                                     *
      *                  *---------------------------------------------*
           move      0                    to   w-det-csc-pag-c02      .
      *                      *-----------------------------------------*
      *                      * Write record [cli]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-cli-000      thru wrt-rec-cli-999        .
      *                      *-----------------------------------------*
      *                      * Write record [dcc]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-dcc-000      thru wrt-rec-dcc-999        .
      *                      *-----------------------------------------*
      *                      * INIBITO                                 *
      *                      *-----------------------------------------*
           go to xcv-trt-pds-583.
      *                      *-----------------------------------------*
      *                      * Write record [dcc] per la dipendenza    *
      *                      *-----------------------------------------*
           perform   wrt-rec-dcc-dpz-000  thru wrt-rec-dcc-dpz-999    .
       xcv-trt-pds-583.
      *                      *-----------------------------------------*
      *                      * A incremento contatore                  *
      *                      *-----------------------------------------*
           go to     xcv-trt-pds-586.
       xcv-trt-pds-584.
      *                  *---------------------------------------------*
      *                  * Fornitore                                   *
      *                  *---------------------------------------------*
           move      0                    to   w-det-csc-pag-c02      .
      *                      *-----------------------------------------*
      *                      * Write record [fnt]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-fnt-000      thru wrt-rec-fnt-999        .
      *                      *-----------------------------------------*
      *                      * Write record [dcf]                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-dcf-000      thru wrt-rec-dcf-999        .
      *                      *-----------------------------------------*
      *                      * A incremento contatore                  *
      *                      *-----------------------------------------*
           go to     xcv-trt-pds-586.
       xcv-trt-pds-586.
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti           *
      *                  *---------------------------------------------*
           add       1                    to   w-pds-cof              .
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
           move      w-pds-cof            to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       xcv-trt-pds-600.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     xcv-trt-pds-900.
       xcv-trt-pds-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     xcv-trt-pds-999.
       xcv-trt-pds-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [cli]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-cli-cod-cof        to   rf-cli-cod-cli         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-cli-ide-dat         .
           move      "master"             to   rf-cli-ide-ute         .
           move      s-fas                to   rf-cli-ide-fas         .
           move      spaces               to   rf-cli-cod-mne         .
           move      w-cli-rag-soc        to   rf-cli-rag-soc         .
      *                      *-----------------------------------------*
      *                      * Ragione sociale in uppercase            *
      *                      *-----------------------------------------*
           move      w-cli-rag-soc        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   rf-cli-rag-key         .
           move      w-cli-via-cli        to   rf-cli-via-cli         .
           move      w-cli-loc-cli        to   rf-cli-loc-cli         .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           move      w-cli-cod-naz        to   rf-cli-cod-naz         .
           if        rf-cli-cod-naz       =    spaces
                     move  "IT "          to   rf-cli-cod-naz         .
      *                      *-----------------------------------------*
      *                      * Coordinate geografiche                  *
      *                      *-----------------------------------------*
           move      w-cli-cod-cmn        to   rf-cli-cod-cmn         .
           move      w-cli-cod-fzn        to   rf-cli-cod-fzn         .
           move      w-cli-cod-lct        to   rf-cli-cod-lct         .
      *                      *-----------------------------------------*
      *                      * Telefono, ecc.                          *
      *                      *-----------------------------------------*
           move      w-cli-num-tel        to   rf-cli-num-tel         .
           move      w-cli-num-fax        to   rf-cli-num-fax         .
           move      w-cli-num-tlx        to   rf-cli-num-tlx         .
           move      w-cli-nom-int        to   rf-cli-nom-int         .
      *                      *-----------------------------------------*
      *                      * Codice Iva                              *
      *                      *-----------------------------------------*
           move      zero                 to   rf-cli-cod-iva         .
      *                      *-----------------------------------------*
      *                      * Partita Iva                             *
      *                      *-----------------------------------------*
           move      w-cli-prt-iva        to   rf-cli-prt-iva         .
      *                      *-----------------------------------------*
      *                      * Codice fiscale                          *
      *                      *-----------------------------------------*
           move      w-cli-cod-fis        to   rf-cli-cod-fis         .
      *                      *-----------------------------------------*
      *                      * In allegato                             *
      *                      *-----------------------------------------*
           move      "S"                  to   rf-cli-snx-a13         .
      *                      *-----------------------------------------*
      *                      * Contropartita clienti                   *
      *                      * ___ default ___                         *
      *                      *-----------------------------------------*
           move      2010010              to   rf-cli-cod-cge         .
      *                      *-----------------------------------------*
      *                      * Area libera                             *
      *                      *-----------------------------------------*
           move      spaces               to   rf-cli-alx-exp         .
       cmp-rec-cli-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [cli]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-cli-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-cli-000      thru cmp-rec-cli-999        .
      *              *-------------------------------------------------*
      *              * Test sul codice                                 *
      *              *-------------------------------------------------*
           if        rf-cli-cod-cli       =    zero
                     go to wrt-rec-cli-999.
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
       wrt-rec-cli-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dcc]                                 *
      *    *                                                           *
      *    * Cliente principale                                        *
      *    *-----------------------------------------------------------*
       cmp-rec-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       cmp-rec-dcc-010.
      *              *-------------------------------------------------*
      *              * Informazioni generali                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data, utente, fase, di ultimo inserimento o *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dcc-ide-dat         .
           move      "master  "           to   rf-dcc-ide-ute         .
           move      s-fas                to   rf-dcc-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      w-cli-cod-cof        to   rf-dcc-cod-cli         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-dpz-cli         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale in uppercase                *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   rf-dcc-rag-key         .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      w-dcc-via-dcc        to   rf-dcc-via-dcc         .
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-dcc-loc-dcc        to   rf-dcc-loc-dcc         .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      w-cli-cod-naz        to   rf-dcc-cod-naz         .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-cmn        to   rf-dcc-cod-cmn         .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-dcc-cod-fzn        to   rf-dcc-cod-fzn         .
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           move      w-dcc-cod-lct        to   rf-dcc-cod-lct         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale per invio documento         *
      *                  *---------------------------------------------*
           move      w-dcc-rs1-doc        to   rf-dcc-rs1-doc         .
           move      w-dcc-rs2-doc        to   rf-dcc-rs2-doc         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 1             *
      *                  *---------------------------------------------*
           move      w-dcc-not-g01        to   rf-dcc-not-g01         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 2             *
      *                  *---------------------------------------------*
           move      w-dcc-not-g02        to   rf-dcc-not-g02         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 3             *
      *                  *---------------------------------------------*
           move      w-dcc-not-g03        to   rf-dcc-not-g03         .
      *                  *---------------------------------------------*
      *                  * Numero di telefono                          *
      *                  *---------------------------------------------*
           move      w-dcc-num-tel        to   rf-dcc-num-tel         .
      *                  *---------------------------------------------*
      *                  * Numero di telefono alternativo              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-tel-alt         .
      *                  *---------------------------------------------*
      *                  * Nome interlocutore                          *
      *                  *---------------------------------------------*
           move      w-dcc-nom-int        to   rf-dcc-nom-int         .
      *                  *---------------------------------------------*
      *                  * Nome interlocutore alternativo              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-int-alt         .
      *                  *---------------------------------------------*
      *                  * Numero di telefax                           *
      *                  *---------------------------------------------*
           move      w-dcc-num-fax        to   rf-dcc-num-fax         .
      *                  *---------------------------------------------*
      *                  * Numero di telex                             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-tlx         .
      *                  *---------------------------------------------*
      *                  * Numero PT-Postel                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-ptp         .
      *                  *---------------------------------------------*
      *                  * Numero PT-Fax                               *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-ptf         .
      *                  *---------------------------------------------*
      *                  * Numero modem                                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-tel-mdm         .
      *                  *---------------------------------------------*
      *                  * Numero posta elettronica                    *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-idn-ema         .
      *                  *---------------------------------------------*
      *                  * Inoltro documenti                           *
      *                  *                                             *
      *                  *  - '01' : Alla sede                         *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-inl-dcm         .
      *                  *---------------------------------------------*
      *                  * Inoltro pagamenti                           *
      *                  *                                             *
      *                  *  - '01' : Alla sede                         *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-inl-pgt         .
      *                  *---------------------------------------------*
      *                  * Nostro codice come fornitore                *
      *                  *---------------------------------------------*
           move      w-dcc-noi-fnt        to   rf-dcc-noi-fnt         .
      *                  *---------------------------------------------*
      *                  * Modalita' di stampa della fattura           *
      *                  *                                             *
      *                  *  - '00' : Non significatico                 *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-mod-sft         .
       cmp-rec-dcc-100.
      *              *-------------------------------------------------*
      *              * Informazioni per la fatturazione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento Iva                    *
      *                  *                                             *
      *                  * - 01 : Sempre soggetto ad iva               *
      *                  * - 02 : Sempre esente da iva, secondo il co- *
      *                  *        dice di esenzione espresso nell'ana- *
      *                  *        grafica cliente di contabilita' ge-  *
      *                  *        nerale                               *
      *                  * - 03 : Cliente con plafond, quindi soggetto *
      *                  *        oppure esente a seconda della forni- *
      *                  *        tura, fermo restando che in caso di  *
      *                  *        esenzione il codice di esenzione e'  *
      *                  *        quello di cui sopra                  *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-tas-ivc         .
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-ctp-ven         .
      *                  *---------------------------------------------*
      *                  * Tipo documento preferenziale per consegna   *
      *                  *                                             *
      *                  *  - '01' : Nessuna preferenza                *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-tdp-plc         .
      *                  *---------------------------------------------*
      *                  * Periodicita' di fatturazione                *
      *                  *                                             *
      *                  *  - '05' : Mensile                           *
      *                  *---------------------------------------------*
           move      05                   to   rf-dcc-per-fat         .
      *                  *---------------------------------------------*
      *                  * Raggruppamento bolle in fattura             *
      *                  *                                             *
      *                  * - '01' : Una fattura per tutte le bolle     *
      *                  *---------------------------------------------*
           if        w-dcc-rag-bft        =    spaces
                     move  01             to   rf-dcc-rag-bft
           else      move  03             to   rf-dcc-rag-bft         .
      *                  *---------------------------------------------*
      *                  * Tipo di esposizione prezzi                  *
      *                  *                                             *
      *                  * - '01' : Standard                           *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-epz-pes         .
      *                  *---------------------------------------------*
      *                  * Codice valuta                               *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-dcc-cod-vlt         .
      *                  *---------------------------------------------*
      *                  * Momento di applicazione del cambio valuta   *
      *                  *                                             *
      *                  * - '01' : Standard                           *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-mom-acv         .
      *                  *---------------------------------------------*
      *                  * Si/No rapporti con legame valutario         *
      *                  *                                             *
      *                  * - 'S'      : Si                             *
      *                  *---------------------------------------------*
           move      "S"                  to   rf-dcc-snx-rlv         .
      *                  *---------------------------------------------*
      *                  * Momento di applicazione legame valutario    *
      *                  *                                             *
      *                  * - '01' : Standard                           *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-mom-alv         .
      *                  *---------------------------------------------*
      *                  * Codice lingua                               *
      *                  *---------------------------------------------*
           move      "I  "                to   rf-dcc-cod-lng         .
      *                  *---------------------------------------------*
      *                  * Tipo fornitura abituale                     *
      *                  *                                             *
      *                  * - '01' : Diretta                            *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-tip-frn         .
      *                  *---------------------------------------------*
      *                  * Codice archivio per fatturazione            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-arc-plf         .
      *                  *---------------------------------------------*
      *                  * Dipendenza archivio per fatturazione        *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-dpz-plf         .
      *                  *---------------------------------------------*
      *                  * Tipo fatturazione                           *
      *                  *                                             *
      *                  * - '00' : Non significativo                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tip-ftz         .
       cmp-rec-dcc-200.
      *              *-------------------------------------------------*
      *              * Informazioni per le condizioni di vendita       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-lst         .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in riga                    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-scr         .
      *                  *---------------------------------------------*
      *                  * % sconto in riga                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-scr (1)     .
           move      zero                 to   rf-dcc-per-scr (2)     .
           move      zero                 to   rf-dcc-per-scr (3)     .
           move      zero                 to   rf-dcc-per-scr (4)     .
           move      zero                 to   rf-dcc-per-scr (5)     .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in chiusura                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-scc         .
      *                  *---------------------------------------------*
      *                  * % sconto in chiusura                        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-scc         .
      *                  *---------------------------------------------*
      *                  * Addebito spese e modalita'                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-snm-spe (1)     .
           move      zero                 to   rf-dcc-snm-spe (2)     .
           move      zero                 to   rf-dcc-snm-spe (3)     .
           move      zero                 to   rf-dcc-snm-spe (4)     .
           move      zero                 to   rf-dcc-snm-spe (5)     .
           move      zero                 to   rf-dcc-snm-spe (6)     .
           move      zero                 to   rf-dcc-per-spe (1)     .
           move      zero                 to   rf-dcc-per-spe (2)     .
           move      zero                 to   rf-dcc-per-spe (3)     .
           move      zero                 to   rf-dcc-per-spe (4)     .
           move      zero                 to   rf-dcc-per-spe (5)     .
           move      zero                 to   rf-dcc-per-spe (6)     .
           move      zero                 to   rf-dcc-imp-spe (1)     .
           move      zero                 to   rf-dcc-imp-spe (2)     .
           move      zero                 to   rf-dcc-imp-spe (3)     .
           move      zero                 to   rf-dcc-imp-spe (4)     .
           move      zero                 to   rf-dcc-imp-spe (5)     .
           move      zero                 to   rf-dcc-imp-spe (6)     .
      *                  *---------------------------------------------*
      *                  * Voci descrittive in fattura                 *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-vde-cod (1)     .
           move      spaces               to   rf-dcc-vde-cod (2)     .
           move      spaces               to   rf-dcc-vde-cod (3)     .
           move      spaces               to   rf-dcc-vde-cod (4)     .
           move      spaces               to   rf-dcc-vde-cod (5)     .
           move      spaces               to   rf-dcc-vde-cod (6)     .
       cmp-rec-dcc-300.
      *              *-------------------------------------------------*
      *              * Informazioni per le condizioni di pagamento     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo esclusione mesi                        *
      *                  *                                             *
      *                  * - 01 : Nessun mese escluso                  *
      *                  * - 02 : Si sposta la scadenza di un mese     *
      *                  * - 03 : Si sposta la scadenza ad un giorno   *
      *                  *        fisso del mese successivo            *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-tip-esm         .
           move      zero                 to   rf-dcc-ggg-alt         .
           move      zero                 to   rf-dcc-mmm-e01         .
           move      zero                 to   rf-dcc-mmm-e02         .
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese incasso          *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-add-spi         .
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese bollo            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-add-spb         .
      *                  *---------------------------------------------*
      *                  * Codice A.B.I.                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-abi        to   rf-dcc-cod-abi         .
      *                  *---------------------------------------------*
      *                  * Codice C.A.B.                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-cab        to   rf-dcc-cod-cab         .
       cmp-rec-dcc-340.
      *                  *---------------------------------------------*
      *                  * Conto corrente di appoggio                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-ccc-app         .
      *                  *---------------------------------------------*
      *                  * Codice CIN                                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-cin         .
      *                  *---------------------------------------------*
      *                  * Codice nostra banca per pagamenti per mezzo *
      *                  * bonifico bancario                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-ban         .
      *                  *---------------------------------------------*
      *                  * Codice nostra banca per presentazione ef-   *
      *                  * fetti                                       *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-bpe         .
      *                  *---------------------------------------------*
      *                  * Codice nostro c/c postale per pagamenti per *
      *                  * mezzo c/c postale                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-ccp         .
      *                  *---------------------------------------------*
      *                  * Tipo incasso preferenziale                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-ipr-iel         .
       cmp-rec-dcc-400.
      *              *-------------------------------------------------*
      *              * Informazioni per la gestione agenti             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-age        to   rf-dcc-cod-age         .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigioni legata al cliente  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-pvg         .
      *                  *---------------------------------------------*
      *                  * % provvigioni legate al cliente             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-pvg (1)     .
           move      zero                 to   rf-dcc-per-pvg (2)     .
           move      zero                 to   rf-dcc-per-pvg (3)     .
       cmp-rec-dcc-500.
      *              *-------------------------------------------------*
      *              * Informazioni per le classificazioni statistiche *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice zona                                 *
      *                  *---------------------------------------------*
           move      w-dcc-cod-zon        to   rf-dcc-cod-zon         .
      *                  *---------------------------------------------*
      *                  * Codice categoria                            *
      *                  *---------------------------------------------*
           move      w-dcc-cod-cat        to   rf-dcc-cod-cat         .
      *                  *---------------------------------------------*
      *                  * Codice statistico                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-stt         .
      *                  *---------------------------------------------*
      *                  * Data di acquisizione del cliente            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dat-aqz         .
      *                  *---------------------------------------------*
      *                  * Status commerciale                          *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-sta-tus         .
      *                  *---------------------------------------------*
      *                  * Data di determinazione dello status         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tud         .
      *                  *---------------------------------------------*
      *                  * Codice cliente di riferimento               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tuc         .
      *                  *---------------------------------------------*
      *                  * Modalita' di trattamento nelle statistiche  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tux         .
       cmp-rec-dcc-600.
      *              *-------------------------------------------------*
      *              * Informazioni per il marketing                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe A..Z di importanza                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cld-imp         .
      *                  *---------------------------------------------*
      *                  * Tipo di rapporto commerciale                *
      *                  *---------------------------------------------*
           move      99                   to   rf-dcc-tra-pco         .
      *                  *---------------------------------------------*
      *                  * Grado di introduzione commerciale           *
      *                  *---------------------------------------------*
           move      99                   to   rf-dcc-gra-ico         .
      *                  *---------------------------------------------*
      *                  * Pericolosita' della concorrenza             *
      *                  *---------------------------------------------*
           move      99                   to   rf-dcc-pcl-ccz         .
      *                  *---------------------------------------------*
      *                  * Previsione di continuita'                   *
      *                  *---------------------------------------------*
           move      99                   to   rf-dcc-pre-ctn         .
      *                  *---------------------------------------------*
      *                  * Origine del contatto                        *
      *                  *---------------------------------------------*
           move      99                   to   rf-dcc-org-ctt         .
      *                  *---------------------------------------------*
      *                  * Codice marketing                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-mkt         .
      *                  *---------------------------------------------*
      *                  * Indice marketing                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-ind-mkt         .
       cmp-rec-dcc-700.
      *              *-------------------------------------------------*
      *              * Informazioni per il controllo del budget        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe di budget                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cla-bdg         .
       cmp-rec-dcc-800.
      *              *-------------------------------------------------*
      *              * Informazioni per ordini e bollettazione         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di cliente con ordini bloccati      *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-for-blo         .
      *                  *---------------------------------------------*
      *                  * Data in cui e' avvenuto il blocco ordini    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dor-blo         .
      *                  *---------------------------------------------*
      *                  * Segnale di cliente con consegne bloccate    *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-fco-blo         .
      *                  *---------------------------------------------*
      *                  * Data in cui e' avvenuto il blocco consegne  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dco-blo         .
      *                  *---------------------------------------------*
      *                  * Trasporto a cura abituale                   *
      *                  *                                             *
      *                  * - '01' : Nessuna preferenza                 *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-dtt-acu         .
      *                  *---------------------------------------------*
      *                  * Codice vettore abituale                     *
      *                  *---------------------------------------------*
           move      w-dcc-cod-vet        to   rf-dcc-cod-vet         .
      *                  *---------------------------------------------*
      *                  * 2. vettore abituale, alternativo            *
      *                  *---------------------------------------------*
           move      w-dcc-cod-vt2        to   rf-dcc-cod-vt2         .
      *                  *---------------------------------------------*
      *                  * 3. vettore abituale, alternativo            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-vt3         .
      *                  *---------------------------------------------*
      *                  * Numero di abbonamento                       *
      *                  *---------------------------------------------*
           move      w-dcc-abn-vtt        to   rf-dcc-abn-vtt         .
      *                  *---------------------------------------------*
      *                  * Distanza in chilometri                      *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dst-klm         .
      *                  *---------------------------------------------*
      *                  * Chiusura per ferie, data inizio             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dti-cpf         .
      *                  *---------------------------------------------*
      *                  * Chiusura per ferie, data fine               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dtf-cpf         .
      *                  *---------------------------------------------*
      *                  * Settimana lavorativa                        *
      *                  *---------------------------------------------*
           move      "AAAAACC"            to   rf-dcc-gdl-nls         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura mattino, inizio          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oam-oin         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura mattino, fine            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oam-ofi         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura pomeriggio, inizio       *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oap-oin         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura pomeriggio, fine         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oap-ofi         .
       cmp-rec-dcc-900.
      *              *-------------------------------------------------*
      *              * Informazioni per applicazioni speciali          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-alx-exp         .
       cmp-rec-dcc-990.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-dcc-999.
       cmp-rec-dcc-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dcc]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-dcc-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dcc-000      thru cmp-rec-dcc-999        .
      *              *-------------------------------------------------*
      *              * Test sul codice                                 *
      *              *-------------------------------------------------*
           if        rf-dcc-cod-cli       =    zero
                     go to wrt-rec-dcc-999.
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       wrt-rec-dcc-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dcc] per la dipendenza                  *
      *    *-----------------------------------------------------------*
       wrt-rec-dcc-dpz-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dcc-dpz-000  thru cmp-rec-dcc-dpz-999    .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       wrt-rec-dcc-dpz-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dcc]                                 *
      *    *                                                           *
      *    * Cliente principale                                        *
      *    *-----------------------------------------------------------*
       cmp-rec-dcc-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       cmp-rec-dcc-dpz-010.
      *              *-------------------------------------------------*
      *              * Informazioni generali                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data, utente, fase, di ultimo inserimento o *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dcc-ide-dat         .
           move      "master  "           to   rf-dcc-ide-ute         .
           move      s-fas                to   rf-dcc-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      w-cli-cod-cof        to   rf-dcc-cod-cli         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      "1   "               to   rf-dcc-dpz-cli         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale in uppercase                *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   rf-dcc-rag-key         .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      w-dcc-via-dcc        to   rf-dcc-via-dcc         .
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-dcc-loc-dcc        to   rf-dcc-loc-dcc         .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      w-cli-cod-naz        to   rf-dcc-cod-naz         .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-cmn        to   rf-dcc-cod-cmn         .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-dcc-cod-fzn        to   rf-dcc-cod-fzn         .
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           move      w-dcc-cod-lct        to   rf-dcc-cod-lct         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale per invio documento         *
      *                  *---------------------------------------------*
           move      w-dcc-rs1-doc        to   rf-dcc-rs1-doc         .
           move      w-dcc-rs2-doc        to   rf-dcc-rs2-doc         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 1             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-not-g01         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 2             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-not-g02         .
      *                  *---------------------------------------------*
      *                  * Nota generica sul cliente nr. 3             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-not-g03         .
      *                  *---------------------------------------------*
      *                  * Numero di telefono                          *
      *                  *---------------------------------------------*
           move      w-dcc-num-tel        to   rf-dcc-num-tel         .
      *                  *---------------------------------------------*
      *                  * Numero di telefono alternativo              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-tel-alt         .
      *                  *---------------------------------------------*
      *                  * Nome interlocutore                          *
      *                  *---------------------------------------------*
           move      w-dcc-nom-int        to   rf-dcc-nom-int         .
      *                  *---------------------------------------------*
      *                  * Nome interlocutore alternativo              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-int-alt         .
      *                  *---------------------------------------------*
      *                  * Numero di telefax                           *
      *                  *---------------------------------------------*
           move      w-dcc-num-fax        to   rf-dcc-num-fax         .
      *                  *---------------------------------------------*
      *                  * Numero di telex                             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-tlx         .
      *                  *---------------------------------------------*
      *                  * Numero PT-Postel                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-ptp         .
      *                  *---------------------------------------------*
      *                  * Numero PT-Fax                               *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-num-ptf         .
      *                  *---------------------------------------------*
      *                  * Numero modem                                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-tel-mdm         .
      *                  *---------------------------------------------*
      *                  * Numero posta elettronica                    *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-idn-ema         .
      *                  *---------------------------------------------*
      *                  * Inoltro documenti                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-inl-dcm         .
      *                  *---------------------------------------------*
      *                  * Inoltro pagamenti                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-inl-pgt         .
      *                  *---------------------------------------------*
      *                  * Nostro codice come fornitore                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-noi-fnt         .
      *                  *---------------------------------------------*
      *                  * Modalita' di stampa della fattura           *
      *                  *                                             *
      *                  *  - '00' : Non significatico                 *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-mod-sft         .
       cmp-rec-dcc-dpz-100.
      *              *-------------------------------------------------*
      *              * Informazioni per la fatturazione                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento Iva                    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tas-ivc         .
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-ctp-ven         .
      *                  *---------------------------------------------*
      *                  * Tipo documento preferenziale per consegna   *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tdp-plc         .
      *                  *---------------------------------------------*
      *                  * Periodicita' di fatturazione                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-fat         .
      *                  *---------------------------------------------*
      *                  * Raggruppamento bolle in fattura             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-rag-bft         .
      *                  *---------------------------------------------*
      *                  * Tipo di esposizione prezzi                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-epz-pes         .
      *                  *---------------------------------------------*
      *                  * Codice valuta                               *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-vlt         .
      *                  *---------------------------------------------*
      *                  * Momento di applicazione del cambio valuta   *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-mom-acv         .
      *                  *---------------------------------------------*
      *                  * Si/No rapporti con legame valutario         *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-snx-rlv         .
      *                  *---------------------------------------------*
      *                  * Momento di applicazione legame valutario    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-mom-alv         .
      *                  *---------------------------------------------*
      *                  * Codice lingua                               *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-lng         .
      *                  *---------------------------------------------*
      *                  * Tipo fornitura abituale                     *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tip-frn         .
       cmp-rec-dcc-dpz-200.
      *              *-------------------------------------------------*
      *              * Informazioni per le condizioni di vendita       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-lst         .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in riga                    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-scr         .
      *                  *---------------------------------------------*
      *                  * % sconto in riga                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-scr (1)     .
           move      zero                 to   rf-dcc-per-scr (2)     .
           move      zero                 to   rf-dcc-per-scr (3)     .
           move      zero                 to   rf-dcc-per-scr (4)     .
           move      zero                 to   rf-dcc-per-scr (5)     .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in chiusura                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-scc         .
      *                  *---------------------------------------------*
      *                  * % sconto in chiusura                        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-scc         .
      *                  *---------------------------------------------*
      *                  * Addebito spese e modalita'                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-snm-spe (1)     .
           move      zero                 to   rf-dcc-snm-spe (2)     .
           move      zero                 to   rf-dcc-snm-spe (3)     .
           move      zero                 to   rf-dcc-snm-spe (4)     .
           move      zero                 to   rf-dcc-snm-spe (5)     .
           move      zero                 to   rf-dcc-snm-spe (6)     .
           move      zero                 to   rf-dcc-per-spe (1)     .
           move      zero                 to   rf-dcc-per-spe (2)     .
           move      zero                 to   rf-dcc-per-spe (3)     .
           move      zero                 to   rf-dcc-per-spe (4)     .
           move      zero                 to   rf-dcc-per-spe (5)     .
           move      zero                 to   rf-dcc-per-spe (6)     .
           move      zero                 to   rf-dcc-imp-spe (1)     .
           move      zero                 to   rf-dcc-imp-spe (2)     .
           move      zero                 to   rf-dcc-imp-spe (3)     .
           move      zero                 to   rf-dcc-imp-spe (4)     .
           move      zero                 to   rf-dcc-imp-spe (5)     .
           move      zero                 to   rf-dcc-imp-spe (6)     .
      *                  *---------------------------------------------*
      *                  * Voci descrittive in fattura                 *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-vde-cod (1)     .
           move      spaces               to   rf-dcc-vde-cod (2)     .
           move      spaces               to   rf-dcc-vde-cod (3)     .
           move      spaces               to   rf-dcc-vde-cod (4)     .
           move      spaces               to   rf-dcc-vde-cod (5)     .
           move      spaces               to   rf-dcc-vde-cod (6)     .
       cmp-rec-dcc-dpz-300.
      *              *-------------------------------------------------*
      *              * Informazioni per le condizioni di pagamento     *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Informazioni per le condizioni di pagamento     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice forma di pagamento                   *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcf-cod-fop         .
      *                  *---------------------------------------------*
      *                  * Tipo esclusione mesi                        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tip-esm         .
      *                  *---------------------------------------------*
      *                  * Giorno fisso di scadenza                    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-ggg-alt         .
      *                  *---------------------------------------------*
      *                  * Mesi di esclusione                          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-mmm-e01         .
           move      zero                 to   rf-dcc-mmm-e02         .
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese incasso          *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-add-spi         .
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese bollo            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-add-spb         .
      *                  *---------------------------------------------*
      *                  * Codice A.B.I.                               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-abi         .
      *                  *---------------------------------------------*
      *                  * Codice C.A.B.                               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-cab         .
       cmp-rec-dcc-dpz-340.
      *                  *---------------------------------------------*
      *                  * Conto corrente di appoggio                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-ccc-app         .
      *                  *---------------------------------------------*
      *                  * Codice CIN                                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cod-cin         .
      *                  *---------------------------------------------*
      *                  * Codice nostra banca per pagamenti per mezzo *
      *                  * bonifico bancario                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-ban         .
      *                  *---------------------------------------------*
      *                  * Codice nostra banca per presentazione ef-   *
      *                  * fetti                                       *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-bpe         .
      *                  *---------------------------------------------*
      *                  * Codice nostro c/c postale per pagamenti per *
      *                  * mezzo c/c postale                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-nos-ccp         .
      *                  *---------------------------------------------*
      *                  * Tipo incasso preferenziale                  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-ipr-iel         .
       cmp-rec-dcc-dpz-400.
      *              *-------------------------------------------------*
      *              * Informazioni per la gestione agenti             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-age         .
      *                  *---------------------------------------------*
      *                  * Categoria di provvigioni legata al cliente  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cat-pvg         .
      *                  *---------------------------------------------*
      *                  * % provvigioni legate al cliente             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-per-pvg (1)     .
           move      zero                 to   rf-dcc-per-pvg (2)     .
           move      zero                 to   rf-dcc-per-pvg (3)     .
       cmp-rec-dcc-dpz-500.
      *              *-------------------------------------------------*
      *              * Informazioni per le classificazioni statistiche *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice zona                                 *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-zon         .
      *                  *---------------------------------------------*
      *                  * Codice categoria                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-cat         .
      *                  *---------------------------------------------*
      *                  * Codice statistico                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-stt         .
      *                  *---------------------------------------------*
      *                  * Data di acquisizione del cliente            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dat-aqz         .
      *                  *---------------------------------------------*
      *                  * Status commerciale                          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tus         .
      *                  *---------------------------------------------*
      *                  * Data di determinazione dello status         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tud         .
      *                  *---------------------------------------------*
      *                  * Codice cliente di riferimento               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tuc         .
      *                  *---------------------------------------------*
      *                  * Modalita' di trattamento nelle statistiche  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-sta-tux         .
       cmp-rec-dcc-dpz-600.
      *              *-------------------------------------------------*
      *              * Informazioni per il marketing                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe A..Z di importanza                   *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-cld-imp         .
      *                  *---------------------------------------------*
      *                  * Tipo di rapporto commerciale                *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-tra-pco         .
      *                  *---------------------------------------------*
      *                  * Grado di introduzione commerciale           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-gra-ico         .
      *                  *---------------------------------------------*
      *                  * Pericolosita' della concorrenza             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-pcl-ccz         .
      *                  *---------------------------------------------*
      *                  * Previsione di continuita'                   *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-pre-ctn         .
      *                  *---------------------------------------------*
      *                  * Origine del contatto                        *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-org-ctt         .
      *                  *---------------------------------------------*
      *                  * Codice marketing                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-mkt         .
      *                  *---------------------------------------------*
      *                  * Indice marketing                            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-ind-mkt         .
       cmp-rec-dcc-dpz-700.
      *              *-------------------------------------------------*
      *              * Informazioni per il controllo del budget        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe di budget                            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cla-bdg         .
       cmp-rec-dcc-dpz-800.
      *              *-------------------------------------------------*
      *              * Informazioni per ordini e bollettazione         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di cliente con ordini bloccati      *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-for-blo         .
      *                  *---------------------------------------------*
      *                  * Data in cui e' avvenuto il blocco ordini    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dor-blo         .
      *                  *---------------------------------------------*
      *                  * Segnale di cliente con consegne bloccate    *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-fco-blo         .
      *                  *---------------------------------------------*
      *                  * Data in cui e' avvenuto il blocco consegne  *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dco-blo         .
      *                  *---------------------------------------------*
      *                  * Trasporto a cura abituale                   *
      *                  *                                             *
      *                  * - '01' : Nessuna preferenza                 *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcc-dtt-acu         .
      *                  *---------------------------------------------*
      *                  * Codice vettore abituale                     *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-vet         .
      *                  *---------------------------------------------*
      *                  * 2. vettore abituale, alternativo            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-vt2         .
      *                  *---------------------------------------------*
      *                  * 3. vettore abituale, alternativo            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-cod-vt3         .
      *                  *---------------------------------------------*
      *                  * Numero di abbonamento                       *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-abn-vtt         .
      *                  *---------------------------------------------*
      *                  * Distanza in chilometri                      *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dst-klm         .
      *                  *---------------------------------------------*
      *                  * Chiusura per ferie, data inizio             *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dti-cpf         .
      *                  *---------------------------------------------*
      *                  * Chiusura per ferie, data fine               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-dtf-cpf         .
      *                  *---------------------------------------------*
      *                  * Settimana lavorativa                        *
      *                  *---------------------------------------------*
           move      "AAAAACC"            to   rf-dcc-gdl-nls         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura mattino, inizio          *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oam-oin         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura mattino, fine            *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oam-ofi         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura pomeriggio, inizio       *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oap-oin         .
      *                  *---------------------------------------------*
      *                  * Orario di apertura pomeriggio, fine         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcc-oap-ofi         .
       cmp-rec-dcc-dpz-900.
      *              *-------------------------------------------------*
      *              * Informazioni per applicazioni speciali          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni speciali         *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcc-alx-exp         .
       cmp-rec-dcc-dpz-990.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-dcc-dpz-999.
       cmp-rec-dcc-dpz-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [fnt]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-cli-cod-cof        to   rf-fnt-cod-fnt         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-fnt-ide-dat         .
           move      "master"             to   rf-fnt-ide-ute         .
           move      s-fas                to   rf-fnt-ide-fas         .
           move      spaces               to   rf-fnt-cod-mne         .
           move      w-cli-rag-soc        to   rf-fnt-rag-soc         .
      *                      *-----------------------------------------*
      *                      * Ragione sociale in uppercase            *
      *                      *-----------------------------------------*
           move      w-cli-rag-soc        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   rf-fnt-rag-key         .
           move      w-cli-via-cli        to   rf-fnt-via-fnt         .
           move      w-cli-loc-cli        to   rf-fnt-loc-fnt         .
      *                      *-----------------------------------------*
      *                      * Codice nazione                          *
      *                      *-----------------------------------------*
           move      w-cli-cod-naz        to   rf-fnt-cod-naz         .
           if        rf-fnt-cod-naz       =    spaces
                     move  "IT "          to   rf-fnt-cod-naz         .
      *                      *-----------------------------------------*
      *                      * Coordinate geografiche                  *
      *                      *-----------------------------------------*
           move      w-cli-cod-cmn        to   rf-fnt-cod-cmn         .
           move      w-cli-cod-fzn        to   rf-fnt-cod-fzn         .
           move      w-cli-cod-lct        to   rf-fnt-cod-lct         .
      *                      *-----------------------------------------*
      *                      * Telefono, ecc.                          *
      *                      *-----------------------------------------*
           move      w-cli-num-tel        to   rf-fnt-num-tel         .
           move      w-cli-num-fax        to   rf-fnt-num-fax         .
           move      w-cli-num-tlx        to   rf-fnt-num-tlx         .
           move      w-cli-nom-int        to   rf-fnt-nom-int         .
      *                      *-----------------------------------------*
      *                      * Codice Iva                              *
      *                      *-----------------------------------------*
           move      zero                 to   rf-fnt-cod-iva         .
      *                      *-----------------------------------------*
      *                      * Partita Iva                             *
      *                      *-----------------------------------------*
           move      w-cli-prt-iva        to   rf-fnt-prt-iva         .
      *                      *-----------------------------------------*
      *                      * Codice fiscale                          *
      *                      *-----------------------------------------*
           move      w-cli-cod-fis        to   rf-fnt-cod-fis         .
      *                      *-----------------------------------------*
      *                      * In allegato                             *
      *                      *-----------------------------------------*
           move      "S"                  to   rf-fnt-snx-a13         .
      *                      *-----------------------------------------*
      *                      * Contropartita fornitori                 *
      *                      *-----------------------------------------*
           move      1406005              to   rf-fnt-cod-cge         .
      *                      *-----------------------------------------*
      *                      * Area libera                             *
      *                      *-----------------------------------------*
           move      spaces               to   rf-fnt-alx-exp         .
       cmp-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [fnt]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-fnt-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-fnt-000      thru cmp-rec-fnt-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       wrt-rec-fnt-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [dcf]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
       cmp-rec-dcf-100.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           move      w-cli-cod-cof        to   rf-dcf-cod-fnt         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-dpz-fnt         .
      *                  *---------------------------------------------*
      *                  * Data, utente, fase, di ultimo inserimento o *
      *                  * modifica                                    *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-dcf-ide-dat         .
           move      "master"             to   rf-cli-ide-ute         .
           move      s-fas                to   rf-dcf-ide-fas         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   rf-dcf-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale in uppercase                *
      *                  *---------------------------------------------*
           move      w-dcc-rag-dcc        to   w-upp-des              .
           perform   trf-des-upp-000      thru trf-des-upp-999        .
           move      w-upp-des            to   rf-dcf-rag-key         .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      w-dcc-via-dcc        to   rf-dcf-via-dcf         .
      *                  *---------------------------------------------*
      *                  * C.a.p. e citta'                             *
      *                  *---------------------------------------------*
           move      w-dcc-loc-dcc        to   rf-dcf-loc-dcf         .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      w-cli-cod-naz        to   rf-dcf-cod-naz         .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      w-dcc-cod-cmn        to   rf-dcf-cod-cmn         .
      *                  *---------------------------------------------*
      *                  * Codice frazione                             *
      *                  *---------------------------------------------*
           move      w-dcc-cod-fzn        to   rf-dcf-cod-fzn         .
      *                  *---------------------------------------------*
      *                  * Codice localita'                            *
      *                  *---------------------------------------------*
           move      w-dcc-cod-lct        to   rf-dcf-cod-lct         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale per inoltro                 *
      *                  *---------------------------------------------*
           move      w-dcc-rs1-doc        to   rf-dcf-rs1-doc         .
           move      w-dcc-rs2-doc        to   rf-dcf-rs2-doc         .
      *                  *---------------------------------------------*
      *                  * Numero di telefono                          *
      *                  *---------------------------------------------*
           move      w-dcc-num-tel        to   rf-dcf-num-tel         .
      *                  *---------------------------------------------*
      *                  * Numero di telefax                           *
      *                  *---------------------------------------------*
           move      w-dcc-num-fax        to   rf-dcf-num-fax         .
      *                  *---------------------------------------------*
      *                  * Numero di telex                             *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-num-tlx         .
      *                  *---------------------------------------------*
      *                  * Nome interlocutore                          *
      *                  *---------------------------------------------*
           move      w-dcc-nom-int        to   rf-dcf-nom-int         .
      *                  *---------------------------------------------*
      *                  * Inoltro documenti                           *
      *                  *                                             *
      *                  *  - '01' : Alla sede                         *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcf-inl-dcm         .
      *                  *---------------------------------------------*
      *                  * Inoltro pagamenti                           *
      *                  *                                             *
      *                  *  - '01' : Alla sede                         *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcf-inl-pgt         .
      *                  *---------------------------------------------*
      *                  * Nostro codice come cliente                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-noi-cli         .
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento Iva                    *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcf-ctp-acq         .
      *                  *---------------------------------------------*
      *                  * Periodicita' di fatturazione                *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Raggruppamento bolle in fattura             *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice valuta                               *
      *                  *---------------------------------------------*
           move      c-sgl                to   rf-dcf-cod-vlt         .
      *                  *---------------------------------------------*
      *                  * Codice lingua                               *
      *                  *---------------------------------------------*
           move      "I  "                to   rf-dcf-cod-lng         .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-cod-lst         .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in riga                    *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcf-cat-scr         .
      *                  *---------------------------------------------*
      *                  * % sconto in riga                            *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Categoria sconto in chiusura                *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * % sconto in chiusura                        *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Addebito spese e modalita'                  *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Voci descrittive in fattura                 *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice forma di pagamento : decodifica      *
      *                  *---------------------------------------------*
           if        w-dcc-des-fop        = "RIM.DIR.                 "
                     move  0000001        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 60 GG.D.F"
                     move  0000002        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 90 GG.F.M"
                     move  0000003        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30 GG. F.M.         "
                     move  0000004        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R. B. 30/60 GG. F.M.     "
                     move  0000005        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60 GG. F.M.         "
                     move  0000006        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "CONTRASSEGNO             "
                     move  0000007        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 30 GG    "
                     move  0000008        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "PAGAMENTO ANTICIPATO     "
                     move  0000009        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90 GG.F.M.    "
                     move  0000010        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "CONTRASSEGNO  SC.3%      "
                     move  0000011        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30 GG.DF            "
                     move  0000012        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIM.DIR. SCONTO 5%       "
                     move  0000013        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. A VISTA  SC. 3%     "
                     move  0000014        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30 GG. FM           "
                     move  0000004        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B.  60 GG.DF           "
                     move  0000016        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 60 GG.FM "
                     move  0000017        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B.120 GG.FM            "
                     move  0000018        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90 GG.FM            "
                     move  0000019        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA SC.20%   "
                     move  0000020        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA SC.10%   "
                     move  0000021        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 75 GG.FM            "
                     move  0000022        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60 GG. DF        "
                     move  0000025        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60 GG.FM         "
                     move  0000005        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90 GG.DF      "
                     move  0000027        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90 GG.FM      "
                     move  0000010        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90/120 GG.DF  "
                     move  0000029        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90/120 GG.FM  "
                     move  0000030        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 120 GG FM"
                     move  0000031        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RIMESSA DIRETTA 150 GG.FM"
                     move  0000032        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90 GG.DF         "
                     move  0000034        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90 GG.FM         "
                     move  0000035        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90/120 GG.DF     "
                     move  0000036        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90/120 GG.FM     "
                     move  0000037        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 120/150/180 GG.FM   "
                     move  0000038        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90/120 GG.DF        "
                     move  0000040        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90/120 GG.FM        "
                     move  0000041        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RB.30/60/90/120/150/180FM"
                     move  0000043        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 30/60/90/120/150 DF "
                     move  0000044        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90/120/150/180 FM"
                     move  0000045        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 150/180 GG FM       "
                     move  0000046        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 180/210 GG FM       "
                     move  0000047        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "RB.30/60/90/120/150/180DF"
                     move  0000048        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B.30/60/90/120/150 GGFM"
                     move  0000049        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90/120/150 GG.DF "
                     move  0000050        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 60/90/120/150 GG.FM "
                     move  0000051        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90/120/150 GG DF    "
                     move  0000054        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90/120/150 GG.FM    "
                     move  0000055        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 120/150 GG DF       "
                     move  0000059        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 120/150 GG FM       "
                     move  0000060        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 150 GG FM           "
                     move  0000063        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 180 GG DF           "
                     move  0000064        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 180 GG FM           "
                     move  0000065        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "BONIFICO BANCARIO        "
                     move  0000071        to   rf-dcf-cod-fop
           else if   w-dcc-des-fop        = "R.B. 90 GG.DF            "
                     move  0000072        to   rf-dcf-cod-fop
           else      move  zero           to   rf-dcf-cod-fop         .
       cmp-rec-dcf-430.
      *                  *---------------------------------------------*
      *                  * Tipo esclusione mesi                        *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giorno fisso di scadenza                    *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mesi di esclusione                          *
      *                  *                                             *
      *                  * N.B.: campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese incasso          *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-add-spi         .
      *                  *---------------------------------------------*
      *                  * Codice tipo addebito spese bollo            *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-add-spb         .
      *                  *---------------------------------------------*
      *                  * Codice A.B.I.                               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcf-cod-abi         .
      *                  *---------------------------------------------*
      *                  * Codice C.A.B.                               *
      *                  *---------------------------------------------*
           move      zero                 to   rf-dcf-cod-cab         .
      *                  *---------------------------------------------*
      *                  * Conto corrente di appoggio                  *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice nostra banca                         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * c/c postale fornitore                       *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo incasso preferenziale                  *
      *                  *                                             *
      *                  * N.B.: Campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice acquisitore                          *
      *                  *                                             *
      *                  * N.B.: Campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Categoria di provvigioni legata al fornito- *
      *                  * re                                          *
      *                  *                                             *
      *                  * N.B.: Campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * % provvigioni legate al fornitore           *
      *                  *                                             *
      *                  * N.B.: Campo attualmente non gestito         *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice zona                                 *
      *                  *---------------------------------------------*
           move      w-dcc-cod-zon        to   rf-dcf-cod-zon         .
      *                  *---------------------------------------------*
      *                  * Codice categoria                            *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice statistico                           *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Classe di budget                            *
      *                  *---------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di Ordini bloccati                     *
      *                  *---------------------------------------------*
           move      01                   to   rf-dcf-for-blo         .
       cmp-rec-dcf-600.
      *                  *---------------------------------------------*
      *                  * Area libera per espansioni future           *
      *                  *---------------------------------------------*
           move      spaces               to   rf-dcf-alx-exp         .
       cmp-rec-dcf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cmp-rec-dcf-999.
       cmp-rec-dcf-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [dcf]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-dcf-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-dcf-000      thru cmp-rec-dcf-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
       wrt-rec-dcf-999.
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
      *    * Routine per l'allineamento di una stringa a sinistra      *
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
       all-str-asx-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-asx-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa in outpiut allineata a si- *
      *              * nistra                                          *
      *              *-------------------------------------------------*
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (1 : w-all-str-i03)     .
       all-str-asx-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa a destra        *
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Valore della stringa da allinea- *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-all-str-alf = Valore della stringa allineata   *
      *    *                          a destra                         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-adx-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-adx-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa allineata a destra         *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-i02          .
           subtract  w-all-str-i03        from w-all-str-i02          .
           add       1                    to   w-all-str-i02          .
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (w-all-str-i02:
                                               w-all-str-i03)         .
       all-str-adx-999.
           exit.

      *    *===========================================================*
      *    * Routine per l'allineamento di una stringa al centro       *
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Valore della stringa da allinea- *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-all-str-alf = Valore della stringa allineata   *
      *    *                          al centro                        *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       all-str-cen-000.
      *              *-------------------------------------------------*
      *              * Se la stringa in input e' tutta a spaces : u-   *
      *              * scita senza alcuna azione                       *
      *              *-------------------------------------------------*
           if        w-all-str-alf        =    spaces
                     go to all-str-cen-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa dichiarata                              *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-alf    tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-alf    tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * stringa in input, troncandola eventualmente al- *
      *              * la massima lunghezza dichiarata                 *
      *              *-------------------------------------------------*
           move      240                  to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-lun
                     move  w-all-str-lun  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Spostamento della stringa utile in input in a-  *
      *              * rea di transito allineata a sinistra            *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-wst          .
           add       1                    to   w-all-str-i01          .
           move      w-all-str-alf
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-wst
                                              (1 : w-all-str-i03)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Preparazione stringa allineata al centro        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-i02          .
           subtract  w-all-str-i03        from w-all-str-i02          .
           divide    2                    into w-all-str-i02          .
           add       1                    to   w-all-str-i02          .
           move      w-all-str-wst
                    (1 : w-all-str-i03)   to   w-all-str-alf
                                              (w-all-str-i02:
                                               w-all-str-i03)         .
       all-str-cen-999.
           exit.

      *    *===========================================================*
      *    * Routine per il concatenamento di max 10 stringhe di max   *
      *    * 80 caratteri ciascuna senza lasciare alcuno spazio di     *
      *    * separazione tra una stringa e l'altra                     *
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
       all-str-cat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del numero di strin-  *
      *              * ghe da concatenare                              *
      *              *-------------------------------------------------*
           if        w-all-str-num        >    10
                     move  10             to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa in output                               *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Se zero stringhe da concatenare : uscita a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           if        w-all-str-num        =    zero
                     go to all-str-cat-999.
       all-str-cat-100.
      *              *-------------------------------------------------*
      *              * Se le stringhe in input sono tutte a spaces :   *
      *              * uscita a spaces                                 *
      *              *-------------------------------------------------*
       all-str-cat-110.
           move      zero                 to   w-all-str-inx          .
       all-str-cat-120.
           add       1                    to   w-all-str-inx          .
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-cat-999.
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-cat-120.
       all-str-cat-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per esame substringhe   *
      *              * da concatenare                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Inizializzazione puntatore su stringa in out-   *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero residuo di caratte- *
      *              * ri residui concatenabili                        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-max          .
       all-str-cat-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su substringa                 *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di stringhe da concatenare : *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-cat-999.
      *              *-------------------------------------------------*
      *              * Se substringa a spaces : la si ignora           *
      *              *-------------------------------------------------*
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-cat-300.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della substringa                *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * substringa, troncandola eventualmente alla mas- *
      *              * sima lunghezza residua                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-max
                     move  w-all-str-max  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Concatenazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-i01          .
           move      w-all-str-cat
                    (w-all-str-inx)
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-alf
                                              (w-all-str-pnt:
                                               w-all-str-i03)         .
      *              *-------------------------------------------------*
      *              * Incremento puntatore su stringa in output       *
      *              *-------------------------------------------------*
           add       w-all-str-i03        to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-all-str-pnt        >    w-all-str-lun
                     go to all-str-cat-999.
      *              *-------------------------------------------------*
      *              * Decremento numero residuo di caratteri disponi- *
      *              * bili                                            *
      *              *-------------------------------------------------*
           subtract  w-all-str-i03        from w-all-str-max          .
      *              *-------------------------------------------------*
      *              * Riciclo a substringa successiva                 *
      *              *-------------------------------------------------*
           go to     all-str-cat-300.
       all-str-cat-999.
           exit.

      *    *===========================================================*
      *    * Routine per il concatenamento di max 10 stringhe di max   *
      *    * 80 caratteri ciascuna lasciando uno spazio di separazio-  *
      *    * ne tra una stringa e l'altra                              *
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
       all-str-csb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione a spaces della stringa in out-  *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-all-str-alf          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione del numero di strin-  *
      *              * ghe da concatenare                              *
      *              *-------------------------------------------------*
           if        w-all-str-num        >    10
                     move  10             to   w-all-str-num          .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione della lunghezza della *
      *              * stringa in output                               *
      *              *-------------------------------------------------*
           if        w-all-str-lun        <    001
                     move  001            to   w-all-str-lun          .
           if        w-all-str-lun        >    240
                     move  240            to   w-all-str-lun          .
      *              *-------------------------------------------------*
      *              * Se zero stringhe da concatenare : uscita a spa- *
      *              * ces                                             *
      *              *-------------------------------------------------*
           if        w-all-str-num        =    zero
                     go to all-str-csb-999.
       all-str-csb-100.
      *              *-------------------------------------------------*
      *              * Se le stringhe in input sono tutte a spaces :   *
      *              * uscita a spaces                                 *
      *              *-------------------------------------------------*
       all-str-csb-110.
           move      zero                 to   w-all-str-inx          .
       all-str-csb-120.
           add       1                    to   w-all-str-inx          .
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-csb-999.
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-csb-120.
       all-str-csb-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice per esame substringhe   *
      *              * da concatenare                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Inizializzazione puntatore su stringa in out-   *
      *              * put                                             *
      *              *-------------------------------------------------*
           move      1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero residuo di caratte- *
      *              * ri residui concatenabili                        *
      *              *-------------------------------------------------*
           move      w-all-str-lun        to   w-all-str-max          .
       all-str-csb-300.
      *              *-------------------------------------------------*
      *              * Incremento indice su substringa                 *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre il numero di stringhe da concatenare : *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-all-str-inx        >    w-all-str-num
                     go to all-str-csb-999.
      *              *-------------------------------------------------*
      *              * Se substringa a spaces : la si ignora           *
      *              *-------------------------------------------------*
           if        w-all-str-cat
                    (w-all-str-inx)       =    spaces
                     go to all-str-csb-300.
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo al primo    *
      *              * carattere utile della substringa                *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i01          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i01
                                   for leading spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione dell'indice relativo all'ultimo  *
      *              * carattere utile della stringa in input          *
      *              *-------------------------------------------------*
           move      zero                 to   w-all-str-i02          .
           inspect   w-all-str-cat
                    (w-all-str-inx)   tallying w-all-str-i02
                                  for trailing spaces                 .
      *              *-------------------------------------------------*
      *              * Determinazione della lunghezza effettiva della  *
      *              * substringa, troncandola eventualmente alla mas- *
      *              * sima lunghezza residua                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-i03          .
           subtract  w-all-str-i01        from w-all-str-i03          .
           subtract  w-all-str-i02        from w-all-str-i03          .
           if        w-all-str-i03        >    w-all-str-max
                     move  w-all-str-max  to   w-all-str-i03          .
      *              *-------------------------------------------------*
      *              * Concatenazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-all-str-i01          .
           move      w-all-str-cat
                    (w-all-str-inx)
                    (w-all-str-i01:
                     w-all-str-i03)       to   w-all-str-alf
                                              (w-all-str-pnt:
                                               w-all-str-i03)         .
      *              *-------------------------------------------------*
      *              * Incremento puntatore su stringa in output       *
      *              *-------------------------------------------------*
           add       w-all-str-i03        to   w-all-str-pnt          .
           add       1                    to   w-all-str-pnt          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-all-str-pnt        >    w-all-str-lun
                     go to all-str-csb-999.
      *              *-------------------------------------------------*
      *              * Decremento numero residuo di caratteri disponi- *
      *              * bili                                            *
      *              *-------------------------------------------------*
           subtract  w-all-str-i03        from w-all-str-max          .
           subtract  1                    from w-all-str-max          .
      *              *-------------------------------------------------*
      *              * Riciclo a substringa successiva                 *
      *              *-------------------------------------------------*
           go to     all-str-csb-300.
       all-str-csb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione comune corrispondente in base a Cap e      *
      *    * descrizione                                               *
      *    *-----------------------------------------------------------*
       det-cap-cfl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-cap-cfl-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi letti        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cap-cfl-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-cap-cfl-cmn      .
           move      zero                 to   w-det-cap-cfl-fzn      .
           move      zero                 to   w-det-cap-cfl-lct      .
           move      spaces               to   w-det-cap-cfl-ele      .
       det-cap-cfl-100.
      *              *-------------------------------------------------*
      *              * Start su file [gxc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DESORD    "         to   f-key                  .
           move      w-det-cap-cfl-cfl    to   rf-gxc-des-ord         .
           move      zero                 to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cap-cfl-900.
       det-cap-cfl-200.
      *              *-------------------------------------------------*
      *              * Next su [gxc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cap-cfl-900.
       det-cap-cfl-300.
      *              *-------------------------------------------------*
      *              * Max su [gxc], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-gxc-des-ord       not  = w-det-cap-cfl-cfl
                     go to det-cap-cfl-900.
       det-cap-cfl-400.
      *              *-------------------------------------------------*
      *              * Selezione su Cap                                *
      *              *-------------------------------------------------*
           if        rf-gxc-cap-avp       not  = w-det-cap-cfl-cap
                     go to det-cap-cfl-200.
      *              *-------------------------------------------------*
      *              * Selezione su Provincia                          *
      *              *-------------------------------------------------*
           if        rf-gxc-cod-prv       not  = w-det-cap-cfl-prv
                     go to det-cap-cfl-200.
       det-cap-cfl-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore elementi letti             *
      *              *-------------------------------------------------*
           add       1                    to   w-det-cap-cfl-ctr      .
       det-cap-cfl-620.
      *              *-------------------------------------------------*
      *              * Bufferizzazione dell'elemento letto             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione codici comune, frazione e    *
      *                  * localita'                                   *
      *                  *---------------------------------------------*
           move      rf-gxc-cod-cmn       to   w-det-cap-cfl-cmn      .
           move      rf-gxc-cod-fzn       to   w-det-cap-cfl-fzn      .
           move      rf-gxc-cod-lct       to   w-det-cap-cfl-lct      .
           move      rf-gxc-cod-prv       to   w-det-cap-cfl-prv      .
           if        rf-gxc-cod-cmn       not  = zero
                     move  "C"            to   w-det-cap-cfl-ele      .
           if        rf-gxc-cod-fzn       not  = zero
                     move  "F"            to   w-det-cap-cfl-ele      .
           if        rf-gxc-cod-lct       not  = zero
                     move  "L"            to   w-det-cap-cfl-ele      .
       det-cap-cfl-800.
      *              *-------------------------------------------------*
      *              * Riciclo a record [gxc] successivo               *
      *              *-------------------------------------------------*
           go to     det-cap-cfl-200.
       det-cap-cfl-900.
      *              *-------------------------------------------------*
      *              * Test in uscita                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su contatore elementi letti            *
      *                  *---------------------------------------------*
           if        w-det-cap-cfl-ctr    =    zero
                     move  "#"            to   w-det-cap-cfl-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-cap-cfl-999.
       det-cap-cfl-999.
           exit.

