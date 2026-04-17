       Identification Division.
       Program-Id.                                 pimptmi2           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:    imp                 *
      *                                   Fase:    imptmi              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/01/05    *
      *                       Ultima revisione:    NdK del 26/11/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Test chiavi 'dbsoft' per Legislazione       *
      *                                                                *
      *                    TEMI                                        *
      *                                                                *
      *                    Prevede un solo processo (release)          *
      *                                                                *
      *                    Scansione /abd/asc/output.txt generato da   *
      *                    chiave.exe                                  *
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
                     "imp"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "imptmi"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pimptmi0"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "     Test chiavi SESTANTE 'dbsoft'      "       .

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
      *        *-------------------------------------------------------*
      *        * Linea 001                                             *
      *        *-------------------------------------------------------*
           05  w-lds-001 redefines
               w-lds-000.
      *            *---------------------------------------------------*
      *            * Sigla prodotto                                    *
      *            *---------------------------------------------------*
               10  w-lds-001-sgl-pro      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-lds-001-cod-cli      pic  9(05)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Release                                           *
      *            *---------------------------------------------------*
               10  w-lds-001-rel-001      pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Password                                          *
      *            *---------------------------------------------------*
               10  w-lds-001-pwd-cli      pic  x(23)                  .

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
      *    * Work per subroutine password TEMI                         *
      *    *-----------------------------------------------------------*
           copy      "tmi/gsc/prg/cpy/dpwdtmi0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di decodifica numero decimale in     *
      *    * esadecimale                                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdecesa0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di decodifica numero decimale in     *
      *    * rappresentazione binaria                                  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdecbin0.cpw"                   .

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
           move      "/abd/asc/output.txt"
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
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           go to     xcv-opn-fls-999.
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
           if        w-lds-000 (001:01)   not  = "L"
                     go to xcv-trt-pds-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione tabella processi standard   *
      *                  *---------------------------------------------*
           perform   dec-pwx-new-nor-000  thru dec-pwx-new-nor-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento cliente                         *
      *                  *---------------------------------------------*
           move      w-lds-001-sgl-pro    to   w-pwx-tip-scd          .
           move      w-lds-001-cod-cli    to   w-pwx-cod-cli          .
           move      w-lds-001-rel-001    to   w-pwx-ele-prc (01)     .
           perform   dec-pwx-new-000      thru dec-pwx-new-999        .
      *                  *---------------------------------------------*
      *                  * Test su password assegnata                  *
      *                  *---------------------------------------------*
           if        w-pwx-key-pwx        =    w-lds-001-pwd-cli
                     go to xcv-trt-pds-586.
      *                  *---------------------------------------------*
      *                  * Preparazione messaggio per il rullino       *
      *                  *---------------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Codice cliente con errore : "
                                delimited by size
                     w-lds-001-cod-cli
                                delimited by spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio per il rullino          *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Preparazione messaggio per il rullino       *
      *                  *---------------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Chiave TANGRAM : "
                                delimited by size
                     w-pwx-key-pwx
                                delimited by spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio per il rullino          *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Preparazione messaggio per il rullino       *
      *                  *---------------------------------------------*
           move      spaces               to   m-msg                  .
           string    "Chiave DBSOFT  : "
                                delimited by size
                     w-lds-001-pwd-cli
                                delimited by spaces
                                          into m-msg                  .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio per il rullino          *
      *                  *---------------------------------------------*
           move      "WR"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     xcv-trt-pds-900.
       xcv-trt-pds-820.

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
      *    * Decodifica password Temi                                  *
      *    *-----------------------------------------------------------*
           copy      "tmi/gsc/prg/cpy/dpwdtmi0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Routine di decodifica da numerico a esadecimale           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdecesa0.cps"                   .

      *    *===========================================================*
      *    * Routine di decodifica da decimale a binario               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wdecbin0.cps"                   .


