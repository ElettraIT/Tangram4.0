       Identification Division.
       Program-Id.                                 mfatbir0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/92    *
      *                       Ultima revisione:    NdK del 14/10/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la lettura dei documenti, e per  *
      *                    la determinazione della voce 'Fatturato'    *
      *                    da trattare per le statistiche di vendita   *
      *                    sul fatturato su righe documento.           *
      *                                                                *
      *                    ELETTRA                                     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "OP"                 *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "CL"                 *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GS"  Inizializzazione lettura sequenziale [bit] e [bir]       *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "GS"                 *
      *                                                                *
      *                       w-mod-fat-bir-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-fat-bir-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-fat-bir-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-fat-bir-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-fat-bir-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GT"  Lettura sequenziale archivio [bit] e [bir]               *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "GT"                 *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-bit            : Record [bit] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-bir            : Record [bir] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-fat-bir-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-fat-bir-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-bir-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-fat-bir-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-bir-pon : Valore della voce    *
      *                                           con incidenza Po-    *
      *                                           sitiva o Negativa    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CS"  Inizializzazione lettura sequenziale [bit] e [bir] per   *
      *       un solo codice prodotto                                  *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "CS"                 *
      *                                                                *
      *                       w-mod-fat-bir-pro : Codice prodotto      *
      *                                                                *
      *                       w-mod-fat-bir-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-fat-bir-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-fat-bir-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-fat-bir-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-fat-bir-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-fat-bir-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CG"  Lettura sequenziale archivio [bit] e [bir] per un solo   *
      *       codice prodotto                                          *
      *                                                                *
      *              Input  : w-mod-fat-bir-ope : "CG"                 *
      *                                                                *
      *                       rf-bit            : Non significativo    *
      *                                                                *
      *                       rf-bir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-bit            : Record [bit] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-bir            : Record [bir] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-fat-bir-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-fat-bir-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-bir-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-bir-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-fat-bir-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-bir-pon : Valore della voce    *
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

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

       Special-Names.       Decimal-Point is comma .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [rel]                                        *
      *    *-----------------------------------------------------------*
           select  optional  rel   assign  to disk     f-rel-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is          f-rel-krn
                             file status   is          f-rel-sts      .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [rel]                                    *
      *    *-----------------------------------------------------------*
       fd  rel  label record standard.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  rel-rec.
      *        *-------------------------------------------------------*
      *        * Numero protocollo                                     *
      *        *-------------------------------------------------------*
           05  rel-num-prt                pic  9(11)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Numero progressivo                                    *
      *        *-------------------------------------------------------*
           05  rel-num-prg                pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Si/No record riferito al 1. periodo                   *
      *        *-------------------------------------------------------*
           05  rel-snx-x1p                pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record riferito al 2. periodo                   *
      *        *-------------------------------------------------------*
           05  rel-snx-x2p                pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record riferito al 3. periodo                   *
      *        *-------------------------------------------------------*
           05  rel-snx-x3p                pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [rel]                *
      *    *-----------------------------------------------------------*
       01  f-rel.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-rel-nam                  pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-rel-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-rel-sts                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Record number per input-output                        *
      *        *-------------------------------------------------------*
           05  f-rel-krn                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Max record number scritto                             *
      *        *-------------------------------------------------------*
           05  f-rel-kms                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Record number per scrittura                           *
      *        *-------------------------------------------------------*
           05  f-rel-put                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Record number per lettura                             *
      *        *-------------------------------------------------------*
           05  f-rel-get                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di open eseguita                                 *
      *        *  - Spaces : No                                        *
      *        *  - #      : Si'                                       *
      *        *-------------------------------------------------------*
           05  f-rel-opn                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Cache memory                                          *
      *        *-------------------------------------------------------*
           05  f-rel-chm.
      *            *---------------------------------------------------*
      *            * Numero di records possibili in cache memory       *
      *            *---------------------------------------------------*
               10  f-rel-chm-max          pic  9(05) value 4096       .
      *            *---------------------------------------------------*
      *            * Cache memory buffer                               *
      *            *---------------------------------------------------*
               05  f-rel-chm-buf.
                   10  f-rel-chm-rec occurs 4096.
                       15  f-rel-chm-prt  pic  9(09)       comp-3     .
                       15  f-rel-chm-prg  pic  9(05)       comp-3     .
                       15  f-rel-chm-x1p  pic  x(01)                  .
                       15  f-rel-chm-x2p  pic  x(01)                  .
                       15  f-rel-chm-x3p  pic  x(01)                  .

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
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Area gestionale [mag]                                 *
      *        *-------------------------------------------------------*
           05  w-prs-arg-mag.
      *            *---------------------------------------------------*
      *            * Si/No espressione dei costi unitari in unita' di  *
      *            * misura diverse                                    *
      *            *                                                   *
      *            * - N : No                                          *
      *            * - K : Costo riferito a 1000 unita'                *
      *            * - C : Costo riferito a 100  unita'                *
      *            * - D : Costo riferito a 10   unita'                *
      *            *---------------------------------------------------*
               10  w-prs-arg-mag-ecu      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no gestione % di maggiorazione prezzo di acquisto  *
      *        *-------------------------------------------------------*
           05  w-prs-snx-mpa              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf/fatfir[tip-fun]'      *
      *    *-----------------------------------------------------------*
       01  w-prs-fat-fir.
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Fatture                              *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-fat          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Note di addebito                     *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-ndb          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Note di accredito                    *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-ncr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Fatture di acconto                   *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-acc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Merce                  *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rtm          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Servizio               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Imballo                *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rti          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 4               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rt4          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 5               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rt5          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 6               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rt6          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 7               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rt7          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 8               *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rt8          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Extra Attivita'        *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-rtx          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Filler riempitivo                                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No detrazione Sconto in chiusura                   *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-scc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No detrazione Sconto pagamento                     *
      *        *-------------------------------------------------------*
           05  w-prs-fat-fir-scp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-fat-bir.
      *        *-------------------------------------------------------*
      *        * Flag di apertura modulo per determinazione valore di  *
      *        * magazzino eseguita                                    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-fvm          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di inizializzazione lettura sequenziale [bit]    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-fgs          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di 'Tipo prossima lettura da eseguire'           *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-nxt          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records scritti in file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-csc          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records riletti da file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-crl          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice prodotto                                       *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-pro          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-npr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo calcolo voce 'Costo del Fatturato'               *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-ccf          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p1i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p1f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p2i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p2f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p3i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-p3f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 1. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-x1p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 2. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-x2p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 3. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-x3p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Quantita' fatturata'  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-qta          pic s9(13)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Fatturato'            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-val          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Costo del Fatturato'  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-cos          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Incidenza in positivo *
      *        * o in negativo'                                        *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-pon          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto in chiusura in valuta base             *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-scc          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto pagamento in valuta base               *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-scp          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto totale in valuta base                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-sct          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto riga in valuta base                    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-scr          pic s9(18)                  .
      *        *-------------------------------------------------------*
      *        * Importo totale in valuta base righe da includere nel- *
      *        * la statistica                                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-itr          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Totali di suddivisione per le righe, in valuta        *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-tri occurs 09
                                          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita da selezione su record [bir]           *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-sel          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Percentuale di maggiorazione                          *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-mpa          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Maggiorazione                                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-bir-pma          pic s9(09)v9(05)            .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione valore unitario  *
      *    * di magazzino                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dvunmag0.dtl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'mfatbir0' *
      *    *-----------------------------------------------------------*
           copy      "ele/svf/prg/cpy/mfatbir0.mdl"                   .

      *    *===========================================================*
      *    * Record file [bit]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .

      *    *===========================================================*
      *    * Record file [bir]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .

      ******************************************************************
       Procedure Division                using w-mod-fat-bir
                                               rf-bit
                                               rf-bir                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-fat-bir-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-fat-bir-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-fat-bir-ope    =    "GS"
                     perform   gts-000    thru gts-999
           else if   w-mod-fat-bir-ope    =    "GT"
                     perform   get-000    thru get-999
           else if   w-mod-fat-bir-ope    =    "CS"
                     perform   cds-000    thru cds-999
           else if   w-mod-fat-bir-ope    =    "CG"
                     perform   cdg-000    thru cdg-999                .
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
      *              * Inizializzazione flag di apertura modulo per    *
      *              * determinazione valore di magazzino a : mai e-   *
      *              * seguita                                         *
      *              *-------------------------------------------------*
           move      "0"                  to   w-aux-fat-bir-fvm      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * non ancora eseguita Start                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-fat-bir-fgs      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di 'Tipo prossima lettura *
      *              * da eseguire' a : 'Indeterminato'                *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-bir-nxt      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione sul tipo fun-   *
      *              * zionamento statistiche sul fatturato su righe   *
      *              * documento                                       *
      *              *-------------------------------------------------*
           perform   prs-fat-bir-000      thru prs-fat-bir-999        .
      *              *-------------------------------------------------*
      *              * Si/No espressione dei costi unitari in unita'   *
      *              * di misura diverse                               *
      *              *-------------------------------------------------*
           perform   prs-snx-ecu-000      thru prs-snx-ecu-999        .
      *              *-------------------------------------------------*
      *              * Gestione % maggiorazione prezzo di acquisto     *
      *              *-------------------------------------------------*
           perform   prs-snx-mpa-000      thru prs-snx-mpa-999        .
      *              *-------------------------------------------------*
      *              * Open file relative di appoggio [rel]            *
      *              *-------------------------------------------------*
           perform   fil-rel-opn-000      thru fil-rel-opn-999        .
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
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [aaf] e [aaq]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-mpa        =    "N"
                     go to opn-999.
      *                  *---------------------------------------------*
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Close file relative di appoggio [rel]           *
      *              *-------------------------------------------------*
           perform   fil-rel-cls-000      thru fil-rel-cls-999        .
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
      *              * [age]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
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
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * Close modulo per la determinazione del valore   *
      *              * unitario di magazzino                           *
      *              *-------------------------------------------------*
           perform   cls-vun-mag-000      thru cls-vun-mag-999        .
      *              *-------------------------------------------------*
      *              * [aaf] e [aaq]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-mpa        =    "N"
                     go to cls-999.
      *                  *---------------------------------------------*
      *                  * [aaf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * [aaq]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Open modulo per la determinazione del valore unitario di  *
      *    * magazzino                                                 *
      *    *-----------------------------------------------------------*
       opn-vun-mag-000.
      *              *-------------------------------------------------*
      *              * Se apertura modulo in essere : nessuna azione e *
      *              * uscita immediata                                *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-fvm    =    "2"
                     go to opn-vun-mag-999.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione valore di magaz-  *
      *              * zino                                            *
      *              *-------------------------------------------------*
           perform   det-vun-mag-opn-000  thru det-vun-mag-opn-999    .
      *              *-------------------------------------------------*
      *              * Flag di apertura modulo a : in essere           *
      *              *-------------------------------------------------*
           move      "2"                  to   w-aux-fat-bir-fvm      .
       opn-vun-mag-999.
           exit.

      *    *===========================================================*
      *    * Close modulo per la determinazione del valore unitario di *
      *    * magazzino                                                 *
      *    *-----------------------------------------------------------*
       cls-vun-mag-000.
      *              *-------------------------------------------------*
      *              * Se apertura modulo non in essere : nessuna a-   *
      *              * zione e a test su cancellazione                 *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-fvm    not  = "2"
                     go to cls-vun-mag-100.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione valore di magaz- *
      *              * zino                                            *
      *              *-------------------------------------------------*
           move      "CL"                 to   d-vun-mag-tip-ope      .
           perform   det-vun-mag-cll-000  thru det-vun-mag-cll-999    .
      *              *-------------------------------------------------*
      *              * Flag di apertura modulo a : chiuso              *
      *              *-------------------------------------------------*
           move      "1"                  to   w-aux-fat-bir-fvm      .
       cls-vun-mag-100.
      *              *-------------------------------------------------*
      *              * Se modulo non chiuso : nessuna azione ed uscita *
      *              * immediata                                       *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-fvm    not  = "1"
                     go to cls-vun-mag-999.
      *              *-------------------------------------------------*
      *              * Test se cancellabile                            *
      *              *-------------------------------------------------*
           move      "C?"                 to   d-vun-mag-tip-ope      .
           perform   det-vun-mag-cll-000  thru det-vun-mag-cll-999    .
           if        d-vun-mag-exi-sts    not  = spaces
                     go to cls-vun-mag-999.
      *              *-------------------------------------------------*
      *              * Flag di apertura modulo a : mai aperto          *
      *              *-------------------------------------------------*
           move      "0"                  to   w-aux-fat-bir-fvm      .
       cls-vun-mag-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione su statistiche sul fatturato    *
      *    * per tipo funzionamento statistiche su righe documento     *
      *    *-----------------------------------------------------------*
       prs-fat-bir-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/svf/fatfir[tip-fun]"
                                delimited by   size
                     w-mod-fat-bir-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-fat-bir-150.
       prs-fat-bir-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-fat-bir-500.
       prs-fat-bir-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-fat-fir          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-fat-bir-700.
       prs-fat-bir-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf/fatfir[tip-fun]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-fat-bir-650.
       prs-fat-bir-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-fat-fir          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-fat-bir-700.
       prs-fat-bir-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-fat-fir          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-fat-bir-700.
       prs-fat-bir-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Fatture                'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-fat    not  = "N"
                     move  "S"            to   w-prs-fat-fir-fat      .
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Note di addebito       'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-ndb    not  = "N"
                     move  "S"            to   w-prs-fat-fir-ndb      .
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Note di accredito      'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-ncr    not  = "N"
                     move  "S"            to   w-prs-fat-fir-ncr      .
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Fatture di acconto     'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-acc    not  = "N"
                     move  "S"            to   w-prs-fat-fir-acc      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Merce                       'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rtm    not  = "N"
                     move  "S"            to   w-prs-fat-fir-rtm      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Servizi                     'S' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rts    not  = "N"
                     move  "S"            to   w-prs-fat-fir-rts      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Imballi                     'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rti    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rti      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 4               'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rt4    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rt4      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 5               'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rt5    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rt5      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 6               'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rt6    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rt6      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 7               'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rt7    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rt7      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 8               'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rt8    not  = "S" and
                     w-prs-fat-fir-rt8    not  = "C"
                     move  "N"            to   w-prs-fat-fir-rt8      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Extra Attivita'             'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-rtx    not  = "S"
                     move  "N"            to   w-prs-fat-fir-rtx      .
      *                  *---------------------------------------------*
      *                  * Si/No detrazione Sconto in chiusura     'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-scc    not  = "S"
                     move  "N"            to   w-prs-fat-fir-scc      .
      *                  *---------------------------------------------*
      *                  * Si/No detrazione Sconto pagamento       'N' *
      *                  *---------------------------------------------*
           if        w-prs-fat-fir-scp    not  = "S"
                     move  "N"            to   w-prs-fat-fir-scp      .
       prs-fat-bir-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-fat-bir-999.
       prs-fat-bir-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No espressione dei costi   *
      *    * unitari in unita' di misura diverse                       *
      *    *-----------------------------------------------------------*
       prs-snx-ecu-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/mag/cos[snx-ecu]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-arg-mag-ecu
           else      move  spaces         to   w-prs-arg-mag-ecu      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N" or
                     w-prs-arg-mag-ecu    =    "K" or
                     w-prs-arg-mag-ecu    =    "C" or
                     w-prs-arg-mag-ecu    =    "D"
                     go to prs-snx-ecu-999.
           move      "N"                  to   w-prs-arg-mag-ecu      .
       prs-snx-ecu-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa alla gestione    *
      *    * della % di maggiorazione prezzo di acquisto               *
      *    *-----------------------------------------------------------*
       prs-snx-mpa-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcf/aaf[snx-mpa]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-mpa
           else      move  spaces         to   w-prs-snx-mpa          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-mpa        not  = "S" and
                     w-prs-snx-mpa        not  = "M"
                     move  "N"            to   w-prs-snx-mpa          .
       prs-snx-mpa-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [bit]       *
      *    *-----------------------------------------------------------*
       gts-000.
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
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * Start eseguita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-aux-fat-bir-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-fat-bir-nxt      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-npr    to   w-aux-fat-bir-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-ccf    to   w-aux-fat-bir-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p1i    to   w-aux-fat-bir-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p1f    to   w-aux-fat-bir-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p2i    to   w-aux-fat-bir-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p2f    to   w-aux-fat-bir-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p3i    to   w-aux-fat-bir-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p3f    to   w-aux-fat-bir-p3f      .
       gts-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale                                       *
      *    *-----------------------------------------------------------*
       get-000.
      *              *-------------------------------------------------*
      *              * Ciclo per [bit] su bolle diverse                *
      *              *-------------------------------------------------*
           perform   get-bit-000          thru get-bit-999            .
      *              *-------------------------------------------------*
      *              * Ciclo per [bft] su bolle di reso                *
      *              *                                                 *
      *              * ATTUALMENTE INIBITO                             *
      *              *-------------------------------------------------*
______*    perform   get-bft-000          thru get-bft-999            .
       get-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     get-999.
       get-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale                                       *
      *    *                                                           *
      *    * Subroutine archivio [bit]                                 *
      *    *-----------------------------------------------------------*
       get-bit-000.
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
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se flag di inizializzazione lettura sequenziale *
      *              * archivio [bit] ancora in Off : uscita come per  *
      *              * fine file                                       *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-fgs    =    spaces
                     go to get-bit-900.
      *              *-------------------------------------------------*
      *              * Se flag di ''Tipo prossima lettura da eseguire' *
      *              * al valore 'Indeterminato' : uscita come per fi- *
      *              * ne file                                         *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-nxt    =    zero
                     go to get-bit-900.
       get-bit-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-nxt    =    1
                     go to get-bit-200
           else if   w-aux-fat-bir-nxt    =    2
                     go to get-bit-225
           else if   w-aux-fat-bir-nxt    =    3
                     go to get-bit-250
           else if   w-aux-fat-bir-nxt    =    4
                     go to get-bit-300
           else if   w-aux-fat-bir-nxt    =    5
                     go to get-bit-600
           else if   w-aux-fat-bir-nxt    =    6
                     go to get-bit-700
           else      go to get-bit-900.
       get-bit-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bit] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p1i    to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-cod-dpz         .
           move      zero                 to   rf-bit-num-doc         .
           move      spaces               to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bit-100.
       get-bit-225.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 2. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bit] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p2i    to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-cod-dpz         .
           move      zero                 to   rf-bit-num-doc         .
           move      spaces               to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bit-100.
       get-bit-250.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 3. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bit] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p3i    to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-cod-dpz         .
           move      zero                 to   rf-bit-num-doc         .
           move      spaces               to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bit-100.
       get-bit-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [bit]                          *
      *              *-------------------------------------------------*
       get-bit-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [bit]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-900.
       get-bit-330.
      *                  *---------------------------------------------*
      *                  * Selezione su codice dipendenza in base allo *
      *                  * status determinato                          *
      *                  *---------------------------------------------*
           move      "TS"                 to   w-dpz-tip-ope          .
           move      rf-bit-cod-dpz       to   w-dpz-cod-dpz          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione status     *
      *                      *-----------------------------------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to get-bit-300.
       get-bit-340.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-bit letta    *
      *                  *---------------------------------------------*
           perform   man-pre-bit-000      thru man-pre-bit-999        .
       get-bit-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [bit], a seconda del numero di  *
      *                  * periodi di riferimento, e se non superato : *
      *                  * uscita per fine file. Inoltre test di ap-   *
      *                  * partenenza ad uno o piu' periodi di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
       get-bit-355.
      *                      *-----------------------------------------*
      *                      * Appartenenza ai periodi di riferimento: *
      *                      * tutti a 'No'                            *
      *                      *-----------------------------------------*
           move      "N"                  to   w-aux-fat-bir-x1p      .
           move      "N"                  to   w-aux-fat-bir-x2p      .
           move      "N"                  to   w-aux-fat-bir-x3p      .
       get-bit-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-fat-bir-npr    =    01
                     go to get-bit-365
           else if   w-aux-fat-bir-npr    =    02
                     go to get-bit-370
           else if   w-aux-fat-bir-npr    =    03
                     go to get-bit-375.
       get-bit-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       >    w-aux-fat-bir-p1f
                     go to get-bit-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S"
                     go to get-bit-400.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bit-900.
       get-bit-370.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 02   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       >    w-aux-fat-bir-p2f
                     go to get-bit-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p2i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S"
                     go to get-bit-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bit-900.
       get-bit-375.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 03   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       >    w-aux-fat-bir-p3f
                     go to get-bit-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p2i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-aux-fat-bir-p3i and
                     rf-bit-dat-doc       not  > w-aux-fat-bir-p3f
                     move  "S"            to   w-aux-fat-bir-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S" or
                     w-aux-fat-bir-x3p    =    "S"
                     go to get-bit-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       <    w-aux-fat-bir-p3i
                     move  3              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bit-900.
       get-bit-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : flag di 'Tipo prossima let-  *
      *                  * tura da eseguire' a 'Read next su [bit] e   *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
           if        rf-bit-cod-tmb       not  = "BIL  "  and
                     rf-bit-cod-tmb       not  = "BILFS"  and
                     rf-bit-cod-tmb       not  = "BNF  "  and
                     rf-bit-cod-tmb       not  = "BNFFO"  and
                     rf-bit-cod-tmb       not  = "BNFFS"  and
                     rf-bit-cod-tmb       not  = "BRVE "  and
                     rf-bit-cod-tmb       not  = "BSO  "  and
                     rf-bit-cod-tmb       not  = "BSOFS"  and
                     rf-bit-cod-tmb       not  = "BV   "  and
                     rf-bit-cod-tmb       not  = "BVFO "  and
                     rf-bit-cod-tmb       not  = "BVFS "
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
       get-bit-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-bit-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-bit-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-bit-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-bit-cdc-vpf       to   w-cvs-vlt-cdc          .
       get-bit-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totali di suddivisione                  *
      *                      *-----------------------------------------*
           move      rf-bit-tot-rig (1)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (1)  .
      *
           move      rf-bit-tot-rig (2)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (2)  .
      *
           move      rf-bit-tot-rig (3)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (3)  .
      *
           move      rf-bit-tot-rig (4)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (4)  .
      *
           move      rf-bit-tot-rig (5)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (5)  .
      *
           move      rf-bit-tot-rig (6)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (6)  .
      *
           move      rf-bit-tot-rig (7)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (7)  .
      *
           move      rf-bit-tot-rig (8)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (8)  .
      *
           move      rf-bit-tot-rig (9)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (9)  .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-bit-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-bit-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scp      .
       get-bit-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scc    =    "S"
                     add   w-aux-fat-bir-scc
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scp    =    "S"
                     add   w-aux-fat-bir-scp
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Eventuale abbattimento totale libero 8  *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-rt8    =    "C"
                     subtract w-aux-fat-bir-tri (8)
                                          from w-aux-fat-bir-sct      .
       get-bit-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si pone il flag di 'Tipo prossima lettura   *
      *                  * da eseguire' a 'Start su [bir]' e si rici-  *
      *                  * cla                                         *
      *                  *---------------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     move  5              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
       get-bit-525.
      *                  *---------------------------------------------*
      *                  * Determinazione importo totale, in valuta    *
      *                  * base, delle righe documento che dovranno    *
      *                  * successivamente essere incluse nella sta-   *
      *                  * tistica                                     *
      *                  *---------------------------------------------*
       get-bit-530.
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare importo totale  *
      *                      * righe                                   *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [bir]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-575.
       get-bit-535.
      *                      *-----------------------------------------*
      *                      * Read next su [bir]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bit-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-bir     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-bir-000      thru man-pre-bir-999        .
       get-bit-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to get-bit-575.
       get-bit-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [bir], se non superata : a *
      *                      * riciclo su read next su [bir]           *
      *                      *-----------------------------------------*
           perform   sel-rec-bir-000      thru sel-rec-bir-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     go to get-bit-535.
       get-bit-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-fat-bir-val    to   w-aux-fat-bir-itr      .
       get-bit-555.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     get-bit-535.
       get-bit-575.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a 'Start su [bir]'                          *
      *                  *---------------------------------------------*
           move      5                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bit-100.
       get-bit-600.
      *              *=================================================*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start su [bir]                              *
      *              *-------------------------------------------------*
       get-bit-625.
      *                  *---------------------------------------------*
      *                  * Start su [bir]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : flag di 'Tipo prossima    *
      *                  * lettura da eseguire' a 'Read next su [bit]' *
      *                  * e riciclo                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : flag di 'Tipo prossima lettura *
      *                  * da eseguire' a 'Read next su [bir]' e rici- *
      *                  * clo                                         *
      *                  *---------------------------------------------*
           move      6                    to   w-aux-fat-bir-nxt      .
           go to     get-bit-100.
       get-bit-700.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [bir]                          *
      *              *-------------------------------------------------*
       get-bit-725.
      *                  *---------------------------------------------*
      *                  * Read next su [bir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se At end : flag di 'Tipo prossima lettura  *
      *                  * da eseguire' a 'Read next su [bit]' e ri-   *
      *                  * ciclo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-bir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-bir-000      thru man-pre-bir-999        .
       get-bit-750.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato : flag di     *
      *                      * 'Tipo prossima lettura da eseguire' a   *
      *                      * 'Read next su [bit]' e riciclo          *
      *                      *-----------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
       get-bit-775.
      *                      *-----------------------------------------*
      *                      * Selezione su [bir], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [bir]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-bir-000      thru sel-rec-bir-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     move  6              to   w-aux-fat-bir-nxt
                     go to get-bit-100.
       get-bit-800.
      *                      *-----------------------------------------*
      *                      * Correzione eventuale della voce 'Fattu- *
      *                      * rato'                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se importo sconto totale in valuta  *
      *                          * base pari a zero : nessuna corre-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     go to get-bit-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-val    =    zero
                     go to get-bit-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-itr    =    zero
                     go to get-bit-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-fat-bir-sct    to   w-aux-fat-bir-scr      .
           multiply  w-aux-fat-bir-val    by   w-aux-fat-bir-scr      .
           divide    w-aux-fat-bir-itr    into w-aux-fat-bir-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-fat-bir-scr    from w-aux-fat-bir-val      .
       get-bit-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-fat-bir-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x1p    to   w-mod-fat-bir-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x2p    to   w-mod-fat-bir-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x3p    to   w-mod-fat-bir-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-qta    to   w-mod-fat-bir-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-val    to   w-mod-fat-bir-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-fat-bir-000  thru det-cos-fat-bir-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-pon    to   w-mod-fat-bir-pon      .
       get-bit-850.
      *                      *-----------------------------------------*
      *                      * Flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [bir]'           *
      *                      *-----------------------------------------*
           move      6                    to   w-aux-fat-bir-nxt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-bit-999.
       get-bit-900.
      *              *-------------------------------------------------*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mod-fat-bir-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-fat-bir-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-bit-999.
       get-bit-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale                                       *
      *    *                                                           *
      *    * Subroutine archivio [bft]                                 *
      *    *-----------------------------------------------------------*
       get-bft-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di 'Tipo prossima lettura *
      *              * da eseguire' a : 'Indeterminato'                *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-fat-bir-nxt      .
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se flag di inizializzazione lettura sequenziale *
      *              * archivio [bft] ancora in Off : uscita come per  *
      *              * fine file                                       *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-fgs    =    spaces
                     go to get-bft-900.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * al valore 'Indeterminato' : uscita come per fi- *
      *              * ne file                                         *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-nxt    =    zero
                     go to get-bft-900.
       get-bft-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-nxt    =    1
                     go to get-bft-200
           else if   w-aux-fat-bir-nxt    =    2
                     go to get-bft-225
           else if   w-aux-fat-bir-nxt    =    3
                     go to get-bft-250
           else if   w-aux-fat-bir-nxt    =    4
                     go to get-bft-300
           else if   w-aux-fat-bir-nxt    =    5
                     go to get-bft-600
           else if   w-aux-fat-bir-nxt    =    6
                     go to get-bft-700
           else      go to get-bft-900.
       get-bft-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bft] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p1i    to   rf-bft-dat-reg         .
           move      zero                 to   rf-bft-cod-dpz         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bft-100.
       get-bft-225.
      *              *=================================================*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 2. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bft] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p2i    to   rf-bft-dat-reg         .
           move      zero                 to   rf-bft-cod-dpz         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bft-100.
       get-bft-250.
      *              *=================================================*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 3. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bft] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-bir-p3i    to   rf-bft-dat-reg         .
           move      zero                 to   rf-bft-cod-dpz         .
           move      spaces               to   rf-bft-cod-tmb         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bft-100.
       get-bft-300.
      *              *=================================================*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [bft]                          *
      *              *-------------------------------------------------*
       get-bft-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [bft]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-900.
       get-bft-330.
      *                  *---------------------------------------------*
      *                  * Selezione su codice dipendenza in base allo *
      *                  * status determinato                          *
      *                  *---------------------------------------------*
       get-bft-340.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-bft letta    *
      *                  *---------------------------------------------*
       get-bft-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [bft], a seconda del numero di  *
      *                  * periodi di riferimento, e se non superato : *
      *                  * uscita per fine file. Inoltre test di ap-   *
      *                  * partenenza ad uno o piu' periodi di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
       get-bft-355.
      *                      *-----------------------------------------*
      *                      * Appartenenza ai periodi di riferimento: *
      *                      * tutti a 'No'                            *
      *                      *-----------------------------------------*
           move      "N"                  to   w-aux-fat-bir-x1p      .
           move      "N"                  to   w-aux-fat-bir-x2p      .
           move      "N"                  to   w-aux-fat-bir-x3p      .
       get-bft-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-fat-bir-npr    =    01
                     go to get-bft-365
           else if   w-aux-fat-bir-npr    =    02
                     go to get-bft-370
           else if   w-aux-fat-bir-npr    =    03
                     go to get-bft-375.
       get-bft-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       >    w-aux-fat-bir-p1f
                     go to get-bft-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p1i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S"
                     go to get-bft-400.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bft-900.
       get-bft-370.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 02   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       >    w-aux-fat-bir-p2f
                     go to get-bft-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p1i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p2i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S"
                     go to get-bft-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bft-900.
       get-bft-375.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 03   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       >    w-aux-fat-bir-p3f
                     go to get-bft-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p1i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p2i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       not  < w-aux-fat-bir-p3i and
                     rf-bft-dat-reg       not  > w-aux-fat-bir-p3f
                     move  "S"            to   w-aux-fat-bir-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S" or
                     w-aux-fat-bir-x3p    =    "S"
                     go to get-bft-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bft-dat-reg       <    w-aux-fat-bir-p3i
                     move  3              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-bft-900.
       get-bft-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : flag di 'Tipo prossima let-  *
      *                  * tura da eseguire' a 'Read next su [bft] e   *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
           if        rf-bft-cod-tmb       not  = "BRVE "
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
       get-bft-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-bft-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-bft-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-bft-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-bft-cdc-vpf       to   w-cvs-vlt-cdc          .
       get-bft-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totali di suddivisione                  *
      *                      *-----------------------------------------*
           move      rf-bft-tot-rig (1)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (1)  .
      *
           move      rf-bft-tot-rig (2)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (2)  .
      *
           move      rf-bft-tot-rig (3)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (3)  .
      *
           move      rf-bft-tot-rig (4)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (4)  .
      *
           move      rf-bft-tot-rig (5)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (5)  .
      *
           move      rf-bft-tot-rig (6)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (6)  .
      *
           move      rf-bft-tot-rig (7)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (7)  .
      *
           move      rf-bft-tot-rig (8)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (8)  .
      *
           move      rf-bft-tot-rig (9)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (9)  .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-bft-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-bft-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scp      .
       get-bft-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scc    =    "S"
                     add   w-aux-fat-bir-scc
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scp    =    "S"
                     add   w-aux-fat-bir-scp
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Eventuale abbattimento totale libero 8  *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-rt8    =    "C"
                     subtract w-aux-fat-bir-tri (8)
                                          from w-aux-fat-bir-sct      .
       get-bft-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si pone il flag di 'Tipo prossima lettura   *
      *                  * da eseguire' a 'Start su [bfr]' e si rici-  *
      *                  * cla                                         *
      *                  *---------------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     move  5              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
       get-bft-525.
      *                  *---------------------------------------------*
      *                  * Determinazione importo totale, in valuta    *
      *                  * base, delle righe documento che dovranno    *
      *                  * successivamente essere incluse nella sta-   *
      *                  * tistica                                     *
      *                  *---------------------------------------------*
       get-bft-530.
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare importo totale  *
      *                      * righe                                   *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [bfr]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-575.
       get-bft-535.
      *                      *-----------------------------------------*
      *                      * Read next su [bfr]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-bft-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-bfr     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
______*    perform   man-pre-bir-000      thru man-pre-bir-999        .
       get-bft-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-bfr-num-prt       not  = rf-bft-num-prt
                     go to get-bft-575.
       get-bft-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [bfr], se non superata : a *
      *                      * riciclo su read next su [bfr]           *
      *                      *-----------------------------------------*
           perform   sel-rec-bfr-000      thru sel-rec-bfr-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     go to get-bft-535.
       get-bft-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-fat-bir-val    to   w-aux-fat-bir-itr      .
       get-bft-555.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     get-bft-535.
       get-bft-575.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a 'Start su [bfr]'                          *
      *                  *---------------------------------------------*
           move      5                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-bft-100.
       get-bft-600.
      *              *=================================================*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start su [bfr]                              *
      *              *-------------------------------------------------*
       get-bft-625.
      *                  *---------------------------------------------*
      *                  * Start su [bfr]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : flag di 'Tipo prossima    *
      *                  * lettura da eseguire' a 'Read next su [bft]' *
      *                  * e riciclo                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : flag di 'Tipo prossima lettura *
      *                  * da eseguire' a 'Read next su [bfr]' e rici- *
      *                  * clo                                         *
      *                  *---------------------------------------------*
           move      6                    to   w-aux-fat-bir-nxt      .
           go to     get-bft-100.
       get-bft-700.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [bfr]                          *
      *              *-------------------------------------------------*
       get-bft-725.
      *                  *---------------------------------------------*
      *                  * Read next su [bfr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se At end : flag di 'Tipo prossima lettura  *
      *                  * da eseguire' a 'Read next su [bft]' e ri-   *
      *                  * ciclo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-bfr letta       *
      *                  *---------------------------------------------*
______*    perform   man-pre-bir-000      thru man-pre-bir-999        .
       get-bft-750.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato : flag di     *
      *                      * 'Tipo prossima lettura da eseguire' a   *
      *                      * 'Read next su [bft]' e riciclo          *
      *                      *-----------------------------------------*
           if        rf-bfr-num-prt       not  = rf-bft-num-prt
                     move  4              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
       get-bft-775.
      *                      *-----------------------------------------*
      *                      * Selezione su [bfr], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [bfr]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-bfr-000      thru sel-rec-bfr-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     move  6              to   w-aux-fat-bir-nxt
                     go to get-bft-100.
       get-bft-800.
      *                      *-----------------------------------------*
      *                      * Correzione eventuale della voce 'Fattu- *
      *                      * rato'                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se importo sconto totale in valuta  *
      *                          * base pari a zero : nessuna corre-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     go to get-bft-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-val    =    zero
                     go to get-bft-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-itr    =    zero
                     go to get-bft-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-fat-bir-sct    to   w-aux-fat-bir-scr      .
           multiply  w-aux-fat-bir-val    by   w-aux-fat-bir-scr      .
           divide    w-aux-fat-bir-itr    into w-aux-fat-bir-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-fat-bir-scr    from w-aux-fat-bir-val      .
       get-bft-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-fat-bir-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x1p    to   w-mod-fat-bir-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x2p    to   w-mod-fat-bir-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x3p    to   w-mod-fat-bir-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-qta    to   w-mod-fat-bir-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-val    to   w-mod-fat-bir-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-fat-bir-000  thru det-cos-fat-bir-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-pon    to   w-mod-fat-bir-pon      .
       get-bft-850.
      *                      *-----------------------------------------*
      *                      * Flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [bfr]'           *
      *                      *-----------------------------------------*
           move      6                    to   w-aux-fat-bir-nxt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-bft-999.
       get-bft-900.
      *              *=================================================*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mod-fat-bir-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-fat-bir-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-bft-999.
       get-bft-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [bir] per   *
      *    * un solo codice prodotto                                   *
      *    *-----------------------------------------------------------*
       cds-000.
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
      *              * Start eseguita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-aux-fat-bir-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-fat-bir-nxt      .
      *              *-------------------------------------------------*
      *              * Inizializzazione nuovo ciclo di scrittura/let-  *
      *              * tura file relative di appoggio                  *
      *              *-------------------------------------------------*
           perform   fil-rel-new-000      thru fil-rel-new-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori per scrittura e ri-  *
      *              * lettura file relative                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-bir-csc      .
           move      zero                 to   w-aux-fat-bir-crl      .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice prodotto                  *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-pro    to   w-aux-fat-bir-pro      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-npr    to   w-aux-fat-bir-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-ccf    to   w-aux-fat-bir-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p1i    to   w-aux-fat-bir-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p1f    to   w-aux-fat-bir-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p2i    to   w-aux-fat-bir-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p2f    to   w-aux-fat-bir-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p3i    to   w-aux-fat-bir-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-bir-p3f    to   w-aux-fat-bir-p3f      .
       cds-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-nxt    =    1
                     go to cds-200
           else if   w-aux-fat-bir-nxt    =    2
                     go to cds-225
           else if   w-aux-fat-bir-nxt    =    3
                     go to cds-250
           else if   w-aux-fat-bir-nxt    =    4
                     go to cds-300
           else      go to cds-900.
       cds-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bir] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-bir-cod-dpz         .
           move      01                   to   rf-bir-tip-mag         .
           move      w-aux-fat-bir-pro    to   rf-bir-num-pro         .
           move      w-aux-fat-bir-p1i    to   rf-bir-dat-doc         .
           move      zero                 to   rf-bir-num-doc         .
           move      zero                 to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cds-100.
       cds-225.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 2. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bir] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-bir-cod-dpz         .
           move      01                   to   rf-bir-tip-mag         .
           move      w-aux-fat-bir-pro    to   rf-bir-num-pro         .
           move      w-aux-fat-bir-p2i    to   rf-bir-dat-doc         .
           move      zero                 to   rf-bir-num-doc         .
           move      zero                 to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cds-100.
       cds-250.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 3. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [bir] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-bir-cod-dpz         .
           move      01                   to   rf-bir-tip-mag         .
           move      w-aux-fat-bir-pro    to   rf-bir-num-pro         .
           move      w-aux-fat-bir-p3i    to   rf-bir-dat-doc         .
           move      zero                 to   rf-bir-num-doc         .
           move      zero                 to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [bir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cds-100.
       cds-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [bir]                          *
      *              *-------------------------------------------------*
       cds-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [bir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-bir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-bir-000      thru man-pre-bir-999        .
       cds-330.
      *                  *---------------------------------------------*
      *                  * Test Max su [bir] per il tipo e il codice   *
      *                  * prodotto : se non superato, uscita per      *
      *                  * fine file                                   *
      *                  *---------------------------------------------*
           if        rf-bir-tip-mag       not  = 01                or
                     rf-bir-num-pro       not  = w-aux-fat-bir-pro
                     go to cds-900.
       cds-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [bir], a seconda del numero di  *
      *                  * periodi di riferimento, e se non superato : *
      *                  * uscita per fine file. Inoltre test di ap-   *
      *                  * partenenza ad uno o piu' periodi di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
       cds-355.
      *                      *-----------------------------------------*
      *                      * Appartenenza ai periodi di riferimento: *
      *                      * tutti a 'No'                            *
      *                      *-----------------------------------------*
           move      "N"                  to   w-aux-fat-bir-x1p      .
           move      "N"                  to   w-aux-fat-bir-x2p      .
           move      "N"                  to   w-aux-fat-bir-x3p      .
       cds-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-fat-bir-npr    =    01
                     go to cds-365
           else if   w-aux-fat-bir-npr    =    02
                     go to cds-370
           else if   w-aux-fat-bir-npr    =    03
                     go to cds-375.
       cds-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       >    w-aux-fat-bir-p1f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     cds-900.
       cds-370.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 02   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       >    w-aux-fat-bir-p2f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p2i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     cds-900.
       cds-375.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 03   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       >    w-aux-fat-bir-p3f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p1i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p1f
                     move  "S"            to   w-aux-fat-bir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p2i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p2f
                     move  "S"            to   w-aux-fat-bir-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       not  < w-aux-fat-bir-p3i and
                     rf-bir-dat-doc       not  > w-aux-fat-bir-p3f
                     move  "S"            to   w-aux-fat-bir-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-x1p    =    "S" or
                     w-aux-fat-bir-x2p    =    "S" or
                     w-aux-fat-bir-x3p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       <    w-aux-fat-bir-p2i
                     move  2              to   w-aux-fat-bir-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-bir-dat-doc       <    w-aux-fat-bir-p3i
                     move  3              to   w-aux-fat-bir-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     cds-900.
       cds-400.
      *                      *-----------------------------------------*
      *                      * Selezione su [bir], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [bir]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-bir-000      thru sel-rec-bir-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     move  4              to   w-aux-fat-bir-nxt
                     go to cds-100.
       cds-500.
      *              *-------------------------------------------------*
      *              * Scrittura file relative di appoggio [rel]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records scritti in fi- *
      *                  * le di appoggio [rel]                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-fat-bir-csc      .
           move      w-aux-fat-bir-csc    to   f-rel-put              .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area record [rel]           *
      *                  *---------------------------------------------*
           move      spaces               to   rel-rec                .
      *                  *---------------------------------------------*
      *                  * Preparazione area record [rel]              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero protocollo fattura               *
      *                      *-----------------------------------------*
           move      rf-bir-num-prt       to   rel-num-prt            .
      *                      *-----------------------------------------*
      *                      * Numero progressivo protocollo fattura   *
      *                      *-----------------------------------------*
           move      rf-bir-num-prg       to   rel-num-prg            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x1p    to   rel-snx-x1p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x2p    to   rel-snx-x2p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-x3p    to   rel-snx-x3p            .
      *                  *---------------------------------------------*
      *                  * Esecuzione funzione Put su file relative    *
      *                  * di appoggio [rel]                           *
      *                  *---------------------------------------------*
           perform   fil-rel-put-000      thru fil-rel-put-999        .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a   *
      *              * 'Read next su [bir]'                            *
      *              *-------------------------------------------------*
           move      4                    to   w-aux-fat-bir-nxt      .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     cds-100.
       cds-900.
      *              *-------------------------------------------------*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cds-999.
       cds-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale file relative per un solo codice pro- *
      *    * dotto                                                     *
      *    *-----------------------------------------------------------*
       cdg-000.
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
       cdg-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file di appoggio relative   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records file relative  *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-fat-bir-crl      .
      *                  *---------------------------------------------*
      *                  * Test se oltre il numero records scritti     *
      *                  *---------------------------------------------*
           if        w-aux-fat-bir-crl    >    w-aux-fat-bir-csc
                     go to cdg-900.
      *                  *---------------------------------------------*
      *                  * Preparazione numero record per rilettura    *
      *                  *---------------------------------------------*
           move      w-aux-fat-bir-crl    to   f-rel-get              .
      *                  *---------------------------------------------*
      *                  * Esecuzione funzione Get da file relative    *
      *                  * di appoggio [rel]                           *
      *                  *---------------------------------------------*
           perform   fil-rel-get-000      thru fil-rel-get-999        .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : riciclo           *
      *                  *---------------------------------------------*
           if        f-rel-get            =    zero
                     go to cdg-000.
       cdg-200.
      *              *-------------------------------------------------*
      *              * Trattamento testata fattura                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [bit]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-bit letta    *
      *                  *---------------------------------------------*
           perform   man-pre-bit-000      thru man-pre-bit-999        .
       cdg-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : riciclo su lettura file re-  *
      *                  * lative                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * ___                                     *
      *                      *-----------------------------------------*
       cdg-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-bit-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-bit-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-bit-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-bit-cdc-vpf       to   w-cvs-vlt-cdc          .
       cdg-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totali di suddivisione                  *
      *                      *-----------------------------------------*
           move      rf-bit-tot-rig (1)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (1)  .
      *
           move      rf-bit-tot-rig (2)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (2)  .
      *
           move      rf-bit-tot-rig (3)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (3)  .
      *
           move      rf-bit-tot-rig (4)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (4)  .
      *
           move      rf-bit-tot-rig (5)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (5)  .
      *
           move      rf-bit-tot-rig (6)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (6)  .
      *
           move      rf-bit-tot-rig (7)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (7)  .
      *
           move      rf-bit-tot-rig (8)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (8)  .
      *
           move      rf-bit-tot-rig (9)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-tri (9)  .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-bit-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-bit-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-scp      .
       cdg-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scc    =    "S"
                     add   w-aux-fat-bir-scc
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scp    =    "S"
                     add   w-aux-fat-bir-scp
                                          to   w-aux-fat-bir-sct      .
      *                      *-----------------------------------------*
      *                      * Totale di suddivisione righe 8          *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-rt8    =    "C"
                     subtract w-aux-fat-bir-tri (8)
                                          from w-aux-fat-bir-sct      .
       cdg-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si va a trattamento riga fattura            *
      *                  *---------------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     go to cdg-600.
       cdg-525.
      *                  *---------------------------------------------*
      *                  * Determinazione importo totale, in valuta    *
      *                  * base, delle righe documento che dovranno    *
      *                  * successivamente essere incluse nella sta-   *
      *                  * tistica                                     *
      *                  *---------------------------------------------*
       cdg-530.
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare importo totale  *
      *                      * righe                                   *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-bir-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [bir]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
       cdg-535.
      *                      *-----------------------------------------*
      *                      * Read next su [bir]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-bir     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-bir-000      thru man-pre-bir-999        .
       cdg-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to cdg-575.
       cdg-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [bir], se non superata : a *
      *                      * riciclo su read next su [bir]           *
      *                      *-----------------------------------------*
           perform   sel-rec-bir-000      thru sel-rec-bir-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     go to cdg-535.
       cdg-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-fat-bir-val    to   w-aux-fat-bir-itr      .
       cdg-555.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     cdg-535.
       cdg-575.
      *                  *---------------------------------------------*
      *                  * A trattamento riga fattura                  *
      *                  *---------------------------------------------*
           go to     cdg-600.
       cdg-600.
      *              *-------------------------------------------------*
      *              * Trattamento riga fattura                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [bir]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-bir-num-prt         .
           move      rel-num-prg          to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-bir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-bir-000      thru man-pre-bir-999        .
       cdg-700.
      *                      *-----------------------------------------*
      *                      * Selezione su [bir], se non superata :   *
      *                      * riciclo su lettura file relative        *
      *                      *-----------------------------------------*
           perform   sel-rec-bir-000      thru sel-rec-bir-999        .
           if        w-aux-fat-bir-sel    not  = spaces
                     go to cdg-000.
       cdg-800.
      *                      *-----------------------------------------*
      *                      * Correzione eventuale della voce 'Fattu- *
      *                      * rato'                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se importo sconto totale in valuta  *
      *                          * base pari a zero : nessuna corre-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-sct    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-val    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-fat-bir-itr    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-fat-bir-sct    to   w-aux-fat-bir-scr      .
           multiply  w-aux-fat-bir-val    by   w-aux-fat-bir-scr      .
           divide    w-aux-fat-bir-itr    into w-aux-fat-bir-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-fat-bir-scr    from w-aux-fat-bir-val      .
       cdg-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-fat-bir-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x1p          to   w-mod-fat-bir-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x2p          to   w-mod-fat-bir-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x3p          to   w-mod-fat-bir-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-qta    to   w-mod-fat-bir-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-val    to   w-mod-fat-bir-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-fat-bir-000  thru det-cos-fat-bir-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-fat-bir-pon    to   w-mod-fat-bir-pon      .
       cdg-850.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     cdg-999.
       cdg-900.
      *              *-------------------------------------------------*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mod-fat-bir-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-bir-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-bir-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-fat-bir-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cdg-999.
       cdg-999.
           exit.

      *    *===========================================================*
      *    * Selezione e determinazioni su record [bir]                *
      *    *-----------------------------------------------------------*
       sel-rec-bir-000.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari generali                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga                      *
      *                  *---------------------------------------------*
           if        rf-bir-tip-rig       not  = "P    "
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-025.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo codice di magazzino       *
      *                  *---------------------------------------------*
           if        rf-bir-tip-mag       not  = 01
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-030.
      *                  *---------------------------------------------*
      *                  * Selezione su Codice prodotto numerico       *
      *                  *---------------------------------------------*
           if        rf-bir-num-pro       =    zero
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-035.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-bir-tip-pro       <    01 or
                     rf-bir-tip-pro       >    09
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-040.
      *                  *---------------------------------------------*
      *                  * Selezione su Quantita' e Valore             *
      *                  *---------------------------------------------*
           if        rf-bir-qta-ven       =    zero and
                     rf-bir-imp-rig       =    zero
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-045.
      *                  *---------------------------------------------*
      *                  * Selezione su righe di omaggio               *
      *                  *---------------------------------------------*
           move      rf-bir-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    9
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-100.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari su personalizzazioni      *
      *              *-------------------------------------------------*
       sel-rec-bir-105.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-bir-tip-pro       =    01
                     if    w-prs-fat-fir-rtm
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    02
                     if    w-prs-fat-fir-rts
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    03
                     if    w-prs-fat-fir-rti
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    04
                     if    w-prs-fat-fir-rt4
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    05
                     if    w-prs-fat-fir-rt5
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    06
                     if    w-prs-fat-fir-rt6
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    07
                     if    w-prs-fat-fir-rt7
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    08
                     if    w-prs-fat-fir-rt8
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
           if        rf-bir-tip-pro       =    09
                     if    w-prs-fat-fir-rtx
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bir-999.
       sel-rec-bir-200.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Quantita' fatturata'         *
      *              *-------------------------------------------------*
           move      rf-bir-qta-ven       to   w-aux-fat-bir-qta      .
       sel-rec-bir-300.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Fatturato', con conversione  *
      *              * in valuta base                                  *
      *              *-------------------------------------------------*
           move      rf-bir-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-val      .
       sel-rec-bir-400.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Incidenza in positivo o in   *
      *              * negativo'                                       *
      *              *-------------------------------------------------*
           if        rf-bir-cod-tmb       =    "BRVE "
                     move  "N"            to   w-aux-fat-bir-pon
           else      move  "P"            to   w-aux-fat-bir-pon      .
       sel-rec-bir-900.
      *              *-------------------------------------------------*
      *              * Flag di selezione superata                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-fat-bir-sel      .
       sel-rec-bir-999.
           exit.

      *    *===========================================================*
      *    * Selezione e determinazioni su record [bfr]                *
      *    *-----------------------------------------------------------*
       sel-rec-bfr-000.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari generali                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su codice tipo documento          *
      *                  *                                             *
      *                  * ELETTRA                                     *
      *                  *---------------------------------------------*
           if        rf-bfr-cod-tmb       not  = "BRVE "
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-020.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga                      *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-rig       not  = "P    "
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-025.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo codice di magazzino       *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-mag       not  = 01
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-030.
      *                  *---------------------------------------------*
      *                  * Selezione su Codice prodotto numerico       *
      *                  *---------------------------------------------*
           if        rf-bfr-num-mag       =    zero
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-035.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-pro       <    01 or
                     rf-bfr-tip-pro       >    09
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-040.
      *                  *---------------------------------------------*
      *                  * Selezione su Quantita' e Valore             *
      *                  *---------------------------------------------*
           if        rf-bfr-qta-acq       =    zero and
                     rf-bfr-imp-rig       =    zero
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-045.
      *                  *---------------------------------------------*
      *                  * Selezione su righe di omaggio               *
      *                  *---------------------------------------------*
           move      rf-bfr-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    9
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-100.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari su personalizzazioni      *
      *              *-------------------------------------------------*
       sel-rec-bfr-105.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-pro       =    01
                     if    w-prs-fat-fir-rtm
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    02
                     if    w-prs-fat-fir-rts
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    03
                     if    w-prs-fat-fir-rti
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    04
                     if    w-prs-fat-fir-rt4
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    05
                     if    w-prs-fat-fir-rt5
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    06
                     if    w-prs-fat-fir-rt6
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    07
                     if    w-prs-fat-fir-rt7
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    08
                     if    w-prs-fat-fir-rt8
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
           if        rf-bfr-tip-pro       =    09
                     if    w-prs-fat-fir-rtx
                                          not  = "S"
                     move  "#"            to   w-aux-fat-bir-sel
                     go to sel-rec-bfr-999.
       sel-rec-bfr-200.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Quantita' fatturata'         *
      *              *-------------------------------------------------*
           move      rf-bfr-qta-acq       to   w-aux-fat-bir-qta      .
       sel-rec-bfr-300.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Fatturato', con conversione  *
      *              * in valuta base                                  *
      *              *-------------------------------------------------*
           move      rf-bfr-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-bir-val      .
       sel-rec-bfr-400.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Incidenza in positivo o in   *
      *              * negativo'                                       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-aux-fat-bir-pon      .
       sel-rec-bfr-900.
      *              *-------------------------------------------------*
      *              * Flag di selezione superata                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-fat-bir-sel      .
       sel-rec-bfr-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla testa- *
      *    * ta documento rf-bit letta                                 *
      *    *-----------------------------------------------------------*
       man-pre-bit-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice del cliente e *
      *              * della sua dipendenza in funzione dello status   *
      *              * espresso in anagrafica commerciale              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record principale dell'anagrafica   *
      *                  * commerciale del cliente                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : nessuna manipola- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to man-pre-bit-400.
       man-pre-bit-200.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del cliente non    *
      *                  * indica che e' stato sostituito da una altro *
      *                  * cliente : nessuna manipolazione             *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tus       not  = 21 and
                     rf-dcc-sta-tus       not  = 52 and
                     rf-dcc-sta-tus       not  = 62 and
                     rf-dcc-sta-tus       not  = 72
                     go to man-pre-bit-400.
      *                  *---------------------------------------------*
      *                  * Se il codice cliente di riferimento e' a    *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tuc       =    zero
                     go to man-pre-bit-400.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo cliente : nessuna mani- *
      *                  * polazione                                   *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tux       not  = 02 and
                     rf-dcc-sta-tux       not  = 03
                     go to man-pre-bit-400.
       man-pre-bit-300.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * del cliente, ed eventualmente anche la di-  *
      *                  * pendenza                                    *
      *                  *---------------------------------------------*
           move      rf-dcc-sta-tuc       to   rf-bit-cod-arc         .
           if        rf-dcc-sta-tux       =    02
                     move  spaces         to   rf-bit-dpz-arc         .
       man-pre-bit-400.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice dell'agente   *
      *              * in funzione dello status espresso in anagrafica *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [age]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [age]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      rf-bit-cod-age       to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : nessuna manipola- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to man-pre-bit-600.
       man-pre-bit-420.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale dell'agente non    *
      *                  * indica che e' stato sostituito da una altro *
      *                  * agente : nessuna manipolazione              *
      *                  *---------------------------------------------*
           if        rf-age-sta-tus       not  = 21 and
                     rf-age-sta-tus       not  = 52 and
                     rf-age-sta-tus       not  = 62 and
                     rf-age-sta-tus       not  = 72
                     go to man-pre-bit-600.
      *                  *---------------------------------------------*
      *                  * Se il codice agente di riferimento e' a     *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-age-sta-tuc       =    zero
                     go to man-pre-bit-600.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo agente : nessuna mani-  *
      *                  * polazione                                   *
      *                  *---------------------------------------------*
           if        rf-age-sta-tux       not  = 02
                     go to man-pre-bit-600.
       man-pre-bit-430.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * dell'agente                                 *
      *                  *---------------------------------------------*
           move      rf-age-sta-tuc       to   rf-bit-cod-age         .
       man-pre-bit-600.
       man-pre-bit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     man-pre-bit-999.
       man-pre-bit-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla riga   *
      *    * documento rf-bir letta                                    *
      *    *-----------------------------------------------------------*
       man-pre-bir-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice del prodotto  *
      *              * in funzione dello status espresso in anagrafica *
      *              * commerciale                                     *
      *              *-------------------------------------------------*
       man-pre-bir-100.
      *                  *---------------------------------------------*
      *                  * Se il tipo riga non indica un prodotto :    *
      *                  * nessuna manipolazione                       *
      *                  *---------------------------------------------*
           if        rf-bir-tip-rig       not  = "P    "
                     go to man-pre-bir-900.
      *                  *---------------------------------------------*
      *                  * Se il tipo codice di magazzino non indica   *
      *                  * un prodotto : nessuna manipolazione         *
      *                  *---------------------------------------------*
           if        rf-bir-tip-mag       not  = 01
                     go to man-pre-bir-900.
       man-pre-bir-200.
      *                  *---------------------------------------------*
      *                  * Lettura record dell'anagrafica commerciale  *
      *                  * del prodotto                                *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-bir-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : nessuna manipola- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to man-pre-bir-900.
       man-pre-bir-300.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del prodotto non   *
      *                  * indica che e' stato sostituito da un' altro *
      *                  * prodotto : nessuna manipolazione            *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tus       not  = 21 and
                     rf-dcp-sta-tus       not  = 52 and
                     rf-dcp-sta-tus       not  = 72
                     go to man-pre-bir-900.
      *                  *---------------------------------------------*
      *                  * Se il codice prodotto di riferimento e' a   *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tuc       =    zero
                     go to man-pre-bir-900.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo prodotto : nessuna ma-  *
      *                  * nipolazione                                 *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tux       not  = 02
                     go to man-pre-bir-900.
       man-pre-bir-400.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * numerico del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-sta-tuc       to   rf-bir-num-pro         .
       man-pre-bir-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     man-pre-bir-999.
       man-pre-bir-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, in w-mod-fat-bir-cos, del- *
      *    * la voce 'Costo del Fatturato' corrispondente al record    *
      *    * in 'rf-bir'                                               *
      *    *                                                           *
      *    * Input  : rf-bir            = Record 'rf-bir' in esame'    *
      *    *                                                           *
      *    *          w-aux-fat-bir-ccf = Tipo di calcolo per la voce  *
      *    *                                                           *
      *    *          w-mod-fat-bir-qta = Valore della voce 'Quanti-   *
      *    *                              ta' Fatturata' gia' deter-   *
      *    *                              minata                       *
      *    *                                                           *
      *    * Output : w-mod-fat-bir-cos = Valore della voce 'Costo del *
      *    *                              Fatturato'                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-cos-fat-bir-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preventivo del valore da determina- *
      *              * re                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-fat-bir-cos      .
       det-cos-fat-bir-100.
      *              *-------------------------------------------------*
      *              * Se tipo calcolo non riconosciuto : uscita imme- *
      *              * diata senza nessuna ulteriore azione            *
      *              *-------------------------------------------------*
           if        w-aux-fat-bir-ccf    not  = "P" and
                     w-aux-fat-bir-ccf    not  = "U"
                     go to det-cos-fat-bir-999.
      *              *-------------------------------------------------*
      *              * Se la quantita' fatturata di riferimento e' a   *
      *              * zero : uscita immediata senza nessuna ulteriore *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-mod-fat-bir-qta    =    zero
                     go to det-cos-fat-bir-999.
      *              *-------------------------------------------------*
      *              * Se tipo codice di magazzino non ammesso per la  *
      *              * determinazione del costo unitario : uscita sen- *
      *              * za nessuna ulteriore azione                     *
      *              *-------------------------------------------------*
           if        rf-bir-tip-mag       not  = 01 and
                     rf-bir-tip-mag       not  = 02 and
                     rf-bir-tip-mag       not  = 03 and
                     rf-bir-tip-mag       not  = 04
                     go to det-cos-fat-bir-999.
      *              *-------------------------------------------------*
      *              * Se codice numerico di magazzino a zero : uscita *
      *              * senza nessuna ulteriore azione                  *
      *              *-------------------------------------------------*
           if        rf-bir-num-pro       =    zero
                     go to det-cos-fat-bir-999.
       det-cos-fat-bir-200.
      *              *-------------------------------------------------*
      *              * Apertura preliminare, se necessario, del modulo *
      *              * per la determinazione del valore unitario di    *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           perform   opn-vun-mag-000      thru opn-vun-mag-999        .
       det-cos-fat-bir-300.
      *              *-------------------------------------------------*
      *              * Determinazione del valore unitario di magazzino *
      *              *-------------------------------------------------*
       det-cos-fat-bir-310.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione                         *
      *                      *-----------------------------------------*
           move      "VM"                 to   d-vun-mag-tip-ope      .
      *                      *-----------------------------------------*
      *                      * Tipo di valore unitario                 *
      *                      *-----------------------------------------*
           if        w-aux-fat-bir-ccf    =    "P"
                     move  0001           to   d-vun-mag-tip-val
           else if   w-aux-fat-bir-ccf    =    "U"
                     move  0002           to   d-vun-mag-tip-val
           else      move  0001           to   d-vun-mag-tip-val      .
      *                      *-----------------------------------------*
      *                      * Data di riferimento per il valore uni-  *
      *                      * tario: pari alla data documento se non  *
      *                      * e' stata passata una data valorizzazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        w-mod-fat-bir-p2i    =    zero
                     move  rf-bir-dat-doc to   d-vun-mag-dat-val
           else      move  w-mod-fat-bir-p2i
                                          to   d-vun-mag-dat-val      .
      *                      *-----------------------------------------*
      *                      * Tipo codice di magazzino                *
      *                      *-----------------------------------------*
           move      rf-bir-tip-mag       to   d-vun-mag-tip-mag      .
      *                      *-----------------------------------------*
      *                      * Codice numerico di magazzino            *
      *                      *-----------------------------------------*
           move      rf-bir-num-pro       to   d-vun-mag-num-mag      .
      *                      *-----------------------------------------*
      *                      * Giacenza di proprieta': non influente   *
      *                      *-----------------------------------------*
           move      zero                 to   d-vun-mag-gia-prp      .
       det-cos-fat-bir-320.
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           perform   det-vun-mag-cll-000  thru det-vun-mag-cll-999    .
       det-cos-fat-bir-330.
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione nel modulo : uscita *
      *                  * immediata lasciando il valore della voce    *
      *                  * 'Costo del Fatturato' a zero                *
      *                  *---------------------------------------------*
           if        d-vun-mag-exi-sts    not  = spaces
                     go to det-cos-fat-bir-999.
      *                  *---------------------------------------------*
      *                  * Se il valore unitario determinato e' a zero *
      *                  * : uscita immediata lasciando il valore del- *
      *                  * la voce 'Costo del Fatturato' a zero        *
      *                  *---------------------------------------------*
           if        d-vun-mag-val-uni    =    zero
                     go to det-cos-fat-bir-999.
       det-cos-fat-bir-400.
      *              *-------------------------------------------------*
      *              * Preparazione del valore della voce 'Costo del   *
      *              * Fatturato' come moltiplicazione tra la Quanti-  *
      *              * ta' Fatturata e il Valore unitario appena de-   *
      *              * terminato                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-mod-fat-bir-qta    by   d-vun-mag-val-uni
                                        giving w-mod-fat-bir-cos
                                                         rounded      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           divide    100                  into w-mod-fat-bir-cos      .
       det-cos-fat-bir-500.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa all'espres-  *
      *              * sione dei costi unitari in unita' di misura     *
      *              * diversa                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N"
                     go to det-cos-fat-bir-600.
      *                  *---------------------------------------------*
      *                  * Applicazione del coefficiente               *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "D"
                     divide 10            into w-mod-fat-bir-cos
           else if   w-prs-arg-mag-ecu    =    "C"
                     divide 100           into w-mod-fat-bir-cos
           else if   w-prs-arg-mag-ecu    =    "K"
                     divide 1000          into w-mod-fat-bir-cos      .
       det-cos-fat-bir-600.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa all'utiliz-  *
      *              * zo della % di maggiorazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-mpa        not  = "M"
                     go to det-cos-fat-bir-900.
      *                  *---------------------------------------------*
      *                  * Determinazione % di maggiorazione           *
      *                  *---------------------------------------------*
           perform   det-cos-fat-mpa-000  thru det-cos-fat-mpa-999    .
      *                  *---------------------------------------------*
      *                  * Tese se % di maggiorazione determinata      *
      *                  *---------------------------------------------*
           if        w-aux-fat-bir-mpa    =    zero
                     go to det-cos-fat-bir-900.
      *                  *---------------------------------------------*
      *                  * Applicazione eventuale % di maggiorazione   *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-fat-bir-pma      .
           multiply  w-aux-fat-bir-mpa    by   w-mod-fat-bir-cos
                                        giving w-aux-fat-bir-pma      .
           divide    100                  into w-aux-fat-bir-pma
                                       rounded                        .
           add       w-aux-fat-bir-pma    to   w-mod-fat-bir-cos      .
       det-cos-fat-bir-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cos-fat-bir-999.
       det-cos-fat-bir-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione della % di maggiorazione    *
      *    *-----------------------------------------------------------*
       det-cos-fat-mpa-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del valore da determinare       *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-bir-mpa      .
       det-cos-fat-mpa-100.
      *              *-------------------------------------------------*
      *              * Lettura record [aaq]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      rf-bir-num-pro       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cos-fat-mpa-900.
      *                  *---------------------------------------------*
      *                  * Se fornitore preferenziale per il prodotto  *
      *                  * a zero : ad uscita                          *
      *                  *---------------------------------------------*
           if        rf-aaq-dcf-pfz       =    zero
                     go to det-cos-fat-mpa-900.
       det-cos-fat-mpa-200.
      *              *-------------------------------------------------*
      *              * Lettura record [aaf]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM"             to   f-key                  .
           move      01                   to   rf-aaf-tip-mag         .
           move      rf-bir-num-pro       to   rf-aaf-num-pro         .
           move      rf-aaq-dcf-pfz       to   rf-aaf-cod-dcf         .
           move      spaces               to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-cos-fat-mpa-900.
       det-cos-fat-mpa-400.
      *              *-------------------------------------------------*
      *              * Eventuale maggiorazione                         *
      *              *-------------------------------------------------*
           if        rf-aaf-per-mpa       =    zero
                     go to det-cos-fat-mpa-900.
           move      rf-aaf-per-mpa       to   w-aux-fat-bir-mpa      .
       det-cos-fat-mpa-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cos-fat-mpa-999.
       det-cos-fat-mpa-999.
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

      *    *===========================================================*
      *    * Subroutines per gestione file relative [rel]              *
      *    *-----------------------------------------------------------*
       fil-rel-opn-000.
      *              *=================================================*
      *              * Funzione Open                                   *
      *              *-------------------------------------------------*
       fil-rel-opn-100.
      *                  *---------------------------------------------*
      *                  * Richiesta alla segreteria di un pathname    *
      *                  * unico per files temporanei, per il file     *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fil-rel-opn-200.
      *                  *---------------------------------------------*
      *                  * Preparazione area ausiliaria per controllo  *
      *                  * i-o sul file relative                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * File name                               *
      *                      *-----------------------------------------*
           move      "rel "               to   f-rel-nam              .
      *                      *-----------------------------------------*
      *                      * File pathname                           *
      *                      *-----------------------------------------*
           move      s-pat                to   f-rel-pat              .
      *                      *-----------------------------------------*
      *                      * File status                             *
      *                      *-----------------------------------------*
           move      "00"                 to   f-rel-sts              .
      *                      *-----------------------------------------*
      *                      * Max record number scritto               *
      *                      *-----------------------------------------*
           move      zero                 to   f-rel-kms              .
      *                      *-----------------------------------------*
      *                      * Flag di Open non eseguita               *
      *                      *-----------------------------------------*
           move      spaces               to   f-rel-opn              .
       fil-rel-opn-999.
           exit.

       fil-rel-opd-000.
      *              *=================================================*
      *              * Subfunzione di Open effettiva, se necessario    *
      *              *-------------------------------------------------*
       fil-rel-opd-100.
      *                  *---------------------------------------------*
      *                  * Se funzione Open effettiva gia' eseguita :  *
      *                  * uscita immediata                            *
      *                  *---------------------------------------------*
           if        f-rel-opn            not  = spaces
                     go to fil-rel-opd-999.
       fil-rel-opd-200.
      *                  *---------------------------------------------*
      *                  * Open vera e propria, con conseguente crea-  *
      *                  * zione del file relative temporaneo          *
      *                  *---------------------------------------------*
           open      i-o   rel                                        .
       fil-rel-opd-300.
      *                  *---------------------------------------------*
      *                  * Flag di Open eseguita                       *
      *                  *---------------------------------------------*
           move      "#"                  to   f-rel-opn              .
       fil-rel-opd-999.
           exit.

       fil-rel-cls-000.
      *              *=================================================*
      *              * Funzione Close                                  *
      *              *-------------------------------------------------*
       fil-rel-cls-100.
      *                  *---------------------------------------------*
      *                  * Se Open mai eseguita : uscita immediata     *
      *                  *---------------------------------------------*
           if        f-rel-opn            =    spaces
                     go to fil-rel-cls-999.
       fil-rel-cls-200.
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           close     rel                                              .
       fil-rel-cls-300.
      *                  *---------------------------------------------*
      *                  * Cancellazione del file relative             *
      *                  *---------------------------------------------*
           delete    file    rel                                      .
       fil-rel-cls-400.
      *                  *---------------------------------------------*
      *                  * Flag di open non eseguita                   *
      *                  *---------------------------------------------*
           move      spaces               to   f-rel-opn              .
       fil-rel-cls-999.
           exit.

       fil-rel-new-000.
      *              *=================================================*
      *              * Funzione New                                    *
      *              *-------------------------------------------------*
       fil-rel-new-100.
      *                  *---------------------------------------------*
      *                  * Record number per scrittura a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   f-rel-put              .
      *                  *---------------------------------------------*
      *                  * Normalizzazione buffer cache memory         *
      *                  *---------------------------------------------*
           move      high-value           to   f-rel-chm-buf          .
       fil-rel-new-999.
           exit.

       fil-rel-inc-000.
      *              *=================================================*
      *              * Funzione Inc                                    *
      *              *-------------------------------------------------*
       fil-rel-inc-100.
      *                  *---------------------------------------------*
      *                  * Incremento record number per scrittura      *
      *                  *---------------------------------------------*
           add       1                    to   f-rel-put              .
       fil-rel-inc-999.
           exit.

       fil-rel-put-000.
      *              *=================================================*
      *              * Funzione Put                                    *
      *              *-------------------------------------------------*
       fil-rel-put-100.
      *                  *---------------------------------------------*
      *                  * Record number attuale pari al record number *
      *                  * per scrittura                               *
      *                  *---------------------------------------------*
           move      f-rel-put            to   f-rel-krn              .
       fil-rel-put-200.
      *                  *---------------------------------------------*
      *                  * Test se in cache memory e deviazione in     *
      *                  * conseguenza                                 *
      *                  *---------------------------------------------*
           if        f-rel-krn            >    f-rel-chm-max
                     go to fil-rel-put-400.
       fil-rel-put-300.
      *                  *---------------------------------------------*
      *                  * Se in cache memory                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione                          *
      *                      *-----------------------------------------*
           move      rel-rec              to   f-rel-chm-rec
                                              (f-rel-krn)             .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento del max record  *
      *                      * number scritto                          *
      *                      *-----------------------------------------*
           if        f-rel-krn            >    f-rel-kms
                     move  f-rel-krn      to   f-rel-kms              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fil-rel-put-999.
       fil-rel-put-400.
      *                  *---------------------------------------------*
      *                  * Se non in cache memory                      *
      *                  *---------------------------------------------*
       fil-rel-put-450.
      *                      *-----------------------------------------*
      *                      * Eventuale open effettiva                *
      *                      *-----------------------------------------*
           perform   fil-rel-opd-000      thru fil-rel-opd-999        .
       fil-rel-put-500.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del confronto tra  *
      *                      * il record number attuale ed il max re-  *
      *                      * cord number scritto                     *
      *                      *-----------------------------------------*
           if        f-rel-krn            not  > f-rel-kms
                     subtract f-rel-chm-max
                                          from f-rel-krn
                     go to fil-rel-put-600
           else      subtract f-rel-chm-max
                                          from f-rel-krn              .
       fil-rel-put-550.
      *                      *-----------------------------------------*
      *                      * Se il record number attuale e' maggiore *
      *                      * del max record number scritto si esegue *
      *                      * un tentativo per Write                  *
      *                      *-----------------------------------------*
           write     rel-rec invalid key
                             go to   fil-rel-put-600.
           go to     fil-rel-put-650.
       fil-rel-put-600.
      *                      *-----------------------------------------*
      *                      * Se il record number attuale non e' mag- *
      *                      * giore del max record number scritto si  *
      *                      * esegue un tentativo per Rewrite         *
      *                      *-----------------------------------------*
           rewrite   rel-rec invalid key
                             go to   fil-rel-put-550.
           go to     fil-rel-put-650.
       fil-rel-put-650.
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento del max record  *
      *                      * number scritto                          *
      *                      *-----------------------------------------*
           add       f-rel-chm-max        to   f-rel-krn              .
           if        f-rel-krn            >    f-rel-kms
                     move  f-rel-krn      to   f-rel-kms              .
       fil-rel-put-999.
           exit.

       fil-rel-get-000.
      *              *=================================================*
      *              * Funzione Get                                    *
      *              *-------------------------------------------------*
       fil-rel-get-100.
      *                  *---------------------------------------------*
      *                  * Record number attuale pari al record number *
      *                  * per lettura                                 *
      *                  *---------------------------------------------*
           move      f-rel-get            to   f-rel-krn              .
       fil-rel-get-200.
      *                  *---------------------------------------------*
      *                  * Se il record number attuale e' maggiore del *
      *                  * max record number scritto si esce sicura-   *
      *                  * mente come per record non esistente         *
      *                  *---------------------------------------------*
           if        f-rel-krn            >    f-rel-kms
                     go to fil-rel-get-700.
       fil-rel-get-300.
      *                  *---------------------------------------------*
      *                  * Test se in cache memory e deviazione in     *
      *                  * conseguenza                                 *
      *                  *---------------------------------------------*
           if        f-rel-krn            >    f-rel-chm-max
                     go to fil-rel-get-500.
       fil-rel-get-400.
      *                  *---------------------------------------------*
      *                  * Se in cache memory                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il record in cache memory e' pari al *
      *                      * valore convenzionale high-value si esce *
      *                      * come per record non esistente           *
      *                      *-----------------------------------------*
           if        f-rel-chm-rec
                    (f-rel-krn)           =    high-value
                     go to fil-rel-get-700.
      *                      *-----------------------------------------*
      *                      * Estrazione                              *
      *                      *-----------------------------------------*
           move      f-rel-chm-rec
                    (f-rel-krn)           to   rel-rec                .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     fil-rel-get-999.
       fil-rel-get-500.
      *                  *---------------------------------------------*
      *                  * Se non in cache memory                      *
      *                  *---------------------------------------------*
       fil-rel-get-550.
      *                      *-----------------------------------------*
      *                      * Eventuale open effettiva                *
      *                      *-----------------------------------------*
           perform   fil-rel-opd-000      thru fil-rel-opd-999        .
       fil-rel-get-600.
      *                      *-----------------------------------------*
      *                      * Lettura con deviazione a seconda del-   *
      *                      * l'esito                                 *
      *                      *-----------------------------------------*
           subtract  f-rel-chm-max        from f-rel-krn              .
           read      rel     invalid key
                             go to   fil-rel-get-700.
       fil-rel-get-650.
      *                      *-----------------------------------------*
      *                      * Se record esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     fil-rel-get-999.
       fil-rel-get-700.
      *                      *-----------------------------------------*
      *                      * Se record non esistente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Segnale di record non esistente     *
      *                          *-------------------------------------*
           move      zero                 to   f-rel-get              .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     fil-rel-get-999.
       fil-rel-get-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione valore unitario di magaz-  *
      *    * zino                                                      *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dvunmag0.dts"                   .

