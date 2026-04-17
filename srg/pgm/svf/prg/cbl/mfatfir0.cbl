       Identification Division.
       Program-Id.                                 mfatfir0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la lettura dei documenti, e per  *
      *                    la determinazione della voce 'Fatturato'    *
      *                    da trattare per le statistiche di vendita   *
      *                    sul fatturato su righe documento.           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * PROMEMORIA: - implementare la chiamata come 'batch' per "IE"   *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "OP"                 *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "CL"                 *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GS"  Inizializzazione lettura sequenziale [fit] e [fir]       *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "GS"                 *
      *                                                                *
      *                       w-mod-fat-fir-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-fat-fir-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-fat-fir-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-fat-fir-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-fat-fir-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GT"  Lettura sequenziale archivio [fit] e [fir]               *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "GT"                 *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-fit            : Record [fit] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-fir            : Record [fir] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-fat-fir-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-fat-fir-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-fir-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-fat-fir-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-fir-pon : Valore della voce    *
      *                                           con incidenza Po-    *
      *                                           sitiva o Negativa    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CS"  Inizializzazione lettura sequenziale [fit] e [fir] per   *
      *       un solo codice prodotto                                  *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "CS"                 *
      *                                                                *
      *                       w-mod-fat-fir-pro : Codice prodotto      *
      *                                                                *
      *                       w-mod-fat-fir-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-fat-fir-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-fat-fir-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-fat-fir-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-fat-fir-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-fat-fir-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CG"  Lettura sequenziale archivio [fit] e [fir] per un solo   *
      *       codice prodotto                                          *
      *                                                                *
      *              Input  : w-mod-fat-fir-ope : "CG"                 *
      *                                                                *
      *                       rf-fit            : Non significativo    *
      *                                                                *
      *                       rf-fir            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-fit            : Record [fit] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-fir            : Record [fir] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-fat-fir-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-fat-fir-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-fat-fir-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-fir-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-fat-fir-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-fat-fir-pon : Valore della voce    *
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
           05  rel-num-prt                pic  9(09)       comp-3     .
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
       01  w-aux-fat-fir.
      *        *-------------------------------------------------------*
      *        * Flag di apertura modulo per determinazione valore di  *
      *        * magazzino eseguita                                    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-fvm          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di inizializzazione lettura sequenziale [fit]    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-fgs          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di 'Tipo prossima lettura da eseguire'           *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-nxt          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records scritti in file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-csc          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records riletti da file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-crl          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice prodotto                                       *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-pro          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-npr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo calcolo voce 'Costo del Fatturato'               *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-ccf          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p1i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p1f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p2i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p2f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p3i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-p3f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 1. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-x1p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 2. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-x2p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 3. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-x3p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Quantita' fatturata'  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-qta          pic s9(13)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Fatturato'            *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-val          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Costo del Fatturato'  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-cos          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Incidenza in positivo *
      *        * o in negativo'                                        *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-pon          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto in chiusura in valuta base             *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-scc          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto pagamento in valuta base               *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-scp          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto totale in valuta base                  *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-sct          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto riga in valuta base                    *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-scr          pic s9(18)                  .
      *        *-------------------------------------------------------*
      *        * Importo totale in valuta base righe da includere nel- *
      *        * la statistica                                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-itr          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Totali di suddivisione per le righe, in valuta        *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-tri occurs 09
                                          pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita da selezione su record [fir]           *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-sel          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Percentuale di maggiorazione                          *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-mpa          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Maggiorazione                                         *
      *        *-------------------------------------------------------*
           05  w-aux-fat-fir-pma          pic s9(09)v9(05)            .

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
      *    * Link-area per modulo dell'area 'svf'           'mfatfir0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mfatfir0.mdl"                   .

      *    *===========================================================*
      *    * Record file [fit]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .

      *    *===========================================================*
      *    * Record file [fir]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .

      ******************************************************************
       Procedure Division                using w-mod-fat-fir
                                               rf-fit
                                               rf-fir                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-fat-fir-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-fat-fir-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-fat-fir-ope    =    "GS"
                     perform   gts-000    thru gts-999
           else if   w-mod-fat-fir-ope    =    "GT"
                     perform   get-000    thru get-999
           else if   w-mod-fat-fir-ope    =    "CS"
                     perform   cds-000    thru cds-999
           else if   w-mod-fat-fir-ope    =    "CG"
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
           move      "0"                  to   w-aux-fat-fir-fvm      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * non ancora eseguita Start                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-fat-fir-fgs      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di 'Tipo prossima lettura *
      *              * da eseguire' a : 'Indeterminato'                *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-fir-nxt      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione sul tipo fun-   *
      *              * zionamento statistiche sul fatturato su righe   *
      *              * documento                                       *
      *              *-------------------------------------------------*
           perform   prs-fat-fir-000      thru prs-fat-fir-999        .
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
           if        w-aux-fat-fir-fvm    =    "2"
                     go to opn-vun-mag-999.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione valore di magaz-  *
      *              * zino                                            *
      *              *-------------------------------------------------*
           perform   det-vun-mag-opn-000  thru det-vun-mag-opn-999    .
      *              *-------------------------------------------------*
      *              * Flag di apertura modulo a : in essere           *
      *              *-------------------------------------------------*
           move      "2"                  to   w-aux-fat-fir-fvm      .
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
           if        w-aux-fat-fir-fvm    not  = "2"
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
           move      "1"                  to   w-aux-fat-fir-fvm      .
       cls-vun-mag-100.
      *              *-------------------------------------------------*
      *              * Se modulo non chiuso : nessuna azione ed uscita *
      *              * immediata                                       *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-fvm    not  = "1"
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
           move      "0"                  to   w-aux-fat-fir-fvm      .
       cls-vun-mag-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione su statistiche sul fatturato    *
      *    * per tipo funzionamento statistiche su righe documento     *
      *    *-----------------------------------------------------------*
       prs-fat-fir-000.
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
                     w-mod-fat-fir-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-fat-fir-150.
       prs-fat-fir-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-fat-fir-500.
       prs-fat-fir-150.
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
           go to     prs-fat-fir-700.
       prs-fat-fir-500.
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
                     go to prs-fat-fir-650.
       prs-fat-fir-600.
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
           go to     prs-fat-fir-700.
       prs-fat-fir-650.
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
           go to     prs-fat-fir-700.
       prs-fat-fir-700.
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
       prs-fat-fir-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-fat-fir-999.
       prs-fat-fir-999.
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
      *    * Inizializzazione lettura sequenziale archivio [fit]       *
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
           move      "#"                  to   w-aux-fat-fir-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-fat-fir-nxt      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-npr    to   w-aux-fat-fir-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-ccf    to   w-aux-fat-fir-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p1i    to   w-aux-fat-fir-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p1f    to   w-aux-fat-fir-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p2i    to   w-aux-fat-fir-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p2f    to   w-aux-fat-fir-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p3i    to   w-aux-fat-fir-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p3f    to   w-aux-fat-fir-p3f      .
       gts-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale archivio [fit]                        *
      *    *-----------------------------------------------------------*
       get-000.
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
      *              * archivio [fit] ancora in Off : uscita come per  *
      *              * fine file                                       *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-fgs    =    spaces
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Se flag di ''Tipo prossima lettura da eseguire' *
      *              * al valore 'Indeterminato' : uscita come per fi- *
      *              * ne file                                         *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-nxt    =    zero
                     go to get-900.
       get-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-nxt    =    1
                     go to get-200
           else if   w-aux-fat-fir-nxt    =    2
                     go to get-225
           else if   w-aux-fat-fir-nxt    =    3
                     go to get-250
           else if   w-aux-fat-fir-nxt    =    4
                     go to get-300
           else if   w-aux-fat-fir-nxt    =    5
                     go to get-600
           else if   w-aux-fat-fir-nxt    =    6
                     go to get-700
           else      go to get-900.
       get-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [fit] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-fir-p1i    to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-doc         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-225.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 2. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [fit] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-fir-p2i    to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-doc         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-250.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 3. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [fit] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-aux-fat-fir-p3i    to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-doc         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fit]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [fit]                          *
      *              *-------------------------------------------------*
       get-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [fit]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
       get-330.
      *                  *---------------------------------------------*
      *                  * Selezione su codice dipendenza in base allo *
      *                  * status determinato                          *
      *                  *---------------------------------------------*
           move      "TS"                 to   w-dpz-tip-ope          .
           move      rf-fit-cod-dpz       to   w-dpz-cod-dpz          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione status     *
      *                      *-----------------------------------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to get-300.
       get-340.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-fit letta    *
      *                  *---------------------------------------------*
           perform   man-pre-fit-000      thru man-pre-fit-999        .
       get-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [fit], a seconda del numero di  *
      *                  * periodi di riferimento, e se non superato : *
      *                  * uscita per fine file. Inoltre test di ap-   *
      *                  * partenenza ad uno o piu' periodi di rife-   *
      *                  * rimento                                     *
      *                  *---------------------------------------------*
       get-355.
      *                      *-----------------------------------------*
      *                      * Appartenenza ai periodi di riferimento: *
      *                      * tutti a 'No'                            *
      *                      *-----------------------------------------*
           move      "N"                  to   w-aux-fat-fir-x1p      .
           move      "N"                  to   w-aux-fat-fir-x2p      .
           move      "N"                  to   w-aux-fat-fir-x3p      .
       get-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-fat-fir-npr    =    01
                     go to get-365
           else if   w-aux-fat-fir-npr    =    02
                     go to get-370
           else if   w-aux-fat-fir-npr    =    03
                     go to get-375.
       get-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       >    w-aux-fat-fir-p1f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S"
                     go to get-400.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-900.
       get-370.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 02   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       >    w-aux-fat-fir-p2f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p2i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p2f
                     move  "S"            to   w-aux-fat-fir-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S" or
                     w-aux-fat-fir-x2p    =    "S"
                     go to get-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       <    w-aux-fat-fir-p2i
                     move  2              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-900.
       get-375.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 03   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       >    w-aux-fat-fir-p3f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p2i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p2f
                     move  "S"            to   w-aux-fat-fir-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       not  < w-aux-fat-fir-p3i and
                     rf-fit-dat-doc       not  > w-aux-fat-fir-p3f
                     move  "S"            to   w-aux-fat-fir-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S" or
                     w-aux-fat-fir-x2p    =    "S" or
                     w-aux-fat-fir-x3p    =    "S"
                     go to get-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       <    w-aux-fat-fir-p2i
                     move  2              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fit-dat-doc       <    w-aux-fat-fir-p3i
                     move  3              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-900.
       get-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : flag di 'Tipo prossima let-  *
      *                  * tura da eseguire' a 'Read next su [fit] e   *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test sul flag di esclusione del docu-   *
      *                      * mento dalle statistiche                 *
      *                      *-----------------------------------------*
           if        rf-fit-flg-nbx (1)   not  = spaces
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al do-   *
      *                      * cumento non e' tra i seguenti :         *
      *                      *                                         *
      *                      *  - 01 : Fattura                     SI  *
      *                      *  - 02 : Nota di addebito            SI  *
      *                      *  - 03 : Nota di accredito           SI  *
      *                      *  - 04 : Fattura Pro-Forma           no  *
      *                      *  - 05 : Auto-fattura                no  *
      *                      *  - 06 : Acconto                     SI  *
      *                      *                                         *
      *                      * si esce per selezione non superata      *
      *                      *-----------------------------------------*
           if        rf-fit-tip-doc       not  = 01 and
                     rf-fit-tip-doc       not  = 02 and
                     rf-fit-tip-doc       not  = 03 and
                     rf-fit-tip-doc       not  = 06
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra quelli da includere    *
      *                      * nelle statistiche secondo quanto e'     *
      *                      * stato specificato con le personalizza-  *
      *                      * zioni : la selezione non e' superata    *
      *                      *-----------------------------------------*
           if        rf-fit-tip-doc       =    01    and
                     w-prs-fat-fir-fat    not  = "S"
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
           if        rf-fit-tip-doc       =    02    and
                     w-prs-fat-fir-ndb    not  = "S"
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
           if        rf-fit-tip-doc       =    03    and
                     w-prs-fat-fir-ncr    not  = "S"
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
           if        rf-fit-tip-doc       =    06    and
                     w-prs-fat-fir-acc    not  = "S"
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
       get-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-fit-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-fit-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-fit-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-fit-cdc-vpf       to   w-cvs-vlt-cdc          .
       get-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totali di suddivisione                  *
      *                      *-----------------------------------------*
           move      rf-fit-tot-rig (1)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (1)  .
      *
           move      rf-fit-tot-rig (2)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (2)  .
      *
           move      rf-fit-tot-rig (3)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (3)  .
      *
           move      rf-fit-tot-rig (4)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (4)  .
      *
           move      rf-fit-tot-rig (5)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (5)  .
      *
           move      rf-fit-tot-rig (6)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (6)  .
      *
           move      rf-fit-tot-rig (7)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (7)  .
      *
           move      rf-fit-tot-rig (8)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (8)  .
      *
           move      rf-fit-tot-rig (9)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (9)  .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-fit-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-fit-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-scp      .
       get-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scc    =    "S"
                     add   w-aux-fat-fir-scc
                                          to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scp    =    "S"
                     add   w-aux-fat-fir-scp
                                          to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Eventuale abbattimento totale libero 8  *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-rt8    =    "C"
                     subtract w-aux-fat-fir-tri (8)
                                          from w-aux-fat-fir-sct      .
       get-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si pone il flag di 'Tipo prossima lettura   *
      *                  * da eseguire' a 'Start su [fir]' e si rici-  *
      *                  * cla                                         *
      *                  *---------------------------------------------*
           if        w-aux-fat-fir-sct    =    zero
                     move  5              to   w-aux-fat-fir-nxt
                     go to get-100.
       get-525.
      *                  *---------------------------------------------*
      *                  * Determinazione importo totale, in valuta    *
      *                  * base, delle righe documento che dovranno    *
      *                  * successivamente essere incluse nella sta-   *
      *                  * tistica                                     *
      *                  *---------------------------------------------*
       get-530.
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare importo totale  *
      *                      * righe                                   *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-fir-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [fir]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-575.
       get-535.
      *                      *-----------------------------------------*
      *                      * Read next su [fir]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-fir     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-fir-000      thru man-pre-fir-999        .
       get-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-fir-num-prt       not  = rf-fit-num-prt
                     go to get-575.
       get-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [fir], se non superata : a *
      *                      * riciclo su read next su [fir]           *
      *                      *-----------------------------------------*
           perform   sel-rec-fir-000      thru sel-rec-fir-999        .
           if        w-aux-fat-fir-sel    not  = spaces
                     go to get-535.
       get-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-fat-fir-val    to   w-aux-fat-fir-itr      .
       get-555.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     get-535.
       get-575.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a 'Start su [fir]'                          *
      *                  *---------------------------------------------*
           move      5                    to   w-aux-fat-fir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-600.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start su [fir]                              *
      *              *-------------------------------------------------*
       get-625.
      *                  *---------------------------------------------*
      *                  * Start su [fir]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : flag di 'Tipo prossima    *
      *                  * lettura da eseguire' a 'Read next su [fit]' *
      *                  * e riciclo                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : flag di 'Tipo prossima lettura *
      *                  * da eseguire' a 'Read next su [fir]' e rici- *
      *                  * clo                                         *
      *                  *---------------------------------------------*
           move      6                    to   w-aux-fat-fir-nxt      .
           go to     get-100.
       get-700.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [fir]                          *
      *              *-------------------------------------------------*
       get-725.
      *                  *---------------------------------------------*
      *                  * Read next su [fir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se At end : flag di 'Tipo prossima lettura  *
      *                  * da eseguire' a 'Read next su [fit]' e ri-   *
      *                  * ciclo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-fir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-fir-000      thru man-pre-fir-999        .
       get-750.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato : flag di     *
      *                      * 'Tipo prossima lettura da eseguire' a   *
      *                      * 'Read next su [fit]' e riciclo          *
      *                      *-----------------------------------------*
           if        rf-fir-num-prt       not  = rf-fit-num-prt
                     move  4              to   w-aux-fat-fir-nxt
                     go to get-100.
       get-775.
      *                      *-----------------------------------------*
      *                      * Selezione su [fir], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [fir]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-fir-000      thru sel-rec-fir-999        .
           if        w-aux-fat-fir-sel    not  = spaces
                     move  6              to   w-aux-fat-fir-nxt
                     go to get-100.
       get-800.
      *                      *-----------------------------------------*
      *                      * Correzione eventuale della voce 'Fattu- *
      *                      * rato'                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se importo sconto totale in valuta  *
      *                          * base pari a zero : nessuna corre-   *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-sct    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-val    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-itr    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-fat-fir-sct    to   w-aux-fat-fir-scr      .
           multiply  w-aux-fat-fir-val    by   w-aux-fat-fir-scr      .
           divide    w-aux-fat-fir-itr    into w-aux-fat-fir-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-fat-fir-scr    from w-aux-fat-fir-val      .
       get-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-fat-fir-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x1p    to   w-mod-fat-fir-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x2p    to   w-mod-fat-fir-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x3p    to   w-mod-fat-fir-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-qta    to   w-mod-fat-fir-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-val    to   w-mod-fat-fir-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-fat-fir-000  thru det-cos-fat-fir-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-pon    to   w-mod-fat-fir-pon      .
       get-850.
      *                      *-----------------------------------------*
      *                      * Flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [fir]'           *
      *                      *-----------------------------------------*
           move      6                    to   w-aux-fat-fir-nxt      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-999.
       get-900.
      *              *-------------------------------------------------*
      *              * Uscita per fine file                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mod-fat-fir-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-fat-fir-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-999.
       get-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [fir] per   *
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
           move      "#"                  to   w-aux-fat-fir-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-fat-fir-nxt      .
      *              *-------------------------------------------------*
      *              * Inizializzazione nuovo ciclo di scrittura/let-  *
      *              * tura file relative di appoggio                  *
      *              *-------------------------------------------------*
           perform   fil-rel-new-000      thru fil-rel-new-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori per scrittura e ri-  *
      *              * lettura file relative                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-fir-csc      .
           move      zero                 to   w-aux-fat-fir-crl      .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice prodotto                  *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-pro    to   w-aux-fat-fir-pro      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-npr    to   w-aux-fat-fir-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-ccf    to   w-aux-fat-fir-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p1i    to   w-aux-fat-fir-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p1f    to   w-aux-fat-fir-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p2i    to   w-aux-fat-fir-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p2f    to   w-aux-fat-fir-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p3i    to   w-aux-fat-fir-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-fat-fir-p3f    to   w-aux-fat-fir-p3f      .
       cds-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-nxt    =    1
                     go to cds-200
           else if   w-aux-fat-fir-nxt    =    2
                     go to cds-225
           else if   w-aux-fat-fir-nxt    =    3
                     go to cds-250
           else if   w-aux-fat-fir-nxt    =    4
                     go to cds-300
           else      go to cds-900.
       cds-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [fir] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRODAT    "         to   f-key                  .
           move      01                   to   rf-fir-tip-mag         .
           move      w-aux-fat-fir-pro    to   rf-fir-num-pro         .
           move      w-aux-fat-fir-p1i    to   rf-fir-dat-doc         .
           move      zero                 to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
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
      *                  * Start su archivio [fir] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRODAT    "         to   f-key                  .
           move      01                   to   rf-fir-tip-mag         .
           move      w-aux-fat-fir-pro    to   rf-fir-num-pro         .
           move      w-aux-fat-fir-p2i    to   rf-fir-dat-doc         .
           move      zero                 to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
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
      *                  * Start su archivio [fir] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PRODAT    "         to   f-key                  .
           move      01                   to   rf-fir-tip-mag         .
           move      w-aux-fat-fir-pro    to   rf-fir-num-pro         .
           move      w-aux-fat-fir-p3i    to   rf-fir-dat-doc         .
           move      zero                 to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fir]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cds-100.
       cds-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [fir]                          *
      *              *-------------------------------------------------*
       cds-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [fir]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-fir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-fir-000      thru man-pre-fir-999        .
       cds-330.
      *                  *---------------------------------------------*
      *                  * Test Max su [fir] per il tipo e il codice   *
      *                  * prodotto : se non superato, uscita per      *
      *                  * fine file                                   *
      *                  *---------------------------------------------*
           if        rf-fir-tip-mag       not  = 01                or
                     rf-fir-num-pro       not  = w-aux-fat-fir-pro
                     go to cds-900.
       cds-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [fir], a seconda del numero di  *
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
           move      "N"                  to   w-aux-fat-fir-x1p      .
           move      "N"                  to   w-aux-fat-fir-x2p      .
           move      "N"                  to   w-aux-fat-fir-x3p      .
       cds-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-fat-fir-npr    =    01
                     go to cds-365
           else if   w-aux-fat-fir-npr    =    02
                     go to cds-370
           else if   w-aux-fat-fir-npr    =    03
                     go to cds-375.
       cds-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       >    w-aux-fat-fir-p1f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S"
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
           if        rf-fir-dat-doc       >    w-aux-fat-fir-p2f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p2i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p2f
                     move  "S"            to   w-aux-fat-fir-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S" or
                     w-aux-fat-fir-x2p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       <    w-aux-fat-fir-p2i
                     move  2              to   w-aux-fat-fir-nxt
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
           if        rf-fir-dat-doc       >    w-aux-fat-fir-p3f
                     go to cds-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p1i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p1f
                     move  "S"            to   w-aux-fat-fir-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p2i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p2f
                     move  "S"            to   w-aux-fat-fir-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       not  < w-aux-fat-fir-p3i and
                     rf-fir-dat-doc       not  > w-aux-fat-fir-p3f
                     move  "S"            to   w-aux-fat-fir-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-x1p    =    "S" or
                     w-aux-fat-fir-x2p    =    "S" or
                     w-aux-fat-fir-x3p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       <    w-aux-fat-fir-p2i
                     move  2              to   w-aux-fat-fir-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fir-dat-doc       <    w-aux-fat-fir-p3i
                     move  3              to   w-aux-fat-fir-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     cds-900.
       cds-400.
      *                      *-----------------------------------------*
      *                      * Selezione su [fir], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [fir]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-fir-000      thru sel-rec-fir-999        .
           if        w-aux-fat-fir-sel    not  = spaces
                     move  4              to   w-aux-fat-fir-nxt
                     go to cds-100.
       cds-500.
      *              *-------------------------------------------------*
      *              * Scrittura file relative di appoggio [rel]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records scritti in fi- *
      *                  * le di appoggio [rel]                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-fat-fir-csc      .
           move      w-aux-fat-fir-csc    to   f-rel-put              .
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
           move      rf-fir-num-prt       to   rel-num-prt            .
      *                      *-----------------------------------------*
      *                      * Numero progressivo protocollo fattura   *
      *                      *-----------------------------------------*
           move      rf-fir-num-prg       to   rel-num-prg            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x1p    to   rel-snx-x1p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x2p    to   rel-snx-x2p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-x3p    to   rel-snx-x3p            .
      *                  *---------------------------------------------*
      *                  * Esecuzione funzione Put su file relative    *
      *                  * di appoggio [rel]                           *
      *                  *---------------------------------------------*
           perform   fil-rel-put-000      thru fil-rel-put-999        .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a   *
      *              * 'Read next su [fir]'                            *
      *              *-------------------------------------------------*
           move      4                    to   w-aux-fat-fir-nxt      .
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
           add       1                    to   w-aux-fat-fir-crl      .
      *                  *---------------------------------------------*
      *                  * Test se oltre il numero records scritti     *
      *                  *---------------------------------------------*
           if        w-aux-fat-fir-crl    >    w-aux-fat-fir-csc
                     go to cdg-900.
      *                  *---------------------------------------------*
      *                  * Preparazione numero record per rilettura    *
      *                  *---------------------------------------------*
           move      w-aux-fat-fir-crl    to   f-rel-get              .
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
      *                  * Lettura record [fit]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-fit letta    *
      *                  *---------------------------------------------*
           perform   man-pre-fit-000      thru man-pre-fit-999        .
       cdg-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : riciclo su lettura file re-  *
      *                  * lative                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al do-   *
      *                      * cumento non e' tra i seguenti :         *
      *                      *                                         *
      *                      *  - 01 : Fattura                     SI  *
      *                      *  - 02 : Nota di addebito            SI  *
      *                      *  - 03 : Nota di accredito           SI  *
      *                      *  - 04 : Fattura Pro-Forma           no  *
      *                      *  - 05 : Auto-fattura                no  *
      *                      *  - 06 : Acconto                     SI  *
      *                      *                                         *
      *                      * si esce per selezione non superata      *
      *                      *-----------------------------------------*
           if        rf-fit-tip-doc       not  = 01 and
                     rf-fit-tip-doc       not  = 02 and
                     rf-fit-tip-doc       not  = 03 and
                     rf-fit-tip-doc       not  = 06
                     go to cdg-000.
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra quelli da includere    *
      *                      * nelle statistiche secondo quanto e'     *
      *                      * stato specificato con le personalizza-  *
      *                      * zioni : la selezione non e' superata    *
      *                      *-----------------------------------------*
           if        rf-fit-tip-doc       =    01    and
                     w-prs-fat-fir-fat    not  = "S"
                     go to cdg-000.
           if        rf-fit-tip-doc       =    02    and
                     w-prs-fat-fir-ndb    not  = "S"
                     go to cdg-000.
           if        rf-fit-tip-doc       =    03    and
                     w-prs-fat-fir-ncr    not  = "S"
                     go to cdg-000.
           if        rf-fit-tip-doc       =    06    and
                     w-prs-fat-fir-acc    not  = "S"
                     go to cdg-000.
       cdg-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-fit-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-fit-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-fit-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-fit-cdc-vpf       to   w-cvs-vlt-cdc          .
       cdg-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Totali di suddivisione                  *
      *                      *-----------------------------------------*
           move      rf-fit-tot-rig (1)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (1)  .
      *
           move      rf-fit-tot-rig (2)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (2)  .
      *
           move      rf-fit-tot-rig (3)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (3)  .
      *
           move      rf-fit-tot-rig (4)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (4)  .
      *
           move      rf-fit-tot-rig (5)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (5)  .
      *
           move      rf-fit-tot-rig (6)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (6)  .
      *
           move      rf-fit-tot-rig (7)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (7)  .
      *
           move      rf-fit-tot-rig (8)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (8)  .
      *
           move      rf-fit-tot-rig (9)   to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-tri (9)  .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-fit-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-fit-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-scp      .
       cdg-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scc    =    "S"
                     add   w-aux-fat-fir-scc
                                          to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-scp    =    "S"
                     add   w-aux-fat-fir-scp
                                          to   w-aux-fat-fir-sct      .
      *                      *-----------------------------------------*
      *                      * Totale di suddivisione righe 8          *
      *                      *-----------------------------------------*
           if        w-prs-fat-fir-rt8    =    "C"
                     subtract w-aux-fat-fir-tri (8)
                                          from w-aux-fat-fir-sct      .
       cdg-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si va a trattamento riga fattura            *
      *                  *---------------------------------------------*
           if        w-aux-fat-fir-sct    =    zero
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
           move      zero                 to   w-aux-fat-fir-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [fir]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fit-num-prt       to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
       cdg-535.
      *                      *-----------------------------------------*
      *                      * Read next su [fir]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-fir     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-fir-000      thru man-pre-fir-999        .
       cdg-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-fir-num-prt       not  = rf-fit-num-prt
                     go to cdg-575.
       cdg-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [fir], se non superata : a *
      *                      * riciclo su read next su [fir]           *
      *                      *-----------------------------------------*
           perform   sel-rec-fir-000      thru sel-rec-fir-999        .
           if        w-aux-fat-fir-sel    not  = spaces
                     go to cdg-535.
       cdg-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-fat-fir-val    to   w-aux-fat-fir-itr      .
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
      *                  * Lettura record [fir]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-fir-num-prt         .
           move      rel-num-prg          to   rf-fir-num-prg         .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-fir letta       *
      *                  *---------------------------------------------*
           perform   man-pre-fir-000      thru man-pre-fir-999        .
       cdg-700.
      *                      *-----------------------------------------*
      *                      * Selezione su [fir], se non superata :   *
      *                      * riciclo su lettura file relative        *
      *                      *-----------------------------------------*
           perform   sel-rec-fir-000      thru sel-rec-fir-999        .
           if        w-aux-fat-fir-sel    not  = spaces
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
           if        w-aux-fat-fir-sct    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-val    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-fat-fir-itr    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-fat-fir-sct    to   w-aux-fat-fir-scr      .
           multiply  w-aux-fat-fir-val    by   w-aux-fat-fir-scr      .
           divide    w-aux-fat-fir-itr    into w-aux-fat-fir-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-fat-fir-scr    from w-aux-fat-fir-val      .
       cdg-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-fat-fir-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x1p          to   w-mod-fat-fir-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x2p          to   w-mod-fat-fir-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x3p          to   w-mod-fat-fir-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-qta    to   w-mod-fat-fir-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-val    to   w-mod-fat-fir-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-fat-fir-000  thru det-cos-fat-fir-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-fat-fir-pon    to   w-mod-fat-fir-pon      .
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
           move      "#"                  to   w-mod-fat-fir-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-fat-fir-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-fat-fir-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-fat-fir-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cdg-999.
       cdg-999.
           exit.

      *    *===========================================================*
      *    * Selezione e determinazioni su record [fir]                *
      *    *-----------------------------------------------------------*
       sel-rec-fir-000.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari generali                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione su blocco documento: non gestito  *
      *                  *---------------------------------------------*
           go to     sel-rec-fir-020.
       sel-rec-fir-005.
      *                  *---------------------------------------------*
      *                  * Selezione su Flag blocco di documento       *
      *                  *---------------------------------------------*
           if        rf-fir-bld-flb       not  = zero
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-010.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo blocco documento          *
      *                  *---------------------------------------------*
           if        rf-fir-bld-tpb       not  = zero
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-015.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga nel blocco di docu-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           if        rf-fir-bld-rgb       not  = zero
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-020.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga                      *
      *                  *---------------------------------------------*
           if        rf-fir-tip-rig       not  = "P    "
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-025.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo codice di magazzino       *
      *                  *---------------------------------------------*
           if        rf-fir-tip-mag       not  = 01
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-030.
      *                  *---------------------------------------------*
      *                  * Selezione su Codice prodotto numerico       *
      *                  *---------------------------------------------*
           if        rf-fir-num-pro       =    zero
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-035.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-fir-tip-pro       <    01 or
                     rf-fir-tip-pro       >    09
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-040.
      *                  *---------------------------------------------*
      *                  * Selezione su Quantita' e Valore             *
      *                  *---------------------------------------------*
           if        rf-fir-qta-ven       =    zero and
                     rf-fir-imp-rig       =    zero
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-045.
      *                  *---------------------------------------------*
      *                  * Selezione su righe di omaggio               *
      *                  *---------------------------------------------*
           move      rf-fir-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    9
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-100.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari su personalizzazioni      *
      *              *-------------------------------------------------*
       sel-rec-fir-105.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-fir-tip-pro       =    01
                     if    w-prs-fat-fir-rtm
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    02
                     if    w-prs-fat-fir-rts
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    03
                     if    w-prs-fat-fir-rti
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    04
                     if    w-prs-fat-fir-rt4
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    05
                     if    w-prs-fat-fir-rt5
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    06
                     if    w-prs-fat-fir-rt6
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    07
                     if    w-prs-fat-fir-rt7
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    08
                     if    w-prs-fat-fir-rt8
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
           if        rf-fir-tip-pro       =    09
                     if    w-prs-fat-fir-rtx
                                          not  = "S"
                     move  "#"            to   w-aux-fat-fir-sel
                     go to sel-rec-fir-999.
       sel-rec-fir-200.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Quantita' fatturata'         *
      *              *-------------------------------------------------*
           move      rf-fir-qta-ven       to   w-aux-fat-fir-qta      .
       sel-rec-fir-300.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Fatturato', con conversione  *
      *              * in valuta base                                  *
      *              *-------------------------------------------------*
           move      rf-fir-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-fat-fir-val      .
       sel-rec-fir-400.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Incidenza in positivo o in   *
      *              * negativo'                                       *
      *              *-------------------------------------------------*
           if        rf-fit-tip-doc       =    01 or
                     rf-fit-tip-doc       =    02 or
                     rf-fit-tip-doc       =    06
                     move  "P"            to   w-aux-fat-fir-pon
           else      move  "N"            to   w-aux-fat-fir-pon      .
       sel-rec-fir-900.
      *              *-------------------------------------------------*
      *              * Flag di selezione superata                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-fat-fir-sel      .
       sel-rec-fir-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla testa- *
      *    * ta documento rf-fit letta                                 *
      *    *-----------------------------------------------------------*
       man-pre-fit-000.
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
           move      rf-fit-cod-cli       to   rf-dcc-cod-cli         .
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
                     go to man-pre-fit-400.
       man-pre-fit-200.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del cliente non    *
      *                  * indica che e' stato sostituito da una altro *
      *                  * cliente : nessuna manipolazione             *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tus       not  = 21 and
                     rf-dcc-sta-tus       not  = 52 and
                     rf-dcc-sta-tus       not  = 62 and
                     rf-dcc-sta-tus       not  = 72
                     go to man-pre-fit-400.
      *                  *---------------------------------------------*
      *                  * Se il codice cliente di riferimento e' a    *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tuc       =    zero
                     go to man-pre-fit-400.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo cliente : nessuna mani- *
      *                  * polazione                                   *
      *                  *---------------------------------------------*
           if        rf-dcc-sta-tux       not  = 02 and
                     rf-dcc-sta-tux       not  = 03
                     go to man-pre-fit-400.
       man-pre-fit-300.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * del cliente, ed eventualmente anche la di-  *
      *                  * pendenza                                    *
      *                  *---------------------------------------------*
           move      rf-dcc-sta-tuc       to   rf-fit-cod-cli         .
           if        rf-dcc-sta-tux       =    02
                     move  spaces         to   rf-fit-dpz-cli         .
       man-pre-fit-400.
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
           move      rf-fit-cod-age       to   rf-age-cod-age         .
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
                     go to man-pre-fit-600.
       man-pre-fit-420.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale dell'agente non    *
      *                  * indica che e' stato sostituito da una altro *
      *                  * agente : nessuna manipolazione              *
      *                  *---------------------------------------------*
           if        rf-age-sta-tus       not  = 21 and
                     rf-age-sta-tus       not  = 52 and
                     rf-age-sta-tus       not  = 62 and
                     rf-age-sta-tus       not  = 72
                     go to man-pre-fit-600.
      *                  *---------------------------------------------*
      *                  * Se il codice agente di riferimento e' a     *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-age-sta-tuc       =    zero
                     go to man-pre-fit-600.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo agente : nessuna mani-  *
      *                  * polazione                                   *
      *                  *---------------------------------------------*
           if        rf-age-sta-tux       not  = 02
                     go to man-pre-fit-600.
       man-pre-fit-430.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * dell'agente                                 *
      *                  *---------------------------------------------*
           move      rf-age-sta-tuc       to   rf-fit-cod-age         .
       man-pre-fit-600.
       man-pre-fit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     man-pre-fit-999.
       man-pre-fit-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla riga   *
      *    * documento rf-fir letta                                    *
      *    *-----------------------------------------------------------*
       man-pre-fir-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice del prodotto  *
      *              * in funzione dello status espresso in anagrafica *
      *              * commerciale                                     *
      *              *-------------------------------------------------*
       man-pre-fir-100.
      *                  *---------------------------------------------*
      *                  * Se il tipo riga non indica un prodotto :    *
      *                  * nessuna manipolazione                       *
      *                  *---------------------------------------------*
           if        rf-fir-tip-rig       not  = "P    "
                     go to man-pre-fir-900.
      *                  *---------------------------------------------*
      *                  * Se il tipo codice di magazzino non indica   *
      *                  * un prodotto : nessuna manipolazione         *
      *                  *---------------------------------------------*
           if        rf-fir-tip-mag       not  = 01
                     go to man-pre-fir-900.
       man-pre-fir-200.
      *                  *---------------------------------------------*
      *                  * Lettura record dell'anagrafica commerciale  *
      *                  * del prodotto                                *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-fir-num-pro       to   rf-dcp-num-pro         .
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
                     go to man-pre-fir-900.
       man-pre-fir-300.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del prodotto non   *
      *                  * indica che e' stato sostituito da un' altro *
      *                  * prodotto : nessuna manipolazione            *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tus       not  = 21 and
                     rf-dcp-sta-tus       not  = 52 and
                     rf-dcp-sta-tus       not  = 72
                     go to man-pre-fir-900.
      *                  *---------------------------------------------*
      *                  * Se il codice prodotto di riferimento e' a   *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tuc       =    zero
                     go to man-pre-fir-900.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo prodotto : nessuna ma-  *
      *                  * nipolazione                                 *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tux       not  = 02
                     go to man-pre-fir-900.
       man-pre-fir-400.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * numerico del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-sta-tuc       to   rf-fir-num-pro         .
       man-pre-fir-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     man-pre-fir-999.
       man-pre-fir-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, in w-mod-fat-fir-cos, del- *
      *    * la voce 'Costo del Fatturato' corrispondente al record    *
      *    * in 'rf-fir'                                               *
      *    *                                                           *
      *    * Input  : rf-fir            = Record 'rf-fir' in esame'    *
      *    *                                                           *
      *    *          w-aux-fat-fir-ccf = Tipo di calcolo per la voce  *
      *    *                                                           *
      *    *          w-mod-fat-fir-qta = Valore della voce 'Quanti-   *
      *    *                              ta' Fatturata' gia' deter-   *
      *    *                              minata                       *
      *    *                                                           *
      *    * Output : w-mod-fat-fir-cos = Valore della voce 'Costo del *
      *    *                              Fatturato'                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-cos-fat-fir-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preventivo del valore da determina- *
      *              * re                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-fat-fir-cos      .
       det-cos-fat-fir-100.
      *              *-------------------------------------------------*
      *              * Se tipo calcolo non riconosciuto : uscita imme- *
      *              * diata senza nessuna ulteriore azione            *
      *              *-------------------------------------------------*
           if        w-aux-fat-fir-ccf    not  = "P" and
                     w-aux-fat-fir-ccf    not  = "U"
                     go to det-cos-fat-fir-999.
      *              *-------------------------------------------------*
      *              * Se la quantita' fatturata di riferimento e' a   *
      *              * zero : uscita immediata senza nessuna ulteriore *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-mod-fat-fir-qta    =    zero
                     go to det-cos-fat-fir-999.
      *              *-------------------------------------------------*
      *              * Se tipo codice di magazzino non ammesso per la  *
      *              * determinazione del costo unitario : uscita sen- *
      *              * za nessuna ulteriore azione                     *
      *              *-------------------------------------------------*
           if        rf-fir-tip-mag       not  = 01 and
                     rf-fir-tip-mag       not  = 02 and
                     rf-fir-tip-mag       not  = 03 and
                     rf-fir-tip-mag       not  = 04
                     go to det-cos-fat-fir-999.
      *              *-------------------------------------------------*
      *              * Se codice numerico di magazzino a zero : uscita *
      *              * senza nessuna ulteriore azione                  *
      *              *-------------------------------------------------*
           if        rf-fir-num-pro       =    zero
                     go to det-cos-fat-fir-999.
       det-cos-fat-fir-200.
      *              *-------------------------------------------------*
      *              * Apertura preliminare, se necessario, del modulo *
      *              * per la determinazione del valore unitario di    *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           perform   opn-vun-mag-000      thru opn-vun-mag-999        .
       det-cos-fat-fir-300.
      *              *-------------------------------------------------*
      *              * Determinazione del valore unitario di magazzino *
      *              *-------------------------------------------------*
       det-cos-fat-fir-310.
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
           if        w-aux-fat-fir-ccf    =    "P"
                     move  0001           to   d-vun-mag-tip-val
           else if   w-aux-fat-fir-ccf    =    "U"
                     move  0002           to   d-vun-mag-tip-val
           else      move  0001           to   d-vun-mag-tip-val      .
      *                      *-----------------------------------------*
      *                      * Data di riferimento per il valore uni-  *
      *                      * tario: pari alla data documento se non  *
      *                      * e' stata passata una data valorizzazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        w-mod-fat-fir-p2i    =    zero
                     move  rf-fir-dat-doc to   d-vun-mag-dat-val
           else      move  w-mod-fat-fir-p2i
                                          to   d-vun-mag-dat-val      .
      *                      *-----------------------------------------*
      *                      * Tipo codice di magazzino                *
      *                      *-----------------------------------------*
           move      rf-fir-tip-mag       to   d-vun-mag-tip-mag      .
      *                      *-----------------------------------------*
      *                      * Codice numerico di magazzino            *
      *                      *-----------------------------------------*
           move      rf-fir-num-pro       to   d-vun-mag-num-mag      .
      *                      *-----------------------------------------*
      *                      * Giacenza di proprieta': non influente   *
      *                      *-----------------------------------------*
           move      zero                 to   d-vun-mag-gia-prp      .
       det-cos-fat-fir-320.
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           perform   det-vun-mag-cll-000  thru det-vun-mag-cll-999    .
       det-cos-fat-fir-330.
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione nel modulo : uscita *
      *                  * immediata lasciando il valore della voce    *
      *                  * 'Costo del Fatturato' a zero                *
      *                  *---------------------------------------------*
           if        d-vun-mag-exi-sts    not  = spaces
                     go to det-cos-fat-fir-999.
      *                  *---------------------------------------------*
      *                  * Se il valore unitario determinato e' a zero *
      *                  * : uscita immediata lasciando il valore del- *
      *                  * la voce 'Costo del Fatturato' a zero        *
      *                  *---------------------------------------------*
           if        d-vun-mag-val-uni    =    zero
                     go to det-cos-fat-fir-999.
       det-cos-fat-fir-400.
      *              *-------------------------------------------------*
      *              * Preparazione del valore della voce 'Costo del   *
      *              * Fatturato' come moltiplicazione tra la Quanti-  *
      *              * ta' Fatturata e il Valore unitario appena de-   *
      *              * terminato                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-mod-fat-fir-qta    by   d-vun-mag-val-uni
                                        giving w-mod-fat-fir-cos
                                                         rounded      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           divide    100                  into w-mod-fat-fir-cos      .
       det-cos-fat-fir-500.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa all'espres-  *
      *              * sione dei costi unitari in unita' di misura     *
      *              * diversa                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "N"
                     go to det-cos-fat-fir-600.
      *                  *---------------------------------------------*
      *                  * Applicazione del coefficiente               *
      *                  *---------------------------------------------*
           if        w-prs-arg-mag-ecu    =    "D"
                     divide 10            into w-mod-fat-fir-cos
           else if   w-prs-arg-mag-ecu    =    "C"
                     divide 100           into w-mod-fat-fir-cos
           else if   w-prs-arg-mag-ecu    =    "K"
                     divide 1000          into w-mod-fat-fir-cos      .
       det-cos-fat-fir-600.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione relativa all'utiliz-  *
      *              * zo della % di maggiorazione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-mpa        not  = "M"
                     go to det-cos-fat-fir-900.
      *                  *---------------------------------------------*
      *                  * Determinazione % di maggiorazione           *
      *                  *---------------------------------------------*
           perform   det-cos-fat-mpa-000  thru det-cos-fat-mpa-999    .
      *                  *---------------------------------------------*
      *                  * Tese se % di maggiorazione determinata      *
      *                  *---------------------------------------------*
           if        w-aux-fat-fir-mpa    =    zero
                     go to det-cos-fat-fir-900.
      *                  *---------------------------------------------*
      *                  * Applicazione eventuale % di maggiorazione   *
      *                  *---------------------------------------------*
           move      zero                 to   w-aux-fat-fir-pma      .
           multiply  w-aux-fat-fir-mpa    by   w-mod-fat-fir-cos
                                        giving w-aux-fat-fir-pma      .
           divide    100                  into w-aux-fat-fir-pma
                                       rounded                        .
           add       w-aux-fat-fir-pma    to   w-mod-fat-fir-cos      .
       det-cos-fat-fir-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-cos-fat-fir-999.
       det-cos-fat-fir-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione della % di maggiorazione    *
      *    *-----------------------------------------------------------*
       det-cos-fat-mpa-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione del valore da determinare       *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-fat-fir-mpa      .
       det-cos-fat-mpa-100.
      *              *-------------------------------------------------*
      *              * Lettura record [aaq]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      01                   to   rf-aaq-tip-mag         .
           move      rf-fir-num-pro       to   rf-aaq-num-pro         .
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
           move      rf-fir-num-pro       to   rf-aaf-num-pro         .
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
           move      rf-aaf-per-mpa       to   w-aux-fat-fir-mpa      .
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

