       Identification Division.
       Program-Id.                                 mffoffr0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    saf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/92    *
      *                       Ultima revisione:    NdK del 29/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la lettura dei documenti, e per  *
      *                    la determinazione della voce 'Fatturato'    *
      *                    da trattare per le statistiche di acquisto  *
      *                    sul fatturato su righe documento.           *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * MEMO           :   Le Start sono tutte per la Dipendenza '01', *
      *                    cioe' la Sede. Si tratta di una carenza     *
      *                    strutturale di difficile risoluzione!       *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "OP"                 *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "CL"                 *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GS"  Inizializzazione lettura sequenziale [fft] e [ffr]       *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "GS"                 *
      *                                                                *
      *                       w-mod-ffo-ffr-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-ffo-ffr-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-ffo-ffr-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-ffo-ffr-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-ffo-ffr-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "GT"  Lettura sequenziale archivio [fft] e [ffr]               *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "GT"                 *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-fft            : Record [fft] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-ffr            : Record [ffr] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-ffo-ffr-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-ffo-ffr-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-ffo-ffr-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-ffo-ffr-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-ffo-ffr-pon : Valore della voce    *
      *                                           con incidenza Po-    *
      *                                           sitiva o Negativa    *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CS"  Inizializzazione lettura sequenziale [fft] e [ffr] per   *
      *       un solo codice prodotto                                  *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "CS"                 *
      *                                                                *
      *                       w-mod-ffo-ffr-tmg : Tipo voce di magaz-  *
      *                                           zino                 *
      *                                                                *
      *                       w-mod-ffo-ffr-pro : Codice prodotto      *
      *                                                                *
      *                       w-mod-ffo-ffr-npr : Numero periodi di    *
      *                                           riferimento          *
      *                                                                *
      *                       w-mod-ffo-ffr-ccf : Tipo calcolo voce    *
      *                                           'Costo del Fattura-  *
      *                                           to'                  *
      *                                                                *
      *                       w-mod-ffo-ffr-p1i : 1. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p1f : 1. periodo data max  *
      *                                                                *
      *                       w-mod-ffo-ffr-p2i : 2. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p2f : 2. periodo data max  *
      *                                                                *
      *                       w-mod-ffo-ffr-p3i : 3. periodo data min  *
      *                                                                *
      *                       w-mod-ffo-ffr-p3f : 3. periodo data max  *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CG"  Lettura sequenziale archivio [fft] e [ffr] per un solo   *
      *       codice prodotto                                          *
      *                                                                *
      *              Input  : w-mod-ffo-ffr-ope : "CG"                 *
      *                                                                *
      *                       rf-fft            : Non significativo    *
      *                                                                *
      *                       rf-ffr            : Non significativo    *
      *                                                                *
      *                                                                *
      *              Output : rf-fft            : Record [fft] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       rf-ffr            : Record [ffr] ottenu- *
      *                                           to                   *
      *                                                                *
      *                       w-mod-ffo-ffr-flg : Si/No fine file      *
      *                                             - Spaces : No      *
      *                                             - #      : Si      *
      *                                                                *
      *                       w-mod-ffo-ffr-sn1 : Si/No record rife-   *
      *                                           rito al 1. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-sn2 : Si/No record rife-   *
      *                                           rito al 2. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-sn3 : Si/No record rife-   *
      *                                           rito al 3. periodo   *
      *                                             - S : Si'          *
      *                                             - N : No           *
      *                                                                *
      *                       w-mod-ffo-ffr-qta : Valore della voce    *
      *                                           'Quantita' Fattura-  *
      *                                           ta' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-ffo-ffr-val : Valore della voce    *
      *                                           'Fatturato' deter-   *
      *                                           minata dal modulo    *
      *                                                                *
      *                       w-mod-ffo-ffr-cos : Valore della voce    *
      *                                           'Costo del Fattura-  *
      *                                           to' determinata dal  *
      *                                           modulo               *
      *                                                                *
      *                       w-mod-ffo-ffr-pon : Valore della voce    *
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
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/saf/ffoffr[tip-fun]'      *
      *    *-----------------------------------------------------------*
       01  w-prs-ffo-ffr.
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Fatture                              *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-fat          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Note di addebito                     *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-ndb          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione Note di accredito                    *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-ncr          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Filler riempitivo                                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Merce                  *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rtm          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Servizio               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Imballo                *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rti          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 4               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rt4          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 5               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rt5          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 6               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rt6          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 7               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rt7          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Libero 8               *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rt8          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No inclusione righe di tipo Extra Attivita'        *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-rtx          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Filler riempitivo                                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No detrazione Sconto in chiusura                   *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-scc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No detrazione Sconto pagamento                     *
      *        *-------------------------------------------------------*
           05  w-prs-ffo-ffr-scp          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per l'esecuzione del modulo                          *
      *    *-----------------------------------------------------------*
       01  w-aux-ffo-ffr.
      *        *-------------------------------------------------------*
      *        * Flag di apertura modulo per determinazione valore di  *
      *        * magazzino eseguita                                    *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-fvm          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di inizializzazione lettura sequenziale [fft]    *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-fgs          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di 'Tipo prossima lettura da eseguire'           *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-nxt          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records scritti in file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-csc          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Contatore records riletti da file relative            *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-crl          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo voce di magazzino                                *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-tmg          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice prodotto                                       *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-pro          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-npr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo calcolo voce 'Costo del Fatturato'               *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-ccf          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p1i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p1f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p2i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p2f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data min                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p3i          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo, data max                                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-p3f          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 1. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-x1p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 2. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-x2p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No record letto riferito a 3. periodo              *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-x3p          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Quantita' fatturata'  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-qta          pic s9(13)v9(03)            .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Fatturato'            *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-val          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Costo del Fatturato'  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-cos          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Valore determinato per la voce 'Incidenza in positivo *
      *        * o in negativo'                                        *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-pon          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto in chiusura in valuta base             *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-scc          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto pagamento in valuta base               *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-scp          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto totale in valuta base                  *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-sct          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Importo Sconto riga in valuta base                    *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-scr          pic s9(18)                  .
      *        *-------------------------------------------------------*
      *        * Importo totale in valuta base righe da includere nel- *
      *        * la statistica                                         *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-itr          pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita da selezione su record [ffr]           *
      *        *-------------------------------------------------------*
           05  w-aux-ffo-ffr-sel          pic  x(01)                  .

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
      *    * Link-area per modulo dell'area 'saf'           'mffoffr0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/saf/prg/cpy/mffoffr0.mdl"                   .

      *    *===========================================================*
      *    * Record file [fft]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rffft"                          .

      *    *===========================================================*
      *    * Record file [ffr]                                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .

      ******************************************************************
       Procedure Division                using w-mod-ffo-ffr
                                               rf-fft
                                               rf-ffr                 .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Richiamo routine in funzione del tipo di opera- *
      *              * zione passato                                   *
      *              *-------------------------------------------------*
           if        w-mod-ffo-ffr-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mod-ffo-ffr-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mod-ffo-ffr-ope    =    "GS"
                     perform   gts-000    thru gts-999
           else if   w-mod-ffo-ffr-ope    =    "GT"
                     perform   get-000    thru get-999
           else if   w-mod-ffo-ffr-ope    =    "CS"
                     perform   cds-000    thru cds-999
           else if   w-mod-ffo-ffr-ope    =    "CG"
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
           move      "0"                  to   w-aux-ffo-ffr-fvm      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * non ancora eseguita Start                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-ffo-ffr-fgs      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di 'Tipo prossima lettura *
      *              * da eseguire' a : 'Indeterminato'                *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-ffo-ffr-nxt      .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione sul tipo fun-   *
      *              * zionamento statistiche sul fatturato su righe   *
      *              * documento                                       *
      *              *-------------------------------------------------*
           perform   prs-ffo-ffr-000      thru prs-ffo-ffr-999        .
      *              *-------------------------------------------------*
      *              * Open file relative di appoggio [rel]            *
      *              *-------------------------------------------------*
           perform   fil-rel-opn-000      thru fil-rel-opn-999        .
      *              *-------------------------------------------------*
      *              * Open file [dcf]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Open file [dcp]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
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
      *              * Close file [dcf]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *              *-------------------------------------------------*
      *              * Close file [dcp]                                *
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
           if        w-aux-ffo-ffr-fvm    =    "2"
                     go to opn-vun-mag-999.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione valore di magaz-  *
      *              * zino                                            *
      *              *-------------------------------------------------*
           perform   det-vun-mag-opn-000  thru det-vun-mag-opn-999    .
      *              *-------------------------------------------------*
      *              * Flag di apertura modulo a : in essere           *
      *              *-------------------------------------------------*
           move      "2"                  to   w-aux-ffo-ffr-fvm      .
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
           if        w-aux-ffo-ffr-fvm    not  = "2"
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
           move      "1"                  to   w-aux-ffo-ffr-fvm      .
       cls-vun-mag-100.
      *              *-------------------------------------------------*
      *              * Se modulo non chiuso : nessuna azione ed uscita *
      *              * immediata                                       *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-fvm    not  = "1"
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
           move      "0"                  to   w-aux-ffo-ffr-fvm      .
       cls-vun-mag-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione su statistiche sul fatturato    *
      *    * per tipo funzionamento statistiche su righe documento     *
      *    *-----------------------------------------------------------*
       prs-ffo-ffr-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/saf/ffoffr[tip-fun]"
                                delimited by   size
                     w-mod-ffo-ffr-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-ffo-ffr-150.
       prs-ffo-ffr-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-ffo-ffr-500.
       prs-ffo-ffr-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-ffo-ffr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-ffo-ffr-700.
       prs-ffo-ffr-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/saf/ffoffr[tip-fun]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-ffo-ffr-650.
       prs-ffo-ffr-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-ffo-ffr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-ffo-ffr-700.
       prs-ffo-ffr-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-ffo-ffr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-ffo-ffr-700.
       prs-ffo-ffr-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Fatture                'S' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-fat    not  = "N"
                     move  "S"            to   w-prs-ffo-ffr-fat      .
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Note di addebito       'S' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-ndb    not  = "N"
                     move  "S"            to   w-prs-ffo-ffr-ndb      .
      *                  *---------------------------------------------*
      *                  * Si/No inclusione Note di accredito      'S' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-ncr    not  = "N"
                     move  "S"            to   w-prs-ffo-ffr-ncr      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Merce                       'S' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rtm    not  = "N"
                     move  "S"            to   w-prs-ffo-ffr-rtm      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Servizi                     'S' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rts    not  = "N"
                     move  "S"            to   w-prs-ffo-ffr-rts      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Imballi                     'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rti    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rti      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 4               'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rt4    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rt4      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 5               'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rt5    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rt5      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 6               'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rt6    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rt6      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 7               'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rt7    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rt7      .
      *                  *---------------------------------------------*
      *                  * Si/No righe tipo Libero 8               'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rt8    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rt8      .
      *                  *---------------------------------------------*
      *                  * Si/No righe Extra Attivita'             'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-rtx    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-rtx      .
      *                  *---------------------------------------------*
      *                  * Si/No detrazione Sconto in chiusura     'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-scc    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-scc      .
      *                  *---------------------------------------------*
      *                  * Si/No detrazione Sconto pagamento       'N' *
      *                  *---------------------------------------------*
           if        w-prs-ffo-ffr-scp    not  = "S"
                     move  "N"            to   w-prs-ffo-ffr-scp      .
       prs-ffo-ffr-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-ffo-ffr-999.
       prs-ffo-ffr-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [fft]       *
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
           move      "#"                  to   w-aux-ffo-ffr-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-ffo-ffr-nxt      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-npr    to   w-aux-ffo-ffr-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-ccf    to   w-aux-ffo-ffr-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p1i    to   w-aux-ffo-ffr-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p1f    to   w-aux-ffo-ffr-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p2i    to   w-aux-ffo-ffr-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p2f    to   w-aux-ffo-ffr-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p3i    to   w-aux-ffo-ffr-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p3f    to   w-aux-ffo-ffr-p3f      .
       gts-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale archivio [fft]                        *
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
      *              * archivio [fft] ancora in Off : uscita come per  *
      *              * fine file                                       *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-fgs    =    spaces
                     go to get-900.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * al valore 'Indeterminato' : uscita come per fi- *
      *              * ne file                                         *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-nxt    =    zero
                     go to get-900.
       get-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-nxt    =    1
                     go to get-200
           else if   w-aux-ffo-ffr-nxt    =    2
                     go to get-225
           else if   w-aux-ffo-ffr-nxt    =    3
                     go to get-250
           else if   w-aux-ffo-ffr-nxt    =    4
                     go to get-300
           else if   w-aux-ffo-ffr-nxt    =    5
                     go to get-600
           else if   w-aux-ffo-ffr-nxt    =    6
                     go to get-700
           else      go to get-900.
       get-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [fft] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOCDDO "         to   f-key                  .
           move      w-aux-ffo-ffr-p1i    to   rf-fft-dat-doc         .
           move      zero                 to   rf-fft-cod-dpz         .
           move      spaces               to   rf-fft-num-doc         .
           move      spaces               to   rf-fft-cod-tmf         .
           move      zero                 to   rf-fft-num-prt         .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
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
      *                  * Start su archivio [fft] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOCDDO "         to   f-key                  .
           move      w-aux-ffo-ffr-p2i    to   rf-fft-dat-doc         .
           move      zero                 to   rf-fft-cod-dpz         .
           move      spaces               to   rf-fft-num-doc         .
           move      spaces               to   rf-fft-cod-tmf         .
           move      zero                 to   rf-fft-num-prt         .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
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
      *                  * Start su archivio [fft] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOCDDO "         to   f-key                  .
           move      w-aux-ffo-ffr-p3i    to   rf-fft-dat-doc         .
           move      zero                 to   rf-fft-cod-dpz         .
           move      spaces               to   rf-fft-num-doc         .
           move      spaces               to   rf-fft-cod-tmf         .
           move      zero                 to   rf-fft-num-prt         .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [fft]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [fft]                          *
      *              *-------------------------------------------------*
       get-325.
      *                  *---------------------------------------------*
      *                  * Read Next su [fft]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-900.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-fft letta    *
      *                  *---------------------------------------------*
           perform   man-pre-fft-000      thru man-pre-fft-999        .
       get-350.
      *                  *---------------------------------------------*
      *                  * Test Max su [fft], a seconda del numero di  *
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
           move      "N"                  to   w-aux-ffo-ffr-x1p      .
           move      "N"                  to   w-aux-ffo-ffr-x2p      .
           move      "N"                  to   w-aux-ffo-ffr-x3p      .
       get-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-ffo-ffr-npr    =    01
                     go to get-365
           else if   w-aux-ffo-ffr-npr    =    02
                     go to get-370
           else if   w-aux-ffo-ffr-npr    =    03
                     go to get-375.
       get-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       >    w-aux-ffo-ffr-p1f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S"
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
           if        rf-fft-dat-doc       >    w-aux-ffo-ffr-p2f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p2i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p2f
                     move  "S"            to   w-aux-ffo-ffr-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S" or
                     w-aux-ffo-ffr-x2p    =    "S"
                     go to get-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       <    w-aux-ffo-ffr-p2i
                     move  2              to   w-aux-ffo-ffr-nxt
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
           if        rf-fft-dat-doc       >    w-aux-ffo-ffr-p3f
                     go to get-900.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p2i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p2f
                     move  "S"            to   w-aux-ffo-ffr-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       not  < w-aux-ffo-ffr-p3i and
                     rf-fft-dat-doc       not  > w-aux-ffo-ffr-p3f
                     move  "S"            to   w-aux-ffo-ffr-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S" or
                     w-aux-ffo-ffr-x2p    =    "S" or
                     w-aux-ffo-ffr-x3p    =    "S"
                     go to get-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       <    w-aux-ffo-ffr-p2i
                     move  2              to   w-aux-ffo-ffr-nxt
                     go to get-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-fft-dat-doc       <    w-aux-ffo-ffr-p3i
                     move  3              to   w-aux-ffo-ffr-nxt
                     go to get-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a fine file            *
      *                          *-------------------------------------*
           go to     get-900.
       get-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : flag di 'Tipo prossima let-  *
      *                  * tura da eseguire' a 'Read next su [fft] e   *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra i seguenti :           *
      *                      *  - 01 : Fattura                         *
      *                      *  - 02 : Nota di addebito                *
      *                      *  - 03 : Nota di accredito               *
      *                      * la selezione non e' superata            *
      *                      *-----------------------------------------*
           if        rf-fft-tip-doc       not  = 01 and
                     rf-fft-tip-doc       not  = 02 and
                     rf-fft-tip-doc       not  = 03
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra quelli da includere    *
      *                      * nelle statistiche secondo quanto e'     *
      *                      * stato specificato con le personalizza-  *
      *                      * zioni : la selezione non e' superata    *
      *                      *-----------------------------------------*
           if        rf-fft-tip-doc       =    01    and
                     w-prs-ffo-ffr-fat    not  = "S"
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
           if        rf-fft-tip-doc       =    02    and
                     w-prs-ffo-ffr-ndb    not  = "S"
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
           if        rf-fft-tip-doc       =    03    and
                     w-prs-ffo-ffr-ncr    not  = "S"
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
       get-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-fft-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-fft-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-fft-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-fft-cdc-vpf       to   w-cvs-vlt-cdc          .
       get-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-fft-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-ffo-ffr-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-fft-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-ffo-ffr-scp      .
       get-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-ffo-ffr-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-ffo-ffr-scc    =    "S"
                     add   w-aux-ffo-ffr-scc
                                          to   w-aux-ffo-ffr-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-ffo-ffr-scp    =    "S"
                     add   w-aux-ffo-ffr-scp
                                          to   w-aux-ffo-ffr-sct      .
       get-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si pone il flag di 'Tipo prossima lettura   *
      *                  * da eseguire' a 'Start su [ffr]' e si rici-  *
      *                  * cla                                         *
      *                  *---------------------------------------------*
           if        w-aux-ffo-ffr-sct    =    zero
                     move  5              to   w-aux-ffo-ffr-nxt
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
           move      zero                 to   w-aux-ffo-ffr-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [ffr]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fft-num-prt       to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-575.
       get-535.
      *                      *-----------------------------------------*
      *                      * Read next su [ffr]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to get-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-ffr     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-ffr-000      thru man-pre-ffr-999        .
       get-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-ffr-num-prt       not  = rf-fft-num-prt
                     go to get-575.
       get-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [ffr], se non superata : a *
      *                      * riciclo su read next su [ffr]           *
      *                      *-----------------------------------------*
           perform   sel-rec-ffr-000      thru sel-rec-ffr-999        .
           if        w-aux-ffo-ffr-sel    not  = spaces
                     go to get-535.
       get-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-ffo-ffr-val    to   w-aux-ffo-ffr-itr      .
       get-555.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura riga successiva       *
      *                      *-----------------------------------------*
           go to     get-535.
       get-575.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a 'Start su [ffr]'                          *
      *                  *---------------------------------------------*
           move      5                    to   w-aux-ffo-ffr-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     get-100.
       get-600.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start su [ffr]                              *
      *              *-------------------------------------------------*
       get-625.
      *                  *---------------------------------------------*
      *                  * Start su [ffr]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fft-num-prt       to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : flag di 'Tipo prossima    *
      *                  * lettura da eseguire' a 'Read next su [fft]' *
      *                  * e riciclo                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
      *                  *---------------------------------------------*
      *                  * Altrimenti : flag di 'Tipo prossima lettura *
      *                  * da eseguire' a 'Read next su [ffr]' e rici- *
      *                  * clo                                         *
      *                  *---------------------------------------------*
           move      6                    to   w-aux-ffo-ffr-nxt      .
           go to     get-100.
       get-700.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [ffr]                          *
      *              *-------------------------------------------------*
       get-725.
      *                  *---------------------------------------------*
      *                  * Read next su [ffr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se At end : flag di 'Tipo prossima lettura  *
      *                  * da eseguire' a 'Read next su [fft]' e ri-   *
      *                  * ciclo                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-ffr letta       *
      *                  *---------------------------------------------*
           perform   man-pre-ffr-000      thru man-pre-ffr-999        .
       get-750.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato : flag di     *
      *                      * 'Tipo prossima lettura da eseguire' a   *
      *                      * 'Read next su [fft]' e riciclo          *
      *                      *-----------------------------------------*
           if        rf-ffr-num-prt       not  = rf-fft-num-prt
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to get-100.
       get-775.
      *                      *-----------------------------------------*
      *                      * Selezione su [ffr], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [ffr]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-ffr-000      thru sel-rec-ffr-999        .
           if        w-aux-ffo-ffr-sel    not  = spaces
                     move  6              to   w-aux-ffo-ffr-nxt
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
           if        w-aux-ffo-ffr-sct    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-val    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-itr    =    zero
                     go to get-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-ffo-ffr-sct    to   w-aux-ffo-ffr-scr      .
           multiply  w-aux-ffo-ffr-val    by   w-aux-ffo-ffr-scr      .
           divide    w-aux-ffo-ffr-itr    into w-aux-ffo-ffr-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-ffo-ffr-scr    from w-aux-ffo-ffr-val      .
       get-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-ffo-ffr-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x1p    to   w-mod-ffo-ffr-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x2p    to   w-mod-ffo-ffr-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x3p    to   w-mod-ffo-ffr-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-qta    to   w-mod-ffo-ffr-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-val    to   w-mod-ffo-ffr-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-ffo-ffr-000  thru det-cos-ffo-ffr-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-pon    to   w-mod-ffo-ffr-pon      .
       get-850.
      *                      *-----------------------------------------*
      *                      * Flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [ffr]'           *
      *                      *-----------------------------------------*
           move      6                    to   w-aux-ffo-ffr-nxt      .
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
           move      "#"                  to   w-mod-ffo-ffr-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-ffo-ffr-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     get-999.
       get-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [ffr] per   *
      *    * un solo codice prodotto                                   *
      *    *-----------------------------------------------------------*
       cds-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag lettura sequenziale a :   *
      *              * Start eseguita                                  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-aux-ffo-ffr-fgs      .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a : *
      *              * Start su 1. periodo                             *
      *              *-------------------------------------------------*
           move      1                    to   w-aux-ffo-ffr-nxt      .
      *              *-------------------------------------------------*
      *              * Inizializzazione nuovo ciclo di scrittura/let-  *
      *              * tura file relative di appoggio                  *
      *              *-------------------------------------------------*
           perform   fil-rel-new-000      thru fil-rel-new-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatori per scrittura e ri-  *
      *              * lettura file relative                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-aux-ffo-ffr-csc      .
           move      zero                 to   w-aux-ffo-ffr-crl      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo voce di magazzino           *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-tmg    to   w-aux-ffo-ffr-tmg      .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice prodotto                  *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-pro    to   w-aux-ffo-ffr-pro      .
      *              *-------------------------------------------------*
      *              * Memorizzazione numero periodi di riferimento    *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-npr    to   w-aux-ffo-ffr-npr      .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo calcolo voce 'Costo del     *
      *              * Fatturato'                                      *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-ccf    to   w-aux-ffo-ffr-ccf      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p1i    to   w-aux-ffo-ffr-p1i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 1. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p1f    to   w-aux-ffo-ffr-p1f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p2i    to   w-aux-ffo-ffr-p2i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 2. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p2f    to   w-aux-ffo-ffr-p2f      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data min              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p3i    to   w-aux-ffo-ffr-p3i      .
      *              *-------------------------------------------------*
      *              * Memorizzazione 3. periodo data max              *
      *              *-------------------------------------------------*
           move      w-mod-ffo-ffr-p3f    to   w-aux-ffo-ffr-p3f      .
       cds-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del flag di 'Tipo prossima *
      *              * lettura da eseguire'                            *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-nxt    =    1
                     go to cds-200
           else if   w-aux-ffo-ffr-nxt    =    2
                     go to cds-225
           else if   w-aux-ffo-ffr-nxt    =    3
                     go to cds-250
           else if   w-aux-ffo-ffr-nxt    =    4
                     go to cds-300
           else      go to cds-900.
       cds-200.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Start sul 1. periodo                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su archivio [ffr] per il 1. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-ffr-cod-dpz         .
           move      w-aux-ffo-ffr-tmg    to   rf-ffr-tip-mag         .
           move      w-aux-ffo-ffr-pro    to   rf-ffr-num-mag         .
           move      w-aux-ffo-ffr-p1i    to   rf-ffr-dat-reg         .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [ffr]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
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
      *                  * Start su archivio [ffr] per il 2. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-ffr-cod-dpz         .
           move      w-aux-ffo-ffr-tmg    to   rf-ffr-tip-mag         .
           move      w-aux-ffo-ffr-pro    to   rf-ffr-num-mag         .
           move      w-aux-ffo-ffr-p2i    to   rf-ffr-dat-reg         .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [ffr]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
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
      *                  * Start su archivio [ffr] per il 3. periodo   *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "MAGDAT    "         to   f-key                  .
           move      01                   to   rf-ffr-cod-dpz         .
           move      w-aux-ffo-ffr-tmg    to   rf-ffr-tip-mag         .
           move      w-aux-ffo-ffr-pro    to   rf-ffr-num-mag         .
           move      w-aux-ffo-ffr-p3i    to   rf-ffr-dat-reg         .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita per fine file      *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Flag di 'Tipo prossima lettura da eseguire' *
      *                  * a : Read next su [ffr]                      *
      *                  *---------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     cds-100.
       cds-300.
      *              *-------------------------------------------------*
      *              * Se flag di 'Tipo prossima lettura da eseguire'  *
      *              * a : Read next su [ffr]                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next su [ffr]                          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se At End : uscita per fine file            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cds-900.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-ffr letta       *
      *                  *---------------------------------------------*
           perform   man-pre-ffr-000      thru man-pre-ffr-999        .
       cds-330.
      *                  *---------------------------------------------*
      *                  * Test Max su [ffr] per il tipo e il codice   *
      *                  * prodotto : se non superato, uscita per      *
      *                  * fine file                                   *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-mag       not  = w-aux-ffo-ffr-tmg or
                     rf-ffr-num-mag       not  = w-aux-ffo-ffr-pro
                     go to cds-900.
       cds-350.
      *                  *---------------------------------------------*
      *                  * Selezioni su [ffr], a seconda del numero di *
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
           move      "N"                  to   w-aux-ffo-ffr-x1p      .
           move      "N"                  to   w-aux-ffo-ffr-x2p      .
           move      "N"                  to   w-aux-ffo-ffr-x3p      .
       cds-360.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero peri- *
      *                      * odi di riferimento                      *
      *                      *-----------------------------------------*
           if        w-aux-ffo-ffr-npr    =    01
                     go to cds-365
           else if   w-aux-ffo-ffr-npr    =    02
                     go to cds-370
           else if   w-aux-ffo-ffr-npr    =    03
                     go to cds-375.
       cds-365.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 01   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       >    w-aux-ffo-ffr-p1f
                     go to cds-300.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Altrimenti : a riciclo              *
      *                          *-------------------------------------*
           go to     cds-300.
       cds-370.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 02   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       >    w-aux-ffo-ffr-p2f
                     go to cds-300.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p2i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p2f
                     move  "S"            to   w-aux-ffo-ffr-x2p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S" or
                     w-aux-ffo-ffr-x2p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       <    w-aux-ffo-ffr-p2i
                     move  2              to   w-aux-ffo-ffr-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a riciclo              *
      *                          *-------------------------------------*
           go to     cds-300.
       cds-375.
      *                      *-----------------------------------------*
      *                      * Se numero periodi di riferimento : 03   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test max                            *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       >    w-aux-ffo-ffr-p3f
                     go to cds-300.
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 1. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p1i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p1f
                     move  "S"            to   w-aux-ffo-ffr-x1p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 2. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p2i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p2f
                     move  "S"            to   w-aux-ffo-ffr-x2p      .
      *                          *-------------------------------------*
      *                          * Test di appartenenza al 3. periodo  *
      *                          * di riferimento                      *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       not  < w-aux-ffo-ffr-p3i and
                     rf-ffr-dat-doc       not  > w-aux-ffo-ffr-p3f
                     move  "S"            to   w-aux-ffo-ffr-x3p      .
      *                          *-------------------------------------*
      *                          * Se il record letto appartiene al 1. *
      *                          * periodo di riferimento oppure al 2. *
      *                          * periodo di riferimento oppure al 3. *
      *                          * periodo di riferimento : continua-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-x1p    =    "S" or
                     w-aux-ffo-ffr-x2p    =    "S" or
                     w-aux-ffo-ffr-x3p    =    "S"
                     go to cds-400.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 2.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 2. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       <    w-aux-ffo-ffr-p2i
                     move  2              to   w-aux-ffo-ffr-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Se la data del documento in esame   *
      *                          * e' minore della data minima del 3.  *
      *                          * periodo di riferimento si pone il   *
      *                          * flag di 'Tipo prossima lettura da   *
      *                          * eseguire' a 'Start sul 3. periodo'  *
      *                          * e si ricicla                        *
      *                          *-------------------------------------*
           if        rf-ffr-dat-doc       <    w-aux-ffo-ffr-p3i
                     move  3              to   w-aux-ffo-ffr-nxt
                     go to cds-100.
      *                          *-------------------------------------*
      *                          * Altrimenti : a riciclo              *
      *                          *-------------------------------------*
           go to     cds-300.
       cds-400.
      *                      *-----------------------------------------*
      *                      * Selezione su [ffr], se non superata :   *
      *                      * flag di 'Tipo prossima lettura da ese-  *
      *                      * guire' a 'Read next su [ffr]' e riciclo *
      *                      *-----------------------------------------*
           perform   sel-rec-ffr-000      thru sel-rec-ffr-999        .
           if        w-aux-ffo-ffr-sel    not  = spaces
                     move  4              to   w-aux-ffo-ffr-nxt
                     go to cds-100.
       cds-500.
      *              *-------------------------------------------------*
      *              * Scrittura file relative di appoggio [rel]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records scritti in fi- *
      *                  * le di appoggio [rel]                        *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-ffo-ffr-csc      .
           move      w-aux-ffo-ffr-csc    to   f-rel-put              .
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
           move      rf-ffr-num-prt       to   rel-num-prt            .
      *                      *-----------------------------------------*
      *                      * Numero progressivo protocollo fattura   *
      *                      *-----------------------------------------*
           move      rf-ffr-num-prg       to   rel-num-prg            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x1p    to   rel-snx-x1p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x2p    to   rel-snx-x2p            .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-x3p    to   rel-snx-x3p            .
      *                  *---------------------------------------------*
      *                  * Esecuzione funzione Put su file relative    *
      *                  * di appoggio [rel]                           *
      *                  *---------------------------------------------*
           perform   fil-rel-put-000      thru fil-rel-put-999        .
      *              *-------------------------------------------------*
      *              * Flag di 'Tipo prossima lettura da eseguire' a   *
      *              * 'Read next su [ffr]'                            *
      *              *-------------------------------------------------*
           move      4                    to   w-aux-ffo-ffr-nxt      .
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
      *              * Lettura sequenziale file di appoggio relative   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore records file relative  *
      *                  *---------------------------------------------*
           add       1                    to   w-aux-ffo-ffr-crl      .
      *                  *---------------------------------------------*
      *                  * Test se oltre il numero records scritti     *
      *                  *---------------------------------------------*
           if        w-aux-ffo-ffr-crl    >    w-aux-ffo-ffr-csc
                     go to cdg-900.
      *                  *---------------------------------------------*
      *                  * Preparazione numero record per rilettura    *
      *                  *---------------------------------------------*
           move      w-aux-ffo-ffr-crl    to   f-rel-get              .
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
      *                  * Normalizzazione record [fft]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [fft]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-fft-num-prt         .
           move      "pgm/ffo/fls/ioc/obj/ioffft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fft                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla testata documento rf-fft letta    *
      *                  *---------------------------------------------*
           perform   man-pre-fft-000      thru man-pre-fft-999        .
       cdg-400.
      *                  *---------------------------------------------*
      *                  * Selezione preliminare su tipo documento, se *
      *                  * non superata : riciclo su lettura file re-  *
      *                  * lative                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra i seguenti :           *
      *                      *  - 01 : Fattura                         *
      *                      *  - 02 : Nota di addebito                *
      *                      *  - 03 : Nota di accredito               *
      *                      * la selezione non e' superata            *
      *                      *-----------------------------------------*
           if        rf-fft-tip-doc       not  = 01 and
                     rf-fft-tip-doc       not  = 02 and
                     rf-fft-tip-doc       not  = 03
                     go to cdg-000.
      *                      *-----------------------------------------*
      *                      * Se il tipo documento associato al docu- *
      *                      * mento non e' tra quelli da includere    *
      *                      * nelle statistiche secondo quanto e'     *
      *                      * stato specificato con le personalizza-  *
      *                      * zioni : la selezione non e' superata    *
      *                      *-----------------------------------------*
           if        rf-fft-tip-doc       =    01    and
                     w-prs-ffo-ffr-fat    not  = "S"
                     go to cdg-000.
           if        rf-fft-tip-doc       =    02    and
                     w-prs-ffo-ffr-ndb    not  = "S"
                     go to cdg-000.
           if        rf-fft-tip-doc       =    03    and
                     w-prs-ffo-ffr-ncr    not  = "S"
                     go to cdg-000.
       cdg-425.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per sottoposizione   *
      *                  * dei valori al cambio valuta                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per il documento           *
      *                      *-----------------------------------------*
           move      rf-fft-sgl-vpf       to   w-cvs-vlt-sgl          .
      *                      *-----------------------------------------*
      *                      * Numero decimali per la valuta           *
      *                      *-----------------------------------------*
           move      rf-fft-dec-vpf       to   w-cvs-vlt-dec          .
      *                      *-----------------------------------------*
      *                      * Tipo di cambio : '/' o '*'              *
      *                      *-----------------------------------------*
           move      rf-fft-tdc-vpf       to   w-cvs-vlt-tdc          .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio per la valuta    *
      *                      * alla data del documento                 *
      *                      *-----------------------------------------*
           move      rf-fft-cdc-vpf       to   w-cvs-vlt-cdc          .
       cdg-450.
      *                  *---------------------------------------------*
      *                  * Sottoposizione al cambio valuta dei valori  *
      *                  * di testata documento che possono essere at- *
      *                  * tinenti le righe documento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           move      rf-fft-tot-scc       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-ffo-ffr-scc      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           move      rf-fft-tot-scp       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-ffo-ffr-scp      .
       cdg-475.
      *                  *---------------------------------------------*
      *                  * Determinazione importo sconto totale che    *
      *                  * deve abbattere gli importi in riga          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Azzeramento preliminare                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-aux-ffo-ffr-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto in chiusura              *
      *                      *-----------------------------------------*
           if        w-prs-ffo-ffr-scc    =    "S"
                     add   w-aux-ffo-ffr-scc
                                          to   w-aux-ffo-ffr-sct      .
      *                      *-----------------------------------------*
      *                      * Importo sconto pagamento                *
      *                      *-----------------------------------------*
           if        w-prs-ffo-ffr-scp    =    "S"
                     add   w-aux-ffo-ffr-scp
                                          to   w-aux-ffo-ffr-sct      .
       cdg-500.
      *                  *---------------------------------------------*
      *                  * Se l'importo sconto totale che deve abbat-  *
      *                  * tere gli importi in riga e' pari a zero :   *
      *                  * si va a trattamento riga fattura            *
      *                  *---------------------------------------------*
           if        w-aux-ffo-ffr-sct    =    zero
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
           move      zero                 to   w-aux-ffo-ffr-itr      .
      *                      *-----------------------------------------*
      *                      * Start su [ffr]                          *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-fft-num-prt       to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : fine determinazione   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
       cdg-535.
      *                      *-----------------------------------------*
      *                      * Read next su [ffr]                      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                      *-----------------------------------------*
      *                      * Se At end : fine determinazione         *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-575.
      *                      *-----------------------------------------*
      *                      * Manipolazioni preliminari sul record    *
      *                      * relativo alla riga documento rf-ffr     *
      *                      * letta                                   *
      *                      *-----------------------------------------*
           perform   man-pre-ffr-000      thru man-pre-ffr-999        .
       cdg-540.
      *                      *-----------------------------------------*
      *                      * Test max, se non superato a fine deter- *
      *                      * minazione                               *
      *                      *-----------------------------------------*
           if        rf-ffr-num-prt       not  = rf-fft-num-prt
                     go to cdg-575.
       cdg-545.
      *                      *-----------------------------------------*
      *                      * Selezione su [ffr], se non superata : a *
      *                      * riciclo su read next su [ffr]           *
      *                      *-----------------------------------------*
           perform   sel-rec-ffr-000      thru sel-rec-ffr-999        .
           if        w-aux-ffo-ffr-sel    not  = spaces
                     go to cdg-535.
       cdg-550.
      *                      *-----------------------------------------*
      *                      * Incremento importo totale con il valore *
      *                      * della riga documento da includere nella *
      *                      * statistica                              *
      *                      *-----------------------------------------*
           add       w-aux-ffo-ffr-val    to   w-aux-ffo-ffr-itr      .
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
      *                  * Lettura record [ffr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rel-num-prt          to   rf-ffr-num-prt         .
           move      rel-num-prg          to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo su lettura file *
      *                  * relative                                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to cdg-000.
      *                  *---------------------------------------------*
      *                  * Manipolazioni preliminari sul record rela-  *
      *                  * tivo alla riga documento rf-ffr letta       *
      *                  *---------------------------------------------*
           perform   man-pre-ffr-000      thru man-pre-ffr-999        .
       cdg-700.
      *                      *-----------------------------------------*
      *                      * Selezione su [ffr], se non superata :   *
      *                      * riciclo su lettura file relative        *
      *                      *-----------------------------------------*
           perform   sel-rec-ffr-000      thru sel-rec-ffr-999        .
           if        w-aux-ffo-ffr-sel    not  = spaces
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
           if        w-aux-ffo-ffr-sct    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo voce 'Fatturato' pari a  *
      *                          * zero : nessuna correzione           *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-val    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Se importo totale in valuta base    *
      *                          * righe da includere nella statistica *
      *                          * pari a zero : nessuna correzione    *
      *                          *-------------------------------------*
           if        w-aux-ffo-ffr-itr    =    zero
                     go to cdg-825.
      *                          *-------------------------------------*
      *                          * Calcolo importo sconto riga         *
      *                          *-------------------------------------*
           move      w-aux-ffo-ffr-sct    to   w-aux-ffo-ffr-scr      .
           multiply  w-aux-ffo-ffr-val    by   w-aux-ffo-ffr-scr      .
           divide    w-aux-ffo-ffr-itr    into w-aux-ffo-ffr-scr      .
      *                          *-------------------------------------*
      *                          * Diminuzione voce 'Fatturato' per lo *
      *                          * sconto riga calcolato               *
      *                          *-------------------------------------*
           subtract  w-aux-ffo-ffr-scr    from w-aux-ffo-ffr-val      .
       cdg-825.
      *                      *-----------------------------------------*
      *                      * Flag di uscita : Ok                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-ffo-ffr-flg      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 1. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x1p          to   w-mod-ffo-ffr-sn1      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 2. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x2p          to   w-mod-ffo-ffr-sn2      .
      *                      *-----------------------------------------*
      *                      * Si/No record riferito al 3. periodo     *
      *                      *-----------------------------------------*
           move      rel-snx-x3p          to   w-mod-ffo-ffr-sn3      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Quantita' Fatturata' *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-qta    to   w-mod-ffo-ffr-qta      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Fatturato'           *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-val    to   w-mod-ffo-ffr-val      .
      *                      *-----------------------------------------*
      *                      * Valore della voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           perform   det-cos-ffo-ffr-000  thru det-cos-ffo-ffr-999    .
      *                      *-----------------------------------------*
      *                      * Valore della voce con incidenza Positi- *
      *                      * va o Negativa                           *
      *                      *-----------------------------------------*
           move      w-aux-ffo-ffr-pon    to   w-mod-ffo-ffr-pon      .
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
           move      "#"                  to   w-mod-ffo-ffr-flg      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 1. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn1      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 2. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn2      .
      *                  *---------------------------------------------*
      *                  * Record riferito al 3. periodo : No          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-mod-ffo-ffr-sn3      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Quantita' Fatturata'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-qta      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Fatturato'               *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-val      .
      *                  *---------------------------------------------*
      *                  * Valore della voce 'Costo del Fatturato'     *
      *                  *---------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-cos      .
      *                  *---------------------------------------------*
      *                  * Valore Positivo o Negativo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-ffo-ffr-pon      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cdg-999.
       cdg-999.
           exit.

      *    *===========================================================*
      *    * Selezione e determinazioni su record [ffr]                *
      *    *-----------------------------------------------------------*
       sel-rec-ffr-000.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari generali                  *
      *              *-------------------------------------------------*
       sel-rec-ffr-005.
      *                  *---------------------------------------------*
      *                  * Selezione su Flag blocco di documento       *
      *                  *---------------------------------------------*
           if        rf-ffr-bld-flb       not  = zero
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-010.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo blocco documento          *
      *                  *---------------------------------------------*
           if        rf-ffr-bld-tpb       not  = zero
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-015.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga nel blocco di docu-  *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           if        rf-ffr-bld-rgb       not  = zero
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-020.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo riga                      *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-rig       not  = "P    " and
                     rf-ffr-tip-rig       not  = "M    " and
                     rf-ffr-tip-rig       not  = "V    "
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-030.
      *                  *---------------------------------------------*
      *                  * Selezione su Codice prodotto numerico       *
      *                  *---------------------------------------------*
           if        rf-ffr-num-mag       =    zero
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-035.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-pro       <    01 or
                     rf-ffr-tip-pro       >    09
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-040.
      *                  *---------------------------------------------*
      *                  * Selezione su Quantita' e Valore             *
      *                  *---------------------------------------------*
           if        rf-ffr-qta-acq       =    zero and
                     rf-ffr-imp-rig       =    zero
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-045.
      *                  *---------------------------------------------*
      *                  * Selezione su righe di omaggio               *
      *                  *---------------------------------------------*
           move      rf-ffr-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    9
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-100.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari su personalizzazioni      *
      *              *-------------------------------------------------*
       sel-rec-ffr-105.
      *                  *---------------------------------------------*
      *                  * Selezione su Tipo prodotto                  *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-pro       =    01
                     if    w-prs-ffo-ffr-rtm
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    02
                     if    w-prs-ffo-ffr-rts
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    03
                     if    w-prs-ffo-ffr-rti
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    04
                     if    w-prs-ffo-ffr-rt4
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    05
                     if    w-prs-ffo-ffr-rt5
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    06
                     if    w-prs-ffo-ffr-rt6
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    07
                     if    w-prs-ffo-ffr-rt7
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    08
                     if    w-prs-ffo-ffr-rt8
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
           if        rf-ffr-tip-pro       =    09
                     if    w-prs-ffo-ffr-rtx
                                          not  = "S"
                     move  "#"            to   w-aux-ffo-ffr-sel
                     go to sel-rec-ffr-999.
       sel-rec-ffr-200.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Quantita' fatturata'         *
      *              *-------------------------------------------------*
           move      rf-ffr-qta-acq       to   w-aux-ffo-ffr-qta      .
       sel-rec-ffr-300.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Fatturato', con conversione  *
      *              * in valuta base                                  *
      *              *-------------------------------------------------*
           move      rf-ffr-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
           move      w-cvs-vlt-avb        to   w-aux-ffo-ffr-val      .
       sel-rec-ffr-400.
      *              *-------------------------------------------------*
      *              * Preparazione voce 'Incidenza in positivo o in   *
      *              * negativo'                                       *
      *              *-------------------------------------------------*
           if        rf-fft-tip-doc       =    01 or
                     rf-fft-tip-doc       =    02
                     move  "P"            to   w-aux-ffo-ffr-pon
           else      move  "N"            to   w-aux-ffo-ffr-pon      .
       sel-rec-ffr-900.
      *              *-------------------------------------------------*
      *              * Flag di selezione superata                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-aux-ffo-ffr-sel      .
       sel-rec-ffr-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla testa- *
      *    * ta documento rf-fft letta                                 *
      *    *-----------------------------------------------------------*
       man-pre-fft-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice fornitore e   *
      *              * della sua dipendenza in funzione dello status   *
      *              * espresso in anagrafica commerciale              *
      *              *-------------------------------------------------*
       man-pre-fft-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcf]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Lettura record principale dell'anagrafica   *
      *                  * commerciale del fornitore                   *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      rf-fft-cod-arc       to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente : nessuna manipola- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to man-pre-fft-900.
       man-pre-fft-200.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del fornitore non  *
      *                  * indica che e' stato sostituito da una altro *
      *                  * fornitore : nessuna manipolazione           *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tus       not  = 21 and
                     rf-dcf-sta-tus       not  = 52 and
                     rf-dcf-sta-tus       not  = 62 and
                     rf-dcf-sta-tus       not  = 72
                     go to man-pre-fft-900.
      *                  *---------------------------------------------*
      *                  * Se il codice fornitore di riferimento e' a  *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tuc       =    zero
                     go to man-pre-fft-900.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo fornitore : nessuna ma- *
      *                  * nipolazione                                 *
      *                  *---------------------------------------------*
           if        rf-dcf-sta-tux       not  = 02 and
                     rf-dcf-sta-tux       not  = 03
                     go to man-pre-fft-900.
       man-pre-fft-300.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * del fornitore, ed eventualmente anche la    *
      *                  * dipendenza                                  *
      *                  *---------------------------------------------*
           move      rf-dcf-sta-tuc       to   rf-fft-cod-arc         .
           if        rf-dcf-sta-tux       =    02
                     move  spaces         to   rf-fft-dpz-arc         .
       man-pre-fft-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     man-pre-fft-999.
       man-pre-fft-999.
           exit.

      *    *===========================================================*
      *    * Manipolazioni preliminari sul record relativo alla riga   *
      *    * documento rf-ffr letta                                    *
      *    *-----------------------------------------------------------*
       man-pre-ffr-000.
      *              *-------------------------------------------------*
      *              * Eventuale sostituzione del codice del prodotto  *
      *              * in funzione dello status espresso in anagrafica *
      *              * commerciale                                     *
      *              *-------------------------------------------------*
       man-pre-ffr-100.
      *                  *---------------------------------------------*
      *                  * Se il tipo riga non indica un prodotto :    *
      *                  * nessuna manipolazione                       *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-rig       not  = "P    "
                     go to man-pre-ffr-900.
      *                  *---------------------------------------------*
      *                  * Se il tipo codice di magazzino non indica   *
      *                  * un prodotto : nessuna manipolazione         *
      *                  *---------------------------------------------*
           if        rf-ffr-tip-mag       not  = 01
                     go to man-pre-ffr-900.
       man-pre-ffr-200.
      *                  *---------------------------------------------*
      *                  * Lettura record dell'anagrafica commerciale  *
      *                  * del prodotto                                *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-ffr-num-mag       to   rf-dcp-num-pro         .
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
                     go to man-pre-ffr-900.
       man-pre-ffr-300.
      *                  *---------------------------------------------*
      *                  * Se lo status commerciale del prodotto non   *
      *                  * indica che e' stato sostituito da un' altro *
      *                  * prodotto : nessuna manipolazione            *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tus       not  = 21 and
                     rf-dcp-sta-tus       not  = 52 and
                     rf-dcp-sta-tus       not  = 72
                     go to man-pre-ffr-900.
      *                  *---------------------------------------------*
      *                  * Se il codice prodotto di riferimento e' a   *
      *                  * zero : nessuna manipolazione                *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tuc       =    zero
                     go to man-pre-ffr-900.
      *                  *---------------------------------------------*
      *                  * Se comunque le statistiche non devono esse- *
      *                  * re girate sul nuovo prodotto : nessuna ma-  *
      *                  * nipolazione                                 *
      *                  *---------------------------------------------*
           if        rf-dcp-sta-tux       not  = 02
                     go to man-pre-ffr-900.
       man-pre-ffr-400.
      *                  *---------------------------------------------*
      *                  * Manipolazione : si sostituisce il codice    *
      *                  * numerico del prodotto                       *
      *                  *---------------------------------------------*
           move      rf-dcp-sta-tuc       to   rf-ffr-num-mag         .
       man-pre-ffr-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     man-pre-ffr-999.
       man-pre-ffr-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, in w-mod-ffo-ffr-cos, del- *
      *    * la voce 'Costo del Fatturato' corrispondente al record    *
      *    * in 'rf-ffr'                                               *
      *    *                                                           *
      *    * Input  : rf-ffr            = Record 'rf-ffr' in esame'    *
      *    *                                                           *
      *    *          w-aux-ffo-ffr-ccf = Tipo di calcolo per la voce  *
      *    *                                                           *
      *    *          w-mod-ffo-ffr-qta = Valore della voce 'Quanti-   *
      *    *                              ta' Fatturata' gia' deter-   *
      *    *                              minata                       *
      *    *                                                           *
      *    * Output : w-mod-ffo-ffr-cos = Valore della voce 'Costo del *
      *    *                              Fatturato'                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-cos-ffo-ffr-000.
      *              *-------------------------------------------------*
      *              * Azzeramento preventivo del valore da determina- *
      *              * re                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-ffo-ffr-cos      .
       det-cos-ffo-ffr-100.
      *              *-------------------------------------------------*
      *              * Se tipo calcolo non riconosciuto : uscita imme- *
      *              * diata senza nessuna ulteriore azione            *
      *              *-------------------------------------------------*
           if        w-aux-ffo-ffr-ccf    not  = "P" and
                     w-aux-ffo-ffr-ccf    not  = "U"
                     go to det-cos-ffo-ffr-999.
      *              *-------------------------------------------------*
      *              * Se la quantita' fatturata di riferimento e' a   *
      *              * zero : uscita immediata senza nessuna ulteriore *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-mod-ffo-ffr-qta    =    zero
                     go to det-cos-ffo-ffr-999.
      *              *-------------------------------------------------*
      *              * Se tipo codice di magazzino non ammesso per la  *
      *              * determinazione del costo unitario : uscita sen- *
      *              * za nessuna ulteriore azione                     *
      *              *-------------------------------------------------*
           if        rf-ffr-tip-mag       not  = 01 and
                     rf-ffr-tip-mag       not  = 02 and
                     rf-ffr-tip-mag       not  = 03 and
                     rf-ffr-tip-mag       not  = 04
                     go to det-cos-ffo-ffr-999.
      *              *-------------------------------------------------*
      *              * Se codice numerico di magazzino a zero : uscita *
      *              * senza nessuna ulteriore azione                  *
      *              *-------------------------------------------------*
           if        rf-ffr-num-mag       =    zero
                     go to det-cos-ffo-ffr-999.
       det-cos-ffo-ffr-200.
      *              *-------------------------------------------------*
      *              * Apertura preliminare, se necessario, del modulo *
      *              * per la determinazione del valore unitario di    *
      *              * magazzino                                       *
      *              *-------------------------------------------------*
           perform   opn-vun-mag-000      thru opn-vun-mag-999        .
       det-cos-ffo-ffr-300.
      *              *-------------------------------------------------*
      *              * Determinazione del valore unitario di magazzino *
      *              *-------------------------------------------------*
       det-cos-ffo-ffr-310.
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
           if        w-aux-ffo-ffr-ccf    =    "P"
                     move  0001           to   d-vun-mag-tip-val
           else if   w-aux-ffo-ffr-ccf    =    "U"
                     move  0002           to   d-vun-mag-tip-val
           else      move  0001           to   d-vun-mag-tip-val      .
      *                      *-----------------------------------------*
      *                      * Data di riferimento per il valore uni-  *
      *                      * tario: pari alla data documento         *
      *                      *-----------------------------------------*
           move      rf-ffr-dat-doc       to   d-vun-mag-dat-val      .
      *                      *-----------------------------------------*
      *                      * Tipo codice di magazzino                *
      *                      *-----------------------------------------*
           move      rf-ffr-tip-mag       to   d-vun-mag-tip-mag      .
      *                      *-----------------------------------------*
      *                      * Codice numerico di magazzino            *
      *                      *-----------------------------------------*
           move      rf-ffr-num-mag       to   d-vun-mag-num-mag      .
      *                      *-----------------------------------------*
      *                      * Giacenza di proprieta': non influente   *
      *                      *-----------------------------------------*
           move      zero                 to   d-vun-mag-gia-prp      .
       det-cos-ffo-ffr-320.
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo                         *
      *                  *---------------------------------------------*
           perform   det-vun-mag-cll-000  thru det-vun-mag-cll-999    .
       det-cos-ffo-ffr-330.
      *                  *---------------------------------------------*
      *                  * Se errori di esecuzione nel modulo : uscita *
      *                  * immediata lasciando il valore della voce    *
      *                  * 'Costo del Fatturato' a zero                *
      *                  *---------------------------------------------*
           if        d-vun-mag-exi-sts    not  = spaces
                     go to det-cos-ffo-ffr-999.
      *                  *---------------------------------------------*
      *                  * Se il valore unitario determinato e' a zero *
      *                  * : uscita immediata lasciando il valore del- *
      *                  * la voce 'Costo del Fatturato' a zero        *
      *                  *---------------------------------------------*
           if        d-vun-mag-val-uni    =    zero
                     go to det-cos-ffo-ffr-999.
       det-cos-ffo-ffr-400.
      *              *-------------------------------------------------*
      *              * Preparazione del valore della voce 'Costo del   *
      *              * Fatturato' come moltiplicazione tra la Quanti-  *
      *              * ta' Fatturata e il Valore unitario appena de-   *
      *              * terminato                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-mod-ffo-ffr-qta    by   d-vun-mag-val-uni
                                        giving w-mod-ffo-ffr-cos
                                                         rounded      .
      *                  *---------------------------------------------*
      *                  * Applicazione decimali gestione magazzino    *
      *                  *---------------------------------------------*
           divide    100                  into w-mod-ffo-ffr-cos      .
       det-cos-ffo-ffr-999.
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

