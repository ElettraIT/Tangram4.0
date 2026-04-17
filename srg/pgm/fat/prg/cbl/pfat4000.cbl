       Identification Division.
       Program-Id.                                 pfat4000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    dif                 *
      *                                   Fase:    fat400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 18/07/94    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Fatturazione differita                      *
      *                                                                *
      *                    Richieste per esecuzione                    *
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
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      *    *===========================================================*
      *    * File Control [rlt]                                        *
      *    *-----------------------------------------------------------*
           select  optional  rlt   assign  to disk          f-rlt-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is               w-rlt-krn
                             file  status is                f-rlt-sts .

      *    *===========================================================*
      *    * File Control [lsf]                                        *
      *    *-----------------------------------------------------------*
           select            lsf   assign to input-output   f-lsf-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-lsf-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Codice cliente per fatturazione                   *
      *            *---------------------------------------------------*
               10  srt-cli-plf            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice lingua                                     *
      *            *---------------------------------------------------*
               10  srt-cod-lng            pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Fatturazione separata per il documento            *
      *            *                                                   *
      *            * - Spaces : No                                     *
      *            * - S      : Si                                     *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  srt-snx-fts            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo conferma d'ordine cliente       *
      *            *                                                   *
      *            * N.B. : Solo se una fattura per ogni ordine        *
      *            *---------------------------------------------------*
               10  srt-coc-prt-011        pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo fornitura                                    *
      *            *                                                   *
      *            * - 01 : Diretta                                    *
      *            * - 11 : Tramite Leasing                            *
      *            * - 21 : Tramite Gruppo d'Acquisto                  *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  srt-tip-frn            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente per tipo fornitura                 *
      *            *                                                   *
      *            * - Se tipo fornitura 01 Diretta                    *
      *            *        Valore a zero                              *
      *            * - Se tipo fornitura 11 Tramite Leasing            *
      *            *        Pari al cliente commerciale                *
      *            * - Se tipo fornitura 21 Tramite Gruppo d'Acquisto  *
      *            *   - Se tipo fatturazione 01 Separata              *
      *            *        Pari al cliente commerciale                *
      *            *   - Se tipo fatturazione 02 Cumulativa            *
      *            *        Valore a zero                              *
      *            *---------------------------------------------------*
               10  srt-cod-cli-tpf        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Dipendenza del cliente                            *
      *            *                                                   *
      *            * - Se tipo fornitura 01 Diretta                    *
      *            *        Valore a spaces                            *
      *            * - Se tipo fornitura 11 Tramite Leasing            *
      *            *        Pari alla dipendenza cliente commerciale   *
      *            * - Se tipo fornitura 21 Tramite Gruppo d'Acquisto  *
      *            *   - Se tipo fatturazione 01 Separata              *
      *            *        Pari alla dipendenza cliente commerciale   *
      *            *   - Se tipo fatturazione 02 Cumulativa            *
      *            *        Valore a spaces                            *
      *            *---------------------------------------------------*
               10  srt-dpz-cli-tpf        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Dipendenza del cliente per fatturazione           *
      *            *                                                   *
      *            * N.B. : Solo se una fattura per ogni bolla         *
      *            *---------------------------------------------------*
               10  srt-dpc-plf-003        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Data documento                                    *
      *            *                                                   *
      *            * N.B. : Solo se una fattura per ogni bolla         *
      *            *---------------------------------------------------*
               10  srt-dat-doc-003        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento                                  *
      *            *                                                   *
      *            * N.B. : Solo se una fattura per ogni bolla         *
      *            *---------------------------------------------------*
               10  srt-num-doc-003        pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento per generazione fattura            *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-tmo-ftr-1o2        pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Sigla valuta per fatturazione                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-sgl-vpf-1o2        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali valuta per fatturazione           *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-dec-vpf-1o2        pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di cambio per la valuta per fatturazione     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-tdc-vpf-1o2        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Assoggettamento iva cliente                       *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-ass-iva-1o2        pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-cod-age-1o2        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Flag di significativita' provvigioni per il docu- *
      *            * mento                                             *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-fsp-doc-1o2        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Ammontare provvigioni a forfait per il documento  *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-pvf-age-1o2        pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo vendita per l'agente                         *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-tip-vpa-1o2        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice intermediario                              *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-cod-ime-1o2        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Presenza di un ammontare a forfait per le provvi- *
      *            * gioni per l'intermediario                         *
      *            *                                                   *
      *            * - Spaces : No                                     *
      *            * - S      : Si                                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-snx-pfi-1o2        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Dipendenza del cliente per fatturazione           *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle di tipo 02    *
      *            *---------------------------------------------------*
               10  srt-dpc-plf-002        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Inoltro pagamenti al cliente                      *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle di tipo 02    *
      *            *---------------------------------------------------*
               10  srt-inl-pgt-1o2        pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice forma di pagamento                         *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-cod-fop-1o2        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Presenza di una quota a forfait manuale indotta   *
      *            * dalla forma di pagamento                          *
      *            *                                                   *
      *            * - Spaces : No                                     *
      *            * - S      : Si                                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            *        Solo se quota a forfait effettivamente im- *
      *            *        postata in bolla diversa da zero           *
      *            *---------------------------------------------------*
               10  srt-snx-qaf-1o2        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Presenza di una data scadenza manuale indotta     *
      *            * dalla forma di pagamento                          *
      *            *                                                   *
      *            * - Spaces : No                                     *
      *            * - S      : Si                                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-snx-dsm-1o2        pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice ABI per l'appoggio bancario                *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            *        Solo se la forma di pagamento contiene al- *
      *            *        meno un codice di pagamento che richieda   *
      *            *        l'appoggio bancario                        *
      *            *---------------------------------------------------*
               10  srt-cod-abi-1o2        pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice CAB per l'appoggio bancario                *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            *        Solo se la forma di pagamento contiene al- *
      *            *        meno un codice di pagamento che richieda   *
      *            *        l'appoggio bancario                        *
      *            *                                                   *
      *            *        Solo se il codice ABI e' diverso da zero   *
      *            *---------------------------------------------------*
               10  srt-cod-cab-1o2        pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice c/c per l'appoggio bancario                *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            *        Solo se la forma di pagamento contiene al- *
      *            *        meno un codice di pagamento che richieda   *
      *            *        l'appoggio bancario ed il c/corrente       *
      *            *                                                   *
      *            *        Solo se il codice ABI e' diverso da zero   *
      *            *        Solo se il codice CAB e' diverso da zero   *
      *            *---------------------------------------------------*
               10  srt-ccc-app-1o2        pic  x(12)                  .
      *            *---------------------------------------------------*
      *            * Data scadenza manuale                             *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            *        Solo se in presenza di data scadenza manu- *
      *            *        ale indotta dalla forma di pagamento       *
      *            *---------------------------------------------------*
               10  srt-pag-dsm-1o2        pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice addebito spese incasso                     *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-add-spi-1o2        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Codice addebito spese bollo                       *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *---------------------------------------------------*
               10  srt-add-spb-1o2        pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Si/No addebito spese                              *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle               *
      *            *                                                   *
      *            * N.B. : Solo se personalizzazione su tipo accetta- *
      *            *        zione spese per fatturazione al valore 01  *
      *            *---------------------------------------------------*
               10  srt-spe-snx-1o2 occurs 06
                                          pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Dipendenza del cliente per fatturazione           *
      *            *                                                   *
      *            * N.B. : Solo se raggruppamento bolle di tipo 01    *
      *            *---------------------------------------------------*
               10  srt-dpc-plf-001        pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Data documento                                    *
      *            *---------------------------------------------------*
               10  srt-dat-doc            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento                                  *
      *            *---------------------------------------------------*
               10  srt-num-doc            pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *                                                   *
      *            * - Se tipo fornitura 01 Diretta                    *
      *            *        Pari al cliente commerciale                *
      *            * - Se tipo fornitura 11 Tramite Leasing            *
      *            *        Pari al cliente commerciale                *
      *            * - Se tipo fornitura 21 Tramite Gruppo d'Acquisto  *
      *            *   - Se tipo fatturazione 01 Separata              *
      *            *        Pari al cliente commerciale                *
      *            *   - Se tipo fatturazione 02 Cumulativa            *
      *            *        Pari al cliente per fatturazione           *
      *            *---------------------------------------------------*
               10  srt-cod-cli            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza cliente                         *
      *            *                                                   *
      *            * - Se tipo fornitura 01 Diretta                    *
      *            *        Pari alla dipendenza cliente commerciale   *
      *            * - Se tipo fornitura 11 Tramite Leasing            *
      *            *        Pari alla dipendenza cliente commerciale   *
      *            * - Se tipo fornitura 21 Tramite Gruppo d'Acquisto  *
      *            *   - Se tipo fatturazione 01 Separata              *
      *            *        Pari alla dipendenza cliente commerciale   *
      *            *   - Se tipo fatturazione 02 Cumulativa            *
      *            *        Pari alla dipendenza cliente per fattura-  *
      *            *        zione                                      *
      *            *---------------------------------------------------*
               10  srt-dpz-cli            pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo record [bit] : s.aa.dd.nnnnnn   *
      *            *---------------------------------------------------*
               10  srt-prt-bit            pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo raggruppamento bolle in fattura per il cli-  *
      *            * ente                                              *
      *            * - 01 : Una unica fattura per tutte le bolle e per *
      *            *        qualsiasi destinatario del cliente         *
      *            * - 02 : Una fattura per ogni destinatario del cli- *
      *            *        ente, ma raggruppando le bolle             *
      *            * - 03 : Una fattura per ogni bolla                 *
      *            * - 11 : Una fattura per ogni ordine                *
      *            *---------------------------------------------------*
               10  srt-rag-bft            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Assoggettamento iva del cliente                   *
      *            *---------------------------------------------------*
               10  srt-tas-ivc            pic  9(02)                  .
               10  srt-ass-iva            pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Inoltro documenti                                 *
      *            * - 01 : alla sede                                  *
      *            * - 02 : alla sede legale                           *
      *            * - 03 : alla filiale                               *
      *            *---------------------------------------------------*
               10  srt-inl-dcm            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo esclusione mesi per le scadenze              *
      *            * - 01 : Nessun mese escluso                        *
      *            * - 02 : Si sposta la scadenza di un mese           *
      *            * - 03 : Si sposta la scadenza ad un giorno fisso   *
      *            *        del mese successivo                        *
      *            *---------------------------------------------------*
               10  srt-tip-esm            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Giorno di scadenza fisso nel mese successivo, se  *
      *            * tipo esclusione mese pari a 03                    *
      *            *---------------------------------------------------*
               10  srt-ggg-alt            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Mese di esclusione per le scadenze del cliente 1  *
      *            *---------------------------------------------------*
               10  srt-mmm-e01            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Mese di esclusione per le scadenze del cliente 2  *
      *            *---------------------------------------------------*
               10  srt-mmm-e02            pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Si/no contrassegno                                *
      *            *---------------------------------------------------*
               10  srt-snx-cts            pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice Abi da anagrafica cliente commerciale      *
      *            *---------------------------------------------------*
               10  srt-cod-abi            pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice Cab da anagrafica cliente commerciale      *
      *            *---------------------------------------------------*
               10  srt-cod-cab            pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * C/C appoggio anagrafica cliente commerciale       *
      *            *---------------------------------------------------*
               10  srt-ccc-app            pic  x(12)                  .
      *            *---------------------------------------------------*
      *            * Numero records [bir] contenuti nel documento      *
      *            *---------------------------------------------------*
               10  srt-ctr-bir            pic  9(05)                  .

      *    *===========================================================*
      *    * File Description [rlt]                                    *
      *    *-----------------------------------------------------------*
       fd  rlt.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  rlt-rec.
      *        *-------------------------------------------------------*
      *        * Dati relativi al record [fir]                         *
      *        *-------------------------------------------------------*
           05  rlt-rec-fir.
               10  filler  occurs  1536   pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Dati relativi al record [fix]                         *
      *        *-------------------------------------------------------*
           05  rlt-rec-fix.
               10  filler  occurs  1536   pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [lsf]                                    *
      *    *-----------------------------------------------------------*
       fd  lsf  label record omitted.
       01  lsf-rec.
           05  lsf-chr        occurs 9472 pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controlli i-o su [rlt]                *
      *    *-----------------------------------------------------------*
       01  f-rlt.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-rlt-nam                  pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-rlt-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-rlt-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Seconda area ausiliaria per controlli i-o su [rlt]        *
      *    *-----------------------------------------------------------*
       01  w-rlt.
      *        *-------------------------------------------------------*
      *        * Record number interessato dall'operazione di i-o      *
      *        *-------------------------------------------------------*
           05  w-rlt-krn                  pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Max record number memorizzato in assoluto             *
      *        *-------------------------------------------------------*
           05  w-rlt-max                  pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Contatore records per singolo documento               *
      *        *-------------------------------------------------------*
           05  w-rlt-ctr                  pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Indice per scansione records per singolo documento    *
      *        *-------------------------------------------------------*
           05  w-rlt-inx                  pic  9(07) value zero       .
      *        *-------------------------------------------------------*
      *        * Status di uscita da 'Start' e 'Next'                  *
      *        *-------------------------------------------------------*
           05  w-rlt-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [lsf]                *
      *    *-----------------------------------------------------------*
       01  f-lsf.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-lsf-nam                  pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-lsf-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-lsf-sts                  pic  x(02) value "00"       .

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
                     "fat"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "dif"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "fat400"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pfat4000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "         FATTURAZIONE DIFFERITA         "       .

      *    *===========================================================*
      *    * Area per il programma di esecuzione                       *
      *    *-----------------------------------------------------------*
       01  i-exe.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma di esecuzione             *
      *        *-------------------------------------------------------*
           05  i-exe-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Pathname del programma di esecuzione                  *
      *        *-------------------------------------------------------*
           05  i-exe-pat                  pic  x(40)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
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
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .
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
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [bix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbix"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [fix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffix"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [ftilfd]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/num/rec/rnftilfd"                       .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .
      *        *-------------------------------------------------------*
      *        * [zop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzop"                          .
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenza per dipendenza di default                   *
      *        *-------------------------------------------------------*
           05  w-ref-dpz-def.
               10  w-ref-dpz-def-cod      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Referenze relative allo sconto in chiusura            *
      *        *-------------------------------------------------------*
           05  w-ref-sco-chi.
      *            *---------------------------------------------------*
      *            * Descrizione a video sconto in chiusura            *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-des      pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Codice iva sconto in chiusura                     *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-civ      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contropartita sconto in chiusura                  *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Referenze relative allo sconto pagamento              *
      *        *-------------------------------------------------------*
           05  w-ref-sco-pag.
      *            *---------------------------------------------------*
      *            * Descrizione a video sconto pagamento              *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-des      pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Codice iva sconto pagamento                       *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-civ      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contropartita sconto pagamento                    *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-ccp      pic  9(07)                  .

      *    *===========================================================*
      *    * Area per definizione limiti per la fattura                *
      *    *-----------------------------------------------------------*
       01  w-lim-fat.
      *        *-------------------------------------------------------*
      *        * Numero massimo righe per una fattura                  *
      *        *-------------------------------------------------------*
           05  w-lim-fat-max-rig          pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Numero massimo bolle per una fattura                  *
      *        *                                                       *
      *        * N.B.: se deve essere variato il presente valore, deve *
      *        *       essere adeguata anche la capienza del buffer    *
      *        *       protocolli bolle in fattura (w-buf-pbf)         *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-lim-fat-max-tes          pic  9(05) value 199        .
      *        *-------------------------------------------------------*
      *        * Numero massimo contropartite per una fattura          *
      *        *-------------------------------------------------------*
           05  w-lim-fat-max-ccp          pic  9(02) value 10         .

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative alle spese per la fattura- *
      *        * zione                                                 *
      *        *-------------------------------------------------------*
           05  w-prs-spe-fat.
      *            *---------------------------------------------------*
      *            * Tipo accettazione                                 *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tac      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di spese personalizzate totale             *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-nst      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella spese personalizzate, impaccate in alto,  *
      *            * le spese non personalizzate non sono contenute    *
      *            * nella tabella                                     *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tbl occurs 06.
      *                *-----------------------------------------------*
      *                * Numero spesa : 1..6                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-npt  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione per il video estesa               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-dve  pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * Tipo funzionamento spesa                      *
      *                * - 01 : A percentuale fissa per tutti i clien- *
      *                *        ti cui viene addebitata la spesa       *
      *                * - 02 : A percentuale variabile per ogni cli-  *
      *                *        ente cui viene addebitata la spesa,    *
      *                *        con la percentuale di spesa espressa   *
      *                *        nel record dati commerciali cliente    *
      *                * - 03 : Ad importo fisso per tutti i clienti   *
      *                *        cui viene addebitata la spesa          *
      *                * - 04 : Ad importo variabile per tutti i cli-  *
      *                *        enti cui viene addebitata la spesa,    *
      *                *        con l'importo della spesa espresso nel *
      *                *        record dati commerciali cliente        *
      *                * - 05 : A seconda dei clienti, per ognuno dei  *
      *                *        quali si esprimera', nel record dati   *
      *                *        commerciali, se la spesa :             *
      *                *        - non va' addebitata                   *
      *                *        - va' addebitata in percentuale        *
      *                *        - va' addebitata ad importo            *
      *                *        e dove inoltre si esprimera' l'even-   *
      *                *        tuale percentuale o importo da addebi- *
      *                *        tare                                   *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-tfs  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Percentuale fissa per la spesa, in caso che   *
      *                * il tipo funzionamento spesa sia pari a 01,    *
      *                * oppure di default per l'anagrafica dati com-  *
      *                * merciali cliente nel caso che il tipo fun-    *
      *                * zionamento spesa sia pari a 02                *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-per  pic  9(02)v9(01)            .
      *                *-----------------------------------------------*
      *                * Imponibile per la percentuale della spesa,    *
      *                * solo se il tipo funzionamento spesa e' pari a *
      *                * 01 o 02 o 05                                  *
      *                * - 00 : Non significativo                      *
      *                * - 01 : Totale merce                           *
      *                * - 02 : Totale netto                           *
      *                * - 03 : Su una combinazione delle voci seguenti*
      *                * - 11 : Totale merci                           *
      *                * - 12 : Totale servizi                         *
      *                * - 13 : Totale imballi                         *
      *                * - 14 : Totale libero 4                        *
      *                * - 15 : Totale libero 5                        *
      *                * - 16 : Totale libero 6                        *
      *                * - 17 : Totale libero 7                        *
      *                * - 18 : Totale libero 8                        *
      *                * - 19 : Totale extra                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibl  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Totalizzatori da considerarsi per la determi- *
      *                * nazione dell'imponibile per la percentuale    *
      *                * della spesa                                   *
      *                *                                               *
      *                * Solo se l'imponibile per la percentuale e'    *
      *                * pari a 03                                     *
      *                *                                               *
      *                * Per ogni totalizzatore deve essere specifica- *
      *                * to un valore di un carattere, dove :          *
      *                *                                               *
      *                * - N : Non concorre a formare l'imponibile     *
      *                * - S : Concorre a formare l'imponibile         *
      *                *                                               *
      *                * Il significato dei totalizzatori e' di tipo   *
      *                * posizionale, con la seguente codifica :       *
      *                *                                               *
      *                * 1 : Totale merci                              *
      *                * 2 : Totale servizi                            *
      *                * 3 : Totale imballi                            *
      *                * 4 : Totale libero 4                           *
      *                * 5 : Totale libero 5                           *
      *                * 6 : Totale libero 6                           *
      *                * 7 : Totale libero 7                           *
      *                * 8 : Totale libero 8                           *
      *                * 9 : Totale extra                              *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibt.
                       20  w-prs-spe-fat-ibx occurs 09
                                          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Importo fisso per la spesa in caso che il ti- *
      *                * po funzionamento spesa e' pari a 03, oppure   *
      *                * di default per l'anagrafica dati commerciali  *
      *                * cliente nel caso che il tipo funzionamento    *
      *                * spesa sia pari a 04                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-imp  pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Codice iva per la spesa. Se zero significa che*
      *                * la spesa non e' soggetta ad una aliquota iva  *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione agli altri imponi-*
      *                * bili di fattura                               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-civ  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice contropartita per la spesa. Se zero si-*
      *                * gnifica che la spesa non ha una contropartita *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione alle altre contro-*
      *                * partite di fattura                            *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ccp  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatori, indici, e comodi locali                *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-i01      pic  9(01)                  .
               10  w-prs-spe-fat-i02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione relativa al tipo imponibile per lo  *
      *        * sconto di pagamento                                   *
      *        *-------------------------------------------------------*
           05  w-prs-tim-scp.
      *            *---------------------------------------------------*
      *            * Tipo di imponibile                                *
      *            *  - C : Comprendente le spese, normale             *
      *            *  - N : Non comprendente le spese                  *
      *            *  - A : Arrotondamento all'incasso                 *
      *            *---------------------------------------------------*
               10  w-prs-tim-scp-tip      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no gestione lettera d'intenti cliente              *
      *        *-------------------------------------------------------*
           05  w-prs-snx-lic              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione agenti attiva                          *
      *        *-------------------------------------------------------*
           05  w-prs-age-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No gestione portafoglio attiva                     *
      *        *-------------------------------------------------------*
           05  w-prs-gep-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative alla gestione portafoglio  *
      *        *-------------------------------------------------------*
           05  w-prs-gep.
      *            *---------------------------------------------------*
      *            * Tabella parametri per emissione scadenze per cia- *
      *            * scun tipo pagamento                               *
      *            *---------------------------------------------------*
               10  w-prs-gep-tbl.
      *                *-----------------------------------------------*
      *                * Singolo elemento in tabella                   *
      *                *-----------------------------------------------*
                   15  w-prs-gep-ele occurs 11.
      *                    *-------------------------------------------*
      *                    * Si/no esistenza personalizzazione per il  *
      *                    * tipo pagamento                            *
      *                    *-------------------------------------------*
                       20  w-prs-gep-snx-prs
                                          pic  x(01)                  .
      *                    *-------------------------------------------*
      *                    * Codice causale per il tipo pagamento      *
      *                    *-------------------------------------------*
                       20  w-prs-gep-cau-cge
                                          pic  9(03)                  .
      *                    *-------------------------------------------*
      *                    * Codice sottoconto per il tipo pagamento   *
      *                    *-------------------------------------------*
                       20  w-prs-gep-stc-cge
                                          pic  9(07)                  .
               10  w-prs-gep-ctr-001      pic  9(03)                  .
               10  w-prs-gep-wrk-top      pic  9(04)                  .
               10  w-prs-gep-wrk-top-r    redefines
                   w-prs-gep-wrk-top.
                   15  filler             pic  9(02)                  .
                   15  w-prs-gep-wto-tpg  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo accettazione spese per la fatturazione           *
      *        *-------------------------------------------------------*
           05  w-prs-tac-spf              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Modalita' di utilizzo dei codici ABI e CAB in emis-   *
      *        * sione scadenze                                        *
      *        *-------------------------------------------------------*
           05  w-prs-dcc-abi              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo trattamento assoggettamento iva del cliente      *
      *        *-------------------------------------------------------*
           05  w-prs-trt-aic              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/no fattura in attesa di verifica                   *
      *        *-------------------------------------------------------*
           05  w-prs-snx-fav              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo esecuzione                            *
      *        *-------------------------------------------------------*
           05  w-exp-tip-exe.
               10  w-exp-tip-exe-num      pic  9(02)       value 2    .
               10  w-exp-tip-exe-lun      pic  9(02)       value 25   .
               10  w-exp-tip-exe-tbl.
                   15  filler             pic  x(25) value
                            "Stampa di simulazione    "               .
                   15  filler             pic  x(25) value
                            "Emissione definitiva     "               .
      *        *-------------------------------------------------------*
      *        * Work per : Periodicita' di fatturazione               *
      *        *-------------------------------------------------------*
           05  w-exp-per-fat.
               10  w-exp-per-fat-num      pic  9(02)       value 12   .
               10  w-exp-per-fat-lun      pic  9(02)       value 15   .
               10  w-exp-per-fat-tbl.
                   15  filler             pic  x(15) value
                            "Tutte          "                         .
                   15  filler             pic  x(15) value
                            "Giornaliera    "                         .
                   15  filler             pic  x(15) value
                            "Settimanale    "                         .
                   15  filler             pic  x(15) value
                            "Decadale       "                         .
                   15  filler             pic  x(15) value
                            "Quindicinale   "                         .
                   15  filler             pic  x(15) value
                            "Mensile        "                         .
                   15  filler             pic  x(15) value
                            "Bimestrale     "                         .
                   15  filler             pic  x(15) value
                            "Trimestrale    "                         .
                   15  filler             pic  x(15) value
                            "Quadrimestrale "                         .
                   15  filler             pic  x(15) value
                            "Semestrale     "                         .
                   15  filler             pic  x(15) value
                            "Annuale        "                         .
                   15  filler             pic  x(15) value
                            "Biennale       "                         .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-nom      pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfi.
               10  w-let-arc-zfi-flg      pic  x(01)                  .
               10  w-let-arc-zfi-cod      pic  x(05)                  .
               10  w-let-arc-zfi-dpz      pic  9(02)                  .
               10  w-let-arc-zfi-des      pic  x(30)                  .
               10  w-let-arc-zfi-org      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [lic]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-lic.
               10  w-let-arc-lic-flg      pic  x(01)                  .
               10  w-let-arc-lic-cod      pic  9(07)                  .
               10  w-let-arc-lic-dri      pic  9(07)                  .
               10  w-let-arc-lic-drf      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-dui      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  9(02)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-dlb      pic  9(01)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
               10  w-let-arc-dcp-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zac]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zac.
               10  w-let-arc-zac-flg      pic  x(01)                  .
               10  w-let-arc-zac-tip      pic  9(02)                  .
               10  w-let-arc-zac-cod      pic  9(03)                  .
               10  w-let-arc-zac-des.
                   15  w-let-arc-zac-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-zac-civ      pic  9(05)                  .
               10  w-let-arc-zac-ccp      pic  9(07)                  .
               10  w-let-arc-zac-tot      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvf.
               10  w-let-arc-zvf-flg      pic  x(01)                  .
               10  w-let-arc-zvf-num      pic  9(03)                  .
               10  w-let-arc-zvf-cod      pic  x(03)                  .
               10  w-let-arc-zvf-des      pic  x(25)                  .
               10  w-let-arc-zvf-spf      pic  9(03)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [ada]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/larcada0.ltw"                   .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-cod-cli              pic  9(07)                  .
           05  w-sav-prz-ven              pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .
               10  w-err-box-err-m03      pic  x(65)                  .

      *    *===========================================================*
      *    * Work-area per rottura fattura                             *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * Codice cliente per la fatturazione                    *
      *        *-------------------------------------------------------*
           05  w-rot-cli-plf              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice lingua                                         *
      *        *-------------------------------------------------------*
           05  w-rot-cod-lng              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo ordine                                     *
      *        *-------------------------------------------------------*
           05  w-rot-coc-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza cliente per la fatturazione         *
      *        *-------------------------------------------------------*
           05  w-rot-dpc-plf              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo fornitura                                        *
      *        *-------------------------------------------------------*
           05  w-rot-tip-frn              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-rot-cod-cli              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza cliente                             *
      *        *-------------------------------------------------------*
           05  w-rot-dpz-cli              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento per la fatturazione                    *
      *        *-------------------------------------------------------*
           05  w-rot-tmo-ftr              pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Valuta per la fatturazione                            *
      *        *-------------------------------------------------------*
           05  w-rot-sgl-vpf              pic  x(03)                  .
           05  w-rot-dec-vpf              pic  9(01)                  .
           05  w-rot-tdc-vpf              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Assoggettamento Iva                                   *
      *        *-------------------------------------------------------*
           05  w-rot-ass-iva              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente                                         *
      *        *-------------------------------------------------------*
           05  w-rot-cod-age              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di significativita' provvigioni per il documento *
      *        *-------------------------------------------------------*
           05  w-rot-fsp-doc              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * % provvigioni a forfait                               *
      *        *-------------------------------------------------------*
           05  w-rot-pvf-age              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo vendita per l'agente                             *
      *        *-------------------------------------------------------*
           05  w-rot-tip-vpa              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice intermediario                                  *
      *        *-------------------------------------------------------*
           05  w-rot-cod-ime              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no provvigioni a forfait per l'intermediario       *
      *        *-------------------------------------------------------*
           05  w-rot-snx-pfi              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Inoltro pagamenti                                     *
      *        *-------------------------------------------------------*
           05  w-rot-inl-pgt              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice forma di pagamento                             *
      *        *-------------------------------------------------------*
           05  w-rot-cod-fop              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Quote a forfait                                       *
      *        *-------------------------------------------------------*
           05  w-rot-snx-qaf              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Presenza di una data scadenza manuale                 *
      *        *-------------------------------------------------------*
           05  w-rot-snx-dsm              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Coordinate bancarie                                   *
      *        *-------------------------------------------------------*
           05  w-rot-cod-abi              pic  9(05)                  .
           05  w-rot-cod-cab              pic  9(05)                  .
           05  w-rot-ccc-app              pic  x(12)                  .
           05  w-rot-pag-dsm              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Spese                                                 *
      *        *-------------------------------------------------------*
           05  w-rot-add-spi              pic  x(03)                  .
           05  w-rot-add-spb              pic  x(03)                  .
           05  w-rot-spe-snx occurs 06    pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ad uso interno                              *
      *        *-------------------------------------------------------*
           05  w-rot-ctr-rig              pic  9(05)                  .
           05  w-rot-ctr-tes              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per comodi di accettazione                      *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Area per impostazione serie elementi da selezionare   *
      *        *-------------------------------------------------------*
           05  w-acc-ser-edd.
               10  w-acc-ser-edd-max      pic  9(02) value 36         .
               10  w-acc-ser-edd-pev      pic  9(02)                  .
               10  w-acc-ser-edd-nel      pic  9(02)                  .
               10  w-acc-ser-edd-n1v      pic  9(02)                  .
               10  w-acc-ser-edd-nev      pic  9(02)                  .
               10  w-acc-ser-edd-nec      pic  9(02)                  .
               10  w-acc-ser-edd-fce      pic  x(01)                  .
               10  w-acc-ser-edd-led.
                   15  w-acc-ser-edd-rig  pic  x(03)                  .
                   15  w-acc-ser-edd-dpu  pic  x(01)                  .
                   15  filler             pic  x(02)                  .
                   15  w-acc-ser-edd-cod  pic  x(07)                  .
                   15  filler             pic  x(05)                  .
                   15  w-acc-ser-edd-des  pic  x(40)                  .
                   15  filler             pic  x(18)                  .
               10  w-acc-ser-edd-c01      pic  9(02)                  .
               10  w-acc-ser-edd-c02      pic  9(02)                  .
               10  w-acc-ser-edd-c0a      pic  9(02)                  .
               10  w-acc-ser-edd-c0b      pic  9(02)                  .
               10  w-acc-ser-edd-c0c      pic  9(02)                  .
               10  w-acc-ser-edd-c0p      pic  9(02)                  .
               10  w-acc-ser-edd-c0q      pic  9(02)                  .
               10  w-acc-ser-edd-c0r      pic  9(02)                  .
               10  w-acc-ser-edd-spe      pic  9(07)                  .
               10  w-acc-ser-edd-svk      pic  x(04)                  .
               10  w-acc-ser-edd-stu      pic  x(01)                  .
               10  w-acc-ser-edd-fcl.
                   15  filler             pic  x(08)                  .
                   15  w-acc-ser-edd-070  pic  x(70)                  .
                   15  filler             pic  x(02)                  .
               10  w-acc-ser-edd-txt.
                   15  w-acc-ser-edd-rtr  occurs 03
                                          pic  x(40)                  .
               10  w-acc-ser-edd-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-acc-ser-edd-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-acc-ser-edd-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per determinazione pathname file di appoggio     *
      *        *-------------------------------------------------------*
           05  w-det-pat-fde.
      *            *---------------------------------------------------*
      *            * Progressivo lotto di fatturazione in input        *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-lfd      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Anno del lotto di fatturazione                    *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-lfa      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero del lotto di fatturazione                  *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-lfn      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Pathname completo del file                        *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-pat      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Direttorio di base                                *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-dir      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Nome del file                                     *
      *            *---------------------------------------------------*
               10  w-det-pat-fde-nam      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori prima bolla relativi alla forma   *
      *        * di pagamento                                          *
      *        *-------------------------------------------------------*
           05  w-det-vpb-fop.
               10  w-det-vpb-fop-ctr      pic  9(03)                  .
               10  w-det-vpb-fop-wtp      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori valori prima bolla relativi alle  *
      *        * spese incasso                                         *
      *        *-------------------------------------------------------*
           05  w-det-vpb-spi.
               10  w-det-vpb-spi-c01      pic  9(02)                  .
               10  w-det-vpb-spi-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori valori prima bolla relativi alle  *
      *        * spese bollo                                           *
      *        *-------------------------------------------------------*
           05  w-det-vpb-spb.
               10  w-det-vpb-spb-c01      pic  9(02)                  .
               10  w-det-vpb-spb-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det codici iva a piede documento             *
      *        *-------------------------------------------------------*
           05  w-det-civ-pie.
               10  w-det-civ-pie-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det importo spese                            *
      *        *-------------------------------------------------------*
           05  w-det-imp-spe.
               10  w-det-imp-spe-ibl      pic s9(11)                  .
               10  w-det-imp-spe-ibt.
                   15  w-det-imp-spe-ibx occurs 09
                                          pic  x(01)                  .
               10  w-det-imp-spe-c01      pic  9(02)                  .
               10  w-det-imp-spe-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale al netto delle spese in fattura   *
      *        *-------------------------------------------------------*
           05  w-det-tot-nsf.
               10  w-det-tot-nsf-ctr      pic  9(02)                  .
               10  w-det-tot-nsf-inx      pic  9(02)                  .
               10  w-det-tot-nsf-was      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese incasso calcolate           *
      *        *-------------------------------------------------------*
           05  w-det-tot-sic.
               10  w-det-tot-sic-c01      pic  9(02)                  .
               10  w-det-tot-sic-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese bollo                       *
      *        *-------------------------------------------------------*
           05  w-det-tot-spb.
               10  w-det-tot-spb-c01      pic  9(03)                  .
               10  w-det-tot-spb-c02      pic  9(03)                  .
               10  w-det-tot-spb-c03      pic  9(03)                  .
               10  w-det-tot-spb-wis      pic s9(11)                  .
               10  w-det-tot-spb-wpi      pic s9(11)                  .
               10  w-det-tot-spb-wss      pic s9(11)                  .
               10  w-det-tot-spb-wpc      pic s9(11)                  .
               10  w-det-tot-spb-s13      pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese                             *
      *        *-------------------------------------------------------*
           05  w-det-tot-spe.
               10  w-det-tot-spe-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale imponibile                        *
      *        *-------------------------------------------------------*
           05  w-det-tot-ibl.
               10  w-det-tot-ibl-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale imposta                           *
      *        *-------------------------------------------------------*
           05  w-det-tot-imp.
               10  w-det-tot-imp-ctr      pic  9(02)                  .
               10  w-det-tot-imp-wci      pic  9(05)                  .
               10  w-det-tot-imp-wpa      pic s9(11)                  .
               10  w-det-tot-imp-s11      pic s9(11)                  .
               10  w-det-tot-imp-wpr      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det scadenze del documento                   *
      *        *-------------------------------------------------------*
           05  w-det-sca-doc.
               10  w-det-sca-doc-ctr      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Determinazione data ultimo documento emesso           *
      *        *-------------------------------------------------------*
           05  w-det-ult-doc.
      *            *---------------------------------------------------*
      *            * Numero giornale Iva                               *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-giv      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Sigla numerazione                                 *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-sgl      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Data per il controllo                             *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-dte      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza di emissione                    *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-dpe      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Validita' per le dipendenze                       *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-vld      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatore                                         *
      *            *---------------------------------------------------*
               10  w-det-ult-doc-ctr      pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det spese legate a voci descrittive          *
      *        *-------------------------------------------------------*
           05  w-det-spe-zvf.
               10  w-det-spe-zvf-ctr      pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Work per Agg castelletto iva                          *
      *        *-------------------------------------------------------*
           05  w-agg-cst-iva.
               10  w-agg-cst-iva-tip      pic  x(01)                  .
               10  w-agg-cst-iva-coi      pic  9(05)                  .
               10  w-agg-cst-iva-imp      pic s9(11)                  .
               10  w-agg-cst-iva-ctr      pic  9(02)                  .
               10  w-agg-cst-iva-tot      pic s9(11)                  .
               10  w-agg-cst-iva-max      pic  9(02)                  .
               10  w-agg-cst-iva-wpa      pic s9(11)                  .
               10  w-agg-cst-iva-s18      pic s9(18)                  .
               10  w-agg-cst-iva-s11      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Agg castelletto contropartite                *
      *        *-------------------------------------------------------*
           05  w-agg-cst-ctp.
               10  w-agg-cst-ctp-tip      pic  x(01)                  .
               10  w-agg-cst-ctp-cod      pic  9(07)                  .
               10  w-agg-cst-ctp-imp      pic s9(11)                  .
               10  w-agg-cst-ctp-ctr      pic  9(02)                  .
               10  w-agg-cst-ctp-tot      pic s9(11)                  .
               10  w-agg-cst-ctp-max      pic  9(02)                  .
               10  w-agg-cst-ctp-wpa      pic s9(11)                  .
               10  w-agg-cst-ctp-s18      pic s9(18)                  .
               10  w-agg-cst-ctp-s11      pic s9(11)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione imposta in base  *
      *    * ad un imponibile                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Att                               *
      *    *-----------------------------------------------------------*
       01  w-att.
      *        *-------------------------------------------------------*
      *        * Work per Att lotto di fatturazione                    *
      *        *-------------------------------------------------------*
           05  w-att-fti-lfd.
               10  w-att-fti-lfd-flg      pic  x(01)                  .
               10  w-att-fti-lfd-saa      pic  9(03)                  .
               10  w-att-fti-lfd-num      pic  9(07)                  .
               10  w-att-fti-lfd-wnu.
                   15  w-att-fti-lfd-wsa  pic  9(03)                  .
                   15  w-att-fti-lfd-wpr  pic  9(04)                  .

      *    *===========================================================*
      *    * Work per subroutines di Compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per Compattamento castelletto iva                *
      *        *-------------------------------------------------------*
           05  w-cmp-cst-iva.
               10  w-cmp-cst-iva-ctr      pic  9(02)                  .
               10  w-cmp-cst-iva-num      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Compattamento castelletto contropartite      *
      *        *-------------------------------------------------------*
           05  w-cmp-cst-ctp.
               10  w-cmp-cst-ctp-ctr      pic  9(02)                  .
               10  w-cmp-cst-ctp-num      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per calcolo prezzo netto                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per prezzo sottoposto a legame valutario        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work per routine dec-tip-rig-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-dec-tip-rig.
      *        *-------------------------------------------------------*
      *        * Input                                                 *
      *        *-------------------------------------------------------*
           05  w-dec-tip-rig-str.
               10  w-dec-tip-rig-chr      occurs 05
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work                                                  *
      *        *-------------------------------------------------------*
           05  w-dec-tip-rig-inx          pic  9(02)                  .
           05  w-dec-tip-rig-pnt          pic  9(02)                  .
           05  w-dec-tip-rig-cod.
               10  w-dec-tip-rig-num      occurs 03
                                          pic  9(01)                  .
           05  w-dec-tip-rig-c01          pic  9(02)                  .
           05  w-dec-tip-rig-c02          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Output                                                *
      *        *-------------------------------------------------------*
           05  w-dec-tip-rig-tpr          pic  x(01)                  .
           05  w-dec-tip-rig-tfu          pic  x(01)                  .
           05  w-dec-tip-rig-ast          pic  9(03)                  .
           05  w-dec-tip-rig-cac          pic  9(03)                  .
           05  w-dec-tip-rig-tot          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per calcolo tabella scadenze                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dtblscd0.dtw"                   .

      *    *===========================================================*
      *    * Work area per totali fattura                              *
      *    *-----------------------------------------------------------*
       01  w-tot.
           05  w-tot-cdc-vpf              pic  9(06)v9(05)            .
           05  w-tot-pvf-age              pic  9(11)                  .
           05  w-tot-pag-act              pic  9(09)                  .
           05  w-tot-tot-rig occurs 09    pic s9(11)                  .
           05  w-tot-tot-scc              pic s9(11)                  .
           05  w-tot-per-scc              pic  9(02)v9(01)            .
           05  w-tot-tot-scp              pic s9(11)                  .
           05  w-tot-per-scp              pic  9(02)v9(01)            .
           05  w-tot-spe     occurs 06                                .
               10  w-tot-spe-snx          pic  9(01)                  .
               10  w-tot-spe-per          pic  9(02)v9(01)            .
               10  w-tot-spe-imp          pic s9(09)                  .
           05  w-tot-tot-sia              pic s9(09)                  .
           05  w-tot-tot-sic              pic s9(09)                  .
           05  w-tot-tot-spb              pic s9(09)                  .
           05  w-tot-tot-lor              pic s9(11)                  .
           05  w-tot-tot-nsc              pic s9(11)                  .
           05  w-tot-tot-nsf              pic s9(11)                  .
           05  w-tot-tot-spe              pic s9(11)                  .
           05  w-tot-tot-net              pic s9(11)                  .
           05  w-tot-tot-ibl              pic s9(11)                  .
           05  w-tot-tot-imp              pic s9(11)                  .
           05  w-tot-tot-doc              pic s9(11)                  .
           05  w-tot-iva-cst.
               10  w-tot-iva-ele          pic  9(02)                  .
               10  w-tot-iva-rig occurs 12.
                   15  w-tot-iva-cod      pic  9(05)                  .
                   15  w-tot-iva-ibl      pic s9(11)                  .
                   15  w-tot-iva-imp      pic s9(11)                  .
           05  w-tot-ctp-cst.
               10  w-tot-ctp-ele          pic  9(02)                  .
               10  w-tot-ctp-rig occurs 20.
                   15  w-tot-ctp-cod      pic  9(07)                  .
                   15  w-tot-ctp-imp      pic s9(11)                  .
           05  w-tot-scd-cst.
               10  w-tot-scd-ele          pic  9(02)                  .
               10  w-tot-scd-tbl occurs 96.
                   15  w-tot-scd-tip      pic  9(02)                  .
                   15  w-tot-scd-dat      pic  9(07)                  .
                   15  w-tot-scd-cau      pic  9(03)                  .
                   15  w-tot-scd-imp      pic s9(11)                  .

      *    *===========================================================*
      *    * Work per bufferizzazione valori contenuti nella prima bol-*
      *    * la incontrata utili per tutta la fattura riepilogativa    *
      *    *-----------------------------------------------------------*
       01  w-vpb.
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Cliente                           *
      *        *-------------------------------------------------------*
           05  w-vpb-cod-cli              pic  9(07)                  .
           05  w-vpb-cod-lng              pic  x(03)                  .
           05  w-vpb-dpz-cli              pic  x(04)                  .
           05  w-vpb-tas-ivc              pic  9(05)                  .
           05  w-vpb-ass-iva              pic  9(05)                  .
           05  w-vpb-flg-rfp              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Cliente per fatturazione          *
      *        *-------------------------------------------------------*
           05  w-vpb-tip-frn              pic  9(02)                  .
           05  w-vpb-cli-plf              pic  9(07)                  .
           05  w-vpb-dpc-plf              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Tipo movimento per fatturazione   *
      *        *-------------------------------------------------------*
           05  w-vpb-tmo-ftr              pic  x(05)                  .
           05  w-vpb-tip-doc              pic  9(02)                  .
           05  w-vpb-org-doc              pic  9(02)                  .
           05  w-vpb-num-giv              pic  9(02)                  .
           05  w-vpb-sgl-num              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Attributi valuta per fatturazione *
      *        *-------------------------------------------------------*
           05  w-vpb-sgl-vpf              pic  x(03)                  .
           05  w-vpb-dec-vpf              pic  9(01)                  .
           05  w-vpb-tdc-vpf              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Altri valori testata              *
      *        *-------------------------------------------------------*
           05  w-vpb-ctp-ven              pic  9(07)                  .
           05  w-vpb-inl-dcm              pic  9(02)                  .
           05  w-vpb-inl-pgt              pic  9(02)                  .
           05  w-vpb-cod-lst              pic  x(03)                  .
           05  w-vpb-csr-aac              pic  9(05)                  .
           05  w-vpb-psr-aac occurs 05    pic  9(02)v9(01)            .
           05  w-vpb-csc-aac              pic  9(05)                  .
           05  w-vpb-psc-aac              pic  9(02)v9(01)            .
           05  w-vpb-cpv-aac              pic  9(05)                  .
           05  w-vpb-ppv-aac occurs 03    pic  9(02)v9(01)            .
           05  w-vpb-voc-des occurs 06    pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Agente ed intermediario           *
      *        *-------------------------------------------------------*
           05  w-vpb-cod-age              pic  9(07)                  .
           05  w-vpb-fsp-doc              pic  9(02)                  .
           05  w-vpb-tip-vpa              pic  9(02)                  .
           05  w-vpb-cpv-aaa              pic  9(05)                  .
           05  w-vpb-ppv-aaa occurs 03    pic  9(02)v9(01)            .
           05  w-vpb-cod-ime              pic  9(07)                  .
           05  w-vpb-pvf-ime              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Pagamento                         *
      *        *-------------------------------------------------------*
           05  w-vpb-cod-abi              pic  9(05)                  .
           05  w-vpb-cod-cab              pic  9(05)                  .
           05  w-vpb-ccc-app              pic  x(12)                  .
           05  w-vpb-nos-ban              pic  x(10)                  .
           05  w-vpb-nos-ccp              pic  x(10)                  .
           05  w-vpb-pag-qaf              pic  9(09)                  .
           05  w-vpb-pag-dsm              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Forma di pagamento                *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-fop.
               10  w-vpb-cod-fop          pic  9(07)                  .
               10  w-vpb-scp-aap          pic  9(02)v9(01)            .
               10  w-vpb-cpg-ass occurs 3.
                   15  w-vpb-cod-pag      pic  9(07)                  .
                   15  w-vpb-tip-amm      pic  9(02)                  .
                   15  w-vpb-per-toi      pic  9(02)v9(01)            .
                   15  w-vpb-dim-act      pic  9(02)                  .
                   15  w-vpb-tip-pag      pic  9(02)                  .
                   15  w-vpb-num-sca      pic  9(02)                  .
                   15  w-vpb-dec-prs      pic  9(02)                  .
                   15  w-vpb-dap-mes      pic  9(02)                  .
                   15  w-vpb-dap-gio      pic  9(02)                  .
                   15  w-vpb-ggg-int      pic  9(02)                  .
                   15  w-vpb-tip-scm      pic  9(02)                  .
                   15  w-vpb-gio-scm      pic  9(02)                  .
                   15  w-vpb-snx-prs      pic  x(01)                  .
                   15  w-vpb-cau-cge      pic  9(03)                  .
                   15  w-vpb-stc-cge      pic  9(07)                  .
               10  w-vpb-tip-esm          pic  9(02)                  .
               10  w-vpb-ggg-alt          pic  9(02)                  .
               10  w-vpb-mmm-e01          pic  9(02)                  .
               10  w-vpb-mmm-e02          pic  9(02)                  .
               10  w-vpb-snx-cts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Sconto in chiusura                *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-scc.
               10  w-vpb-civ-scc          pic  9(05)                  .
               10  w-vpb-ccp-scc          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Sconto pagamento                  *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-scp.
               10  w-vpb-civ-scp          pic  9(05)                  .
               10  w-vpb-ccp-scp          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese in fattura                  *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-spe occurs 06.
               10  w-vpb-spe-mad          pic  9(01)                  .
               10  w-vpb-spe-ibl          pic  9(02)                  .
               10  w-vpb-spe-ibt.
                   15  w-vpb-spe-ibx      occurs 09
                                          pic  x(01)                  .
               10  w-vpb-spe-civ          pic  9(05)                  .
               10  w-vpb-spe-ccp          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese incasso                     *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-spi.
               10  w-vpb-add-spi          pic  x(03)                  .
               10  w-vpb-civ-spi          pic  9(05)                  .
               10  w-vpb-ccp-spi          pic  9(07)                  .
               10  w-vpb-eit-spi          pic  9(01)                  .
               10  w-vpb-tbl-spi occurs 3                             .
                   15  w-vpb-tpg-spi      pic  9(02)                  .
                   15  w-vpb-tfu-spi      pic  9(01)                  .
                   15  w-vpb-amm-spi      pic  9(07)                  .
                   15  w-vpb-per-spi      pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese bollo                       *
      *        *-------------------------------------------------------*
           05  w-vpb-ass-spb.
               10  w-vpb-add-spb          pic  x(03)                  .
               10  w-vpb-civ-spb          pic  9(05)                  .
               10  w-vpb-ccp-spb          pic  9(07)                  .
               10  w-vpb-eit-spb          pic  9(01)                  .
               10  w-vpb-tbl-spb occurs 3                             .
                   15  w-vpb-tpg-spb      pic  9(02)                  .
                   15  w-vpb-tau-spb      pic  9(02)                  .
                   15  w-vpb-per-spb      pic  9(02)v9(01)            .
                   15  w-vpb-tbs-spb.
                       20  w-vpb-tbe-spb occurs 10.
                           25  w-vpb-tbe-scg
                                          pic  9(11)                  .
                           25  w-vpb-tbe-asc
                                          pic  9(11)                  .
                           25  w-vpb-tbe-psc
                                          pic  9(02)v9(01)            .
                   15  w-vpb-tet-spb      pic  9(11)                  .
                   15  w-vpb-min-spb      pic  9(11)                  .
                   15  w-vpb-max-spb      pic  9(11)                  .
                   15  w-vpb-tar-spb      pic  9(02)                  .
                   15  w-vpb-var-spb      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per bufferizzazioni                                  *
      *    *-----------------------------------------------------------*
       01  w-buf.
      *        *-------------------------------------------------------*
      *        * Work per Buf protocolli bolle in fattura              *
      *        *                                                       *
      *        * N.B.: eventuali variazioni alla definizione di que-   *
      *        *       sto buffer devono essere riportate anche nella  *
      *        *       definizione del record w-rfa utilizzato per la  *
      *        *       scrittura del file di appoggio sequenziale      *
      *        *                                                       *
      *        *-------------------------------------------------------*
           05  w-buf-pbf.
               10   w-buf-pbf-num-ele     pic  9(03)                  .
               10   w-buf-pbf-max-ele     pic  9(03) value 200        .
               10   w-buf-pbf-tbl-ele.
                    15  w-buf-pbf-sng-ele occurs 200                  .
                        20  w-buf-pbf-num-prt
                                          pic  9(11)                  .

      *    *===========================================================*
      *    * Work per routine tst-lfd-pre-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-tst-lfd-pre.
           05  w-tst-lfd-pre-flg          pic  x(01)                  .
           05  w-tst-lfd-pre-saa          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine pre-fil-fde-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-pre-fil-fde.
           05  w-pre-fil-fde-uno          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per routine pre-srt-fil-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-pre-srt-fil.
           05  w-pre-srt-fil-ctr          pic  9(05)                  .
           05  w-pre-srt-fil-pco          pic  9(11)                  .
           05  w-pre-srt-fil-x80          pic  x(80)                  .

      *    *===========================================================*
      *    * Work per routine tst-rot-fat-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-tst-rot-fat.
           05  w-tst-rot-fat-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per routine nor-tot-fat-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-nor-tot-fat.
           05  w-nor-tot-fat-ctr          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine rou-ini-fat-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-rou-ini-fat.
           05  w-rou-ini-fat-ctr          pic  9(03)                  .
           05  w-rou-ini-fat-inx          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine cmp-rec-fit-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-cmp-rec-fit.
           05  w-cmp-rec-fit-ctr          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine cmp-rec-fir-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-cmp-rec-fir.
           05  w-cmp-rec-fir-wtr          pic  x(05)                  .
           05  w-cmp-rec-fir-wtr-r redefines
               w-cmp-rec-fir-wtr.
               10  w-cmp-rec-fir-wtp      pic  x(01)                  .
               10  w-cmp-rec-fir-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .

      *    *===========================================================*
      *    * Work per routine scr-fil-app-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-scr-fil-app.
           05  w-scr-fil-app-ctr          pic  9(03)                  .
           05  w-scr-fil-app-prg          pic  9(05)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pfat4000       *
      *    *                                                           *
      *    * L'area comprende :                                        *
      *    *                                                           *
      *    *  - 'rr'        : Work-area richieste per stampa           *
      *    *  - 'w-rfa'     : Work-area per file di appoggio           *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/pfat4000.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per movimento di contabilita'       *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza dell'azienda *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento per la   *
      *    * fatturazione                                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione provvigioni      *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Se uscita                                       *
      *              *-------------------------------------------------*
           if        w-cnt-sel-prm-stp    =    spaces
                     go to main-450.
      *                  *---------------------------------------------*
      *                  * Ripristino progressivo lotto di fatturazio- *
      *                  * ne differita                                *
      *                  *---------------------------------------------*
           perform   rip-fti-lfd-000      thru rip-fti-lfd-999        .
      *                  *---------------------------------------------*
      *                  * A close files                               *
      *                  *---------------------------------------------*
           go to     main-750.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione programma in foreground              *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
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
      *              *-------------------------------------------------*
      *              * Flag di eventuale visualizzazione forzata       *
      *              *-------------------------------------------------*
           move      s-sts                to   w-cnt-mfu-vis-sgr      .
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
      *              * Scrittura record richieste per foreground       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record richieste           *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * richieste                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-stu-rrr-pnt-stu      .
       exe-pgm-frg-200.
           move      spaces               to   w-stu-rrr-255-byt      .
           move      w-stu-rrr-pnt-stu    to   w-stu-rrr-sav-pnt      .
           unstring  rr                   into w-stu-rrr-255-byt
                                  with pointer w-stu-rrr-pnt-stu      .
           move      w-stu-rrr-255-byt    to   b-chr                  .
           if        w-stu-rrr-pnt-stu    =    w-stu-rrr-sav-pnt
                     go to exe-pgm-frg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           go to     exe-pgm-frg-200.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record richieste             *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione del File di appoggio records per   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   pre-fil-app-000      thru pre-fil-app-999        .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di ipc : decimali quantita' *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dec-qta"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-edt-qta-inc-din    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Lancio del programma di esecuzione              *
      *              *-------------------------------------------------*
           call      i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del programma di esecuzione              *
      *              *-------------------------------------------------*
           cancel    i-exe-pat                                        .
      *              *-------------------------------------------------*
      *              * Cancel del modulo "mbckgr"                      *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
      *              *-------------------------------------------------*
      *              * Visualizzazione eventuali errori di esecuzione  *
      *              *-------------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-020.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Programma non eseguibile dall'utente !            
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-pre-exe-pgm      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-exe-pgm-999.
       pre-exe-pgm-020.
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
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Referenza per eventuale dipendenza di default   *
      *              *-------------------------------------------------*
           perform   ref-dpz-def-000      thru ref-dpz-def-999        .
       pre-exe-pgm-100.
      *              *-------------------------------------------------*
      *              * Determinazione codici dipendenze per l'azienda  *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      "DA"                 to   w-dpz-tip-ope          .
           move      s-ter                to   w-dpz-ide-ter          .
           move      s-ute                to   w-dpz-ide-ute          .
           move      s-azi                to   w-dpz-ide-azi          .
           move      s-sap                to   w-dpz-ide-sap          .
           move      s-arg                to   w-dpz-ide-arg          .
           move      s-set                to   w-dpz-ide-set          .
           move      s-fas                to   w-dpz-ide-fas          .
           move      i-ide-des            to   w-dpz-ide-des          .
           move      s-pro                to   w-dpz-ide-pro          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
      *              *-------------------------------------------------*
      *              * Se zero dipendenze : errore ed uscita           *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        >    zero
                     go to pre-exe-pgm-120.
           move      "EN"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
           move      "#"                  to   w-cnt-pre-exe-pgm      .
           go to     pre-exe-pgm-999.
       pre-exe-pgm-120.
      *              *-------------------------------------------------*
      *              * Test su esito lettura referenza per dipendenza  *
      *              * di default                                      *
      *              *-------------------------------------------------*
           if        w-ref-dpz-def-cod    =    zero
                     go to pre-exe-pgm-140.
           move      w-ref-dpz-def-cod    to   rr-dpz-emi             .
           go to     pre-exe-pgm-200.
       pre-exe-pgm-140.
      *              *-------------------------------------------------*
      *              * Selezione codice dipendenza per il programma    *
      *              *-------------------------------------------------*
           move      "SD"                 to   w-dpz-tip-ope          .
           call      "pgm/azi/prg/obj/pazi000d"
                                         using w-dpz                  .
           cancel    "pgm/azi/prg/obj/pazi000d"                       .
       pre-exe-pgm-180.
      *              *-------------------------------------------------*
      *              * Se scelta non effettuata : uscita               *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Altrimenti memorizzazione in campo testata      *
      *              *-------------------------------------------------*
           move      w-dpz-cod-prg        to   rr-dpz-emi             .
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tabella [zsf] : Spese per la fatturazione e *
      *                  *                 referenze per sconti a      *
      *                  *                 piede fattura               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Open tabella [zsf]                      *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Spese per la fatturazione                   *
      *                  *---------------------------------------------*
           perform   prs-spe-fat-000      thru prs-spe-fat-999        .
      *                  *---------------------------------------------*
      *                  * Tipo imponibile per sconto pagamento        *
      *                  *---------------------------------------------*
           perform   prs-tim-scp-000      thru prs-tim-scp-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione lettere d'intenti a clienti  *
      *                  *---------------------------------------------*
           perform   prs-snx-lic-000      thru prs-snx-lic-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione agenti attiva                *
      *                  *---------------------------------------------*
           perform   prs-age-snx-000      thru prs-age-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione portafoglio attiva           *
      *                  *---------------------------------------------*
           perform   prs-gep-snx-000      thru prs-gep-snx-999        .
      *                  *---------------------------------------------*
      *                  * Gestione portafoglio                        *
      *                  *---------------------------------------------*
           perform   prs-ges-gep-000      thru prs-ges-gep-999        .
      *                  *---------------------------------------------*
      *                  * Tipo accettazione spese per la fatturazione *
      *                  *---------------------------------------------*
           perform   prs-tac-spf-000      thru prs-tac-spf-999        .
      *                  *---------------------------------------------*
      *                  * Modalita' di utilizzo dei codici ABI e CAB  *
      *                  * in emissione scadenze                       *
      *                  *---------------------------------------------*
           perform   prs-dcc-abi-000      thru prs-dcc-abi-999        .
      *                  *---------------------------------------------*
      *                  * Tipo trattamento assoggettamento iva del    *
      *                  * del cliente                                 *
      *                  *---------------------------------------------*
           perform   prs-trt-aic-000      thru prs-trt-aic-999        .
      *                  *---------------------------------------------*
      *                  * Si/no fattura in attesa di verifica         *
      *                  *---------------------------------------------*
           perform   prs-snx-fav-000      thru prs-snx-fav-999        .
       pre-exe-pgm-300.
      *              *-------------------------------------------------*
      *              * Lettura referenze                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Referenze per lo sconto in chiusura         *
      *                  *---------------------------------------------*
           perform   ref-sco-chi-000      thru ref-sco-chi-999        .
      *                  *---------------------------------------------*
      *                  * Referenze per lo sconto pagamento           *
      *                  *---------------------------------------------*
           perform   ref-sco-pag-000      thru ref-sco-pag-999        .
      *                  *---------------------------------------------*
      *                  * Close tabella [zsf]                         *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
       pre-exe-pgm-600.
      *              *-------------------------------------------------*
      *              * Test su lotto di fatturazione precedente        *
      *              *-------------------------------------------------*
           perform   tst-lfd-pre-000      thru tst-lfd-pre-999        .
           if        w-tst-lfd-pre-flg    not  = spaces
                     move  "#"            to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alle spese per   *
      *    * la fatturazione                                           *
      *    *-----------------------------------------------------------*
       prs-spe-fat-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione : Tipo accettazione   *
      *              * spesa                                           *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tac-spf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-spe-fat-tac
           else      move  s-num          to   w-prs-spe-fat-tac      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-tac    not  = 00 and
                     w-prs-spe-fat-tac    not  = 01
                     move  00             to   w-prs-spe-fat-tac      .
      *              *-------------------------------------------------*
      *              * Numero di spese personalizzate caricate : zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-spe-fat-nst      .
       prs-spe-fat-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura spese da 1 a 6 con carica- *
      *              * mento in tabella delle spese trovate            *
      *              *-------------------------------------------------*
       prs-spe-fat-250.
      *                  *---------------------------------------------*
      *                  * Indice 1..6 a zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-spe-fat-i01      .
       prs-spe-fat-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice 1..6                      *
      *                  *---------------------------------------------*
           add       1                    to   w-prs-spe-fat-i01      .
      *                  *---------------------------------------------*
      *                  * Se oltre il max : a chiusura                *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-i01    >    6
                     go to prs-spe-fat-900.
       prs-spe-fat-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura della tabella per la   *
      *                  * spesa corrispondente all'indice, e con co-  *
      *                  * dice lingua per Italia                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-prs-spe-fat-i01    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-spe-fat-500
           else      go to prs-spe-fat-700.
       prs-spe-fat-500.
      *                  *---------------------------------------------*
      *                  * Se record spesa esistente                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo di correttezza, e se non e'   *
      *                      * superato si ricicla a spesa successiva  *
      *                      *-----------------------------------------*
           if        rf-zsf-tfu-spe       <    01 or
                     rf-zsf-tfu-spe       >    05
                     go to prs-spe-fat-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione spesa                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero di spese persona- *
      *                          * lizzate caricate                    *
      *                          *-------------------------------------*
           add       1                    to   w-prs-spe-fat-nst      .
      *                          *-------------------------------------*
      *                          * Numero spesa                        *
      *                          *-------------------------------------*
           move      w-prs-spe-fat-i01    to   w-prs-spe-fat-npt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-prs-spe-fat-dve
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo funzionamento spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-tfu-spe       to   w-prs-spe-fat-tfs
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Percentuale per la spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-per-spe       to   w-prs-spe-fat-per
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo di imponibile per la spesa     *
      *                          *-------------------------------------*
           move      rf-zsf-ibl-spe       to   w-prs-spe-fat-ibl
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Totalizzatori imponibile spesa      *
      *                          *-------------------------------------*
           move      rf-zsf-ibt-spe       to   w-prs-spe-fat-ibt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Importo spesa                       *
      *                          *-------------------------------------*
           move      rf-zsf-imp-spe       to   w-prs-spe-fat-imp
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-prs-spe-fat-civ
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-prs-spe-fat-ccp
                                              (w-prs-spe-fat-nst)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su spesa successiva             *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-700.
      *                  *---------------------------------------------*
      *                  * Se record spesa non esistente               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a spesa successiva              *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a spesa successiva                  *
      *                  *---------------------------------------------*
           go to     prs-spe-fat-300.
       prs-spe-fat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-spe-fat-999.
       prs-spe-fat-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione relativa al tipo di im-   *
      *    * ponibile per lo sconto pagamento                          *
      *    *-----------------------------------------------------------*
       prs-tim-scp-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione : Tipo accettazione   *
      *              * spesa                                           *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tim-scp]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     move  "C"            to   w-prs-tim-scp-tip
           else      move  s-alf          to   w-prs-tim-scp-tip      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-tim-scp-tip    not  = "C" and
                     w-prs-tim-scp-tip    not  = "N" and
                     w-prs-tim-scp-tip    not  = "A"
                     move  "C"            to   w-prs-tim-scp-tip      .
       prs-tim-scp-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione lettere d'in-  *
      *    *                             tenti clienti                 *
      *    *-----------------------------------------------------------*
       prs-snx-lic-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/cge[snx-lic]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-lic
           else      move  spaces         to   w-prs-snx-lic          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-lic        not   = "S"
                     move  "N"            to   w-prs-snx-lic          .
       prs-snx-lic-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione agenti attiva  *
      *    *-----------------------------------------------------------*
       prs-age-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-age-snx
           else      move  spaces         to   w-prs-age-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-age-snx        not  = "S" and
                     w-prs-age-snx        not  = "N"
                     move  "N"            to   w-prs-age-snx          .
       prs-age-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione portafoglio at-*
      *    *                             tiva                          *
      *    *-----------------------------------------------------------*
       prs-gep-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-gep-snx
           else      move  spaces         to   w-prs-gep-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-gep-snx        =    "S" or
                     w-prs-gep-snx        =    "N"
                     go to prs-gep-snx-999.
           move      "N"                  to   w-prs-gep-snx          .
       prs-gep-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni di gestione portafoglio   *
      *    *-----------------------------------------------------------*
       prs-ges-gep-000.
      *              *-------------------------------------------------*
      *              * Open tabella [zop]                              *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       prs-ges-gep-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-gep-ctr-001      .
       prs-ges-gep-120.
           add       1                    to   w-prs-gep-ctr-001      .
           if        w-prs-gep-ctr-001    >    11
                     go to prs-ges-gep-140.
           move      "N"                  to   w-prs-gep-snx-prs
                                              (w-prs-gep-ctr-001)     .
           move      zero                 to   w-prs-gep-cau-cge
                                              (w-prs-gep-ctr-001)     .
           move      zero                 to   w-prs-gep-stc-cge
                                              (w-prs-gep-ctr-001)     .
           go to     prs-ges-gep-120.
       prs-ges-gep-140.
      *              *-------------------------------------------------*
      *              * Start su file [zop]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      101                  to   rf-zop-cod-top         .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prs-ges-gep-900.
       prs-ges-gep-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [zop]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *              *-------------------------------------------------*
      *              * Test se 'at end'                                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prs-ges-gep-900.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-zop-cod-top       >    111
                     go to prs-ges-gep-900.
      *              *-------------------------------------------------*
      *              * Codice tipo operazione in comodo ridefinito     *
      *              *-------------------------------------------------*
           move      rf-zop-cod-top       to   w-prs-gep-wrk-top      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento in tabella             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-prs-gep-snx-prs
                                              (w-prs-gep-wto-tpg)     .
           move      rf-zop-cau-cge       to   w-prs-gep-cau-cge
                                              (w-prs-gep-wto-tpg)     .
           move      rf-zop-stc-cge       to   w-prs-gep-stc-cge
                                              (w-prs-gep-wto-tpg)     .
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale [zop]            *
      *              *-------------------------------------------------*
           go to     prs-ges-gep-200.
       prs-ges-gep-900.
      *              *-------------------------------------------------*
      *              * Close tabella [zop]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       prs-ges-gep-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo accettazione spese per   *
      *    * la fatturazione                                           *
      *    *-----------------------------------------------------------*
       prs-tac-spf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tac-spf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-tac-spf
           else      move  s-num          to   w-prs-tac-spf          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-tac-spf        not  = 00 and
                     w-prs-tac-spf        not  = 01
                     move  00             to   w-prs-tac-spf          .
       prs-tac-spf-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Modalita' di utilizzo dei     *
      *    *                             codici ABI e CAB in emissione *
      *    *                             scadenze                      *
      *    *-----------------------------------------------------------*
       prs-dcc-abi-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[mdu-abi]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-dcc-abi
           else      move  zero           to   w-prs-dcc-abi          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dcc-abi        =    00  or
                     w-prs-dcc-abi        =    01
                     go to prs-dcc-abi-999.
           move      00                   to   w-prs-dcc-abi          .
       prs-dcc-abi-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo trattamento assoggetta-  *
      *    *                             mento iva del cliente         *
      *    *-----------------------------------------------------------*
       prs-trt-aic-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[trt-aic]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-trt-aic
           else      move  zero           to   w-prs-trt-aic          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-trt-aic        =    00  or
                     w-prs-trt-aic        =    01
                     go to prs-trt-aic-999.
           move      00                   to   w-prs-trt-aic          .
       prs-trt-aic-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no fattura in attesa di    *
      *    *                             verifica                      *
      *    *-----------------------------------------------------------*
       prs-snx-fav-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[snx-fav]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-fav
           else      move  spaces         to   w-prs-snx-fav          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-fav        not   = "S"
                     move  "N"            to   w-prs-snx-fav          .
       prs-snx-fav-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative allo sconto in chiusura  *
      *    *-----------------------------------------------------------*
       ref-sco-chi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-sco-chi-des      .
           move      zero                 to   w-ref-sco-chi-civ      .
           move      zero                 to   w-ref-sco-chi-ccp      .
       ref-sco-chi-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della tabella per sconto   *
      *              * in chiusura con codice lingua per l'Italia      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      101                  to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ref-sco-chi-500
           else      go to ref-sco-chi-700.
       ref-sco-chi-500.
      *                  *---------------------------------------------*
      *                  * Se record sconto esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori letti            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-ref-sco-chi-des      .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-ref-sco-chi-civ      .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-ref-sco-chi-ccp      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-chi-999.
       ref-sco-chi-700.
      *                  *---------------------------------------------*
      *                  * Se record sconto non esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Default per la descrizione sconto in    *
      *                      * chiusura                                *
      *                      *-----------------------------------------*
           move      "Sconto incondizionato    "
                                          to   w-ref-sco-chi-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-chi-999.
       ref-sco-chi-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative allo sconto pagamento    *
      *    *-----------------------------------------------------------*
       ref-sco-pag-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-sco-pag-des      .
           move      zero                 to   w-ref-sco-pag-civ      .
           move      zero                 to   w-ref-sco-pag-ccp      .
       ref-sco-pag-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della tabella per sconto   *
      *              * pagamento con codice lingua per l'Italia        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      102                  to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ref-sco-pag-500
           else      go to ref-sco-pag-700.
       ref-sco-pag-500.
      *                  *---------------------------------------------*
      *                  * Se record sconto pagamento esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori letti            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-ref-sco-pag-des      .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-ref-sco-pag-civ      .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-ref-sco-pag-ccp      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-pag-999.
       ref-sco-pag-700.
      *                  *---------------------------------------------*
      *                  * Se record sconto pagamento non esistente    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Default per la descrizione sconto       *
      *                      * pagamento                               *
      *                      *-----------------------------------------*
           move      "Sconto pagamento         "
                                          to   w-ref-sco-pag-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-pag-999.
       ref-sco-pag-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenza per dipendenza di default         *
      *    *-----------------------------------------------------------*
       ref-dpz-def-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-dpz-def-cod      .
      *              *-------------------------------------------------*
      *              * Lettura referenza                               *
      *              *-------------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      "pgm/fat[dpz-def]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ref-dpz-def-999.
      *              *-------------------------------------------------*
      *              * Valore referenza in work-area                   *
      *              *-------------------------------------------------*
           move      s-num                to   w-ref-dpz-def-cod      .
       ref-dpz-def-999.
           exit.

      *    *===========================================================*
      *    * Test su lotto fatturazione differita precedente           *
      *    *-----------------------------------------------------------*
       tst-lfd-pre-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-tst-lfd-pre-flg      .
      *              *-------------------------------------------------*
      *              * Open numerazione [ftilfd]                       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Determinazione secolo/anno attuali              *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-tst-lfd-pre-saa      .
       tst-lfd-pre-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [ftilfd]                 *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Get record [ftilfd]                             *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      w-tst-lfd-pre-saa    to   rn-fti-lfd-scl-ann     .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to tst-lfd-pre-300.
       tst-lfd-pre-200.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Rilascio record                             *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'operazione                 *
      *                  *---------------------------------------------*
           go to     tst-lfd-pre-100.
       tst-lfd-pre-300.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se il lotto precedente e' stato termi- *
      *                  * nato correttamente                          *
      *                  *---------------------------------------------*
           if        rn-fti-lfd-sts-avz   =    spaces
                     go to tst-lfd-pre-400.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Attenzione: il programma e' gia' in uso presso un 
      -              "altro terminale"
                                          to   w-err-box-err-msg      .
           move      "            oppure si e' inaspettatamente interrot
      -              "to, nel qual"
                                          to   w-err-box-err-m02      .
           move      "            caso e' opportuno interpellare l'assis
      -              "tenza software."
                                          to   w-err-box-err-m03      .
           perform   box-msg-e03-000      thru box-msg-e03-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-tst-lfd-pre-flg      .
       tst-lfd-pre-400.
      *              *-------------------------------------------------*
      *              * Release record [ftilfd]                         *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Close file [ftilfd]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
       tst-lfd-pre-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del- *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-opn-000  thru cod-cod-dpz-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per la  *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-opn-000  thru cod-des-zfi-opn-999    .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [bix]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [zac]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *              *-------------------------------------------------*
      *              * [ftilfd]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
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
      *              * [lic]                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione su gestione let-  *
      *                  * tere d'intento clienti attiva               *
      *                  *---------------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to rou-opn-fls-120.
      *                  *---------------------------------------------*
      *                  * Open file                                   *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       rou-opn-fls-120.
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
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * [zin]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * [zbo]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
      *              *-------------------------------------------------*
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
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
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
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
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione imposta           *
      *              *-------------------------------------------------*
           perform   det-imp-iva-opn-000  thru det-imp-iva-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione importo in riga   *
      *              *-------------------------------------------------*
           perform   det-imp-ven-opn-000  thru det-imp-ven-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione provvigioni       *
      *              *-------------------------------------------------*
           perform   det-pvg-age-opn-000  thru det-pvg-age-opn-999    .
      *              *-------------------------------------------------*
      *              * Open file relative di appoggio [rlt]            *
      *              *-------------------------------------------------*
           perform   fil-rlt-opn-000      thru fil-rlt-opn-999        .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del-*
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           perform   cod-cod-dpz-cls-000  thru cod-cod-dpz-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   cod-des-zfi-cls-000  thru cod-des-zfi-cls-999    .
       rou-cls-fls-100.
      *              *-------------------------------------------------*
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [bix]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * [fir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
      *              *-------------------------------------------------*
      *              * [fix]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * [zac]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
      *              *-------------------------------------------------*
      *              * [ftilfd]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
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
      *              * Close file [lic]                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione su gestione let-  *
      *                  * tere d'intento clienti attiva               *
      *                  *---------------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to rou-cls-fls-120.
      *                  *---------------------------------------------*
      *                  * Close file                                  *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       rou-cls-fls-120.
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
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * [zin]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * [zbo]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
      *              *-------------------------------------------------*
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
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
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
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
       rou-cls-fls-400.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione imposta          *
      *              *-------------------------------------------------*
           perform   det-imp-iva-cls-000  thru det-imp-iva-cls-999    .
       rou-cls-fls-420.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione importo in riga  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cls-000  thru det-imp-ven-cls-999    .
       rou-cls-fls-440.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione provvigioni      *
      *              *-------------------------------------------------*
           perform   det-pvg-age-cls-000  thru det-pvg-age-cls-999    .
       rou-cls-fls-600.
      *              *-------------------------------------------------*
      *              * Close file relative di appoggio [rlt]           *
      *              *-------------------------------------------------*
           perform   fil-rlt-cls-000      thru fil-rlt-cls-999        .
       rou-cls-fls-999.
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
      *                  * Tipo esecuzione                             *
      *                  *---------------------------------------------*
           perform   acc-tip-exe-000      thru acc-tip-exe-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Data emissione fatture                      *
      *                  *---------------------------------------------*
           perform   acc-dat-emi-000      thru acc-dat-emi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-250.
       acc-ric-sel-350.
      *                  *---------------------------------------------*
      *                  * Data documenti da fatturare minima          *
      *                  *---------------------------------------------*
           perform   acc-dat-ddi-000      thru acc-dat-ddi-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data documenti da fatturare massima         *
      *                  *---------------------------------------------*
           perform   acc-dat-ddf-000      thru acc-dat-ddf-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-350.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Periodicita' di fatturazione                *
      *                  *---------------------------------------------*
           perform   acc-per-fat-000      thru acc-per-fat-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
       acc-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Tipo movimento da ricercare                 *
      *                  *---------------------------------------------*
           perform   acc-cod-tmo-000      thru acc-cod-tmo-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-500.
       acc-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza da fatturare              *
      *                  *---------------------------------------------*
           perform   acc-cod-dpz-000      thru acc-cod-dpz-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-600.
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
           move      "#CNF"               to   v-not                  .
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
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo esecuzione                                 *
      *              *-------------------------------------------------*
           perform   pmt-tip-exe-000      thru pmt-tip-exe-999        .
      *              *-------------------------------------------------*
      *              * Data emissione fatture                          *
      *              *-------------------------------------------------*
           perform   pmt-dat-emi-000      thru pmt-dat-emi-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data emissione fatture          *
      *              *-------------------------------------------------*
           perform   vis-dat-emi-000      thru vis-dat-emi-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Codice cliente                                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Data documenti da fatturare minima              *
      *              *-------------------------------------------------*
           perform   pmt-dat-ddi-000      thru pmt-dat-ddi-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data documenti da fatturare min *
      *              *-------------------------------------------------*
           perform   vis-dat-ddi-000      thru vis-dat-ddi-999        .
      *              *-------------------------------------------------*
      *              * Data documenti da fatturare massima             *
      *              *-------------------------------------------------*
           perform   pmt-dat-ddf-000      thru pmt-dat-ddf-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data documenti da fatturare max *
      *              *-------------------------------------------------*
           perform   vis-dat-ddf-000      thru vis-dat-ddf-999        .
      *              *-------------------------------------------------*
      *              * Periodicita' di fatturazione                    *
      *              *-------------------------------------------------*
           perform   pmt-per-fat-000      thru pmt-per-fat-999        .
      *              *-------------------------------------------------*
      *              * Prompt tipo movimento da ricercare              *
      *              *-------------------------------------------------*
           perform   pmt-cod-tmo-000      thru pmt-cod-tmo-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento da ricercare     *
      *              *-------------------------------------------------*
           perform   vis-cod-tmo-000      thru vis-cod-tmo-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo movimento da ricercare,    *
      *              * descrizione                                     *
      *              *-------------------------------------------------*
           perform   vis-cod-tmo-des-000  thru vis-cod-tmo-des-999    .
      *              *-------------------------------------------------*
      *              * Codice dipendenza da fatturare                  *
      *              *-------------------------------------------------*
           perform   pmt-cod-dpz-000      thru pmt-cod-dpz-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Tipo esecuzione               *
      *    *-----------------------------------------------------------*
       pmt-tip-exe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo esecuzione            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-exe-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data emissione fatture        *
      *    *-----------------------------------------------------------*
       pmt-dat-emi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data emissione fatture     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-emi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice agente                 *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
      *              *-------------------------------------------------*
      *              * Test se prompt da visualizzare                  *
      *              *-------------------------------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to pmt-cod-age-999.
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice cliente                *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data documenti da fatturare   *
      *    * minima                                                    *
      *    *-----------------------------------------------------------*
       pmt-dat-ddi-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Documenti da fatturare dal :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-ddi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Data documenti da fatturare   *
      *    * massima                                                   *
      *    *-----------------------------------------------------------*
       pmt-dat-ddf-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "                        al :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-ddf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Periodicita' di fatturazione  *
      *    *-----------------------------------------------------------*
       pmt-per-fat-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Periodicita' fatturazione  :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-per-fat-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo movimento da ricercare                        *
      *    *-----------------------------------------------------------*
       pmt-cod-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di movimento per la   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  fatturazione differita    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per Codice dipendenza da fatturare*
      *    *-----------------------------------------------------------*
       pmt-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se da visualizzare                         *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        not  > 1
                     go to pmt-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Documenti della dipendenza :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo esecuzione            *
      *    *-----------------------------------------------------------*
       acc-tip-exe-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-exe-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-exe-lun    to   v-car                  .
           move      w-exp-tip-exe-num    to   v-ldt                  .
           move      "SE#"                to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tip-exe-tbl    to   v-txt                  .
           move      rr-tip-exe           to   v-num                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-exe-999.
       acc-tip-exe-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-exe             .
       acc-tip-exe-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = zero
                     go to acc-tip-exe-600.
           if        v-key                =    "UP  "
                     go to acc-tip-exe-600
           else      go to acc-tip-exe-100.
       acc-tip-exe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale normalizzazione data minima do-   *
      *                  * cumenti da fatturare                        *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 2
                     go to acc-tip-exe-800.
           if        rr-dat-ddi           =    zero
                     go to acc-tip-exe-800.
      *                  *---------------------------------------------*
      *                  * Normalizzazione data documenti da fatturare *
      *                  * minima                                      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dat-ddi             .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data documenti da fatturare *
      *                  * minima                                      *
      *                  *---------------------------------------------*
           perform   vis-dat-ddi-000      thru vis-dat-ddi-999        .
       acc-tip-exe-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-exe-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-exe-100.
       acc-tip-exe-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data emissione fatture     *
      *    *-----------------------------------------------------------*
       acc-dat-emi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-emi-100.
      *              *-------------------------------------------------*
      *              * Visualizzazione note operative                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Operare gli opportuni controlli sulle ultime date 
      -              "di emissione dei documenti"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           if        rr-tip-exe           =    1
                     move  ">"            to   v-edm
           else      move  spaces         to   v-edm                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-emi           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-emi-999.
       acc-dat-emi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-emi             .
       acc-dat-emi-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-emi-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [fit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-fit-000      thru fnd-arc-fit-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-dat-emi-100.
       acc-dat-emi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        rr-dat-emi           not  = zero
                     go to acc-dat-emi-600.
           if        v-key                =    "UP  "
                     go to acc-dat-emi-600
           else      go to acc-dat-emi-100.
      *                  *---------------------------------------------*
      *                  * Test su data registrazione minima           *
      *                  *---------------------------------------------*
           move      "DR"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
           if        rr-dat-emi           not  < l-cge-300-dat-reg
                     go to acc-dat-emi-600.
      *                  *---------------------------------------------*
      *                  * Se test non superato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing data minima                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rr-dat-emi           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Composizione stringa                *
      *                          *-------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "La data emissione fatture non puo' essere inferior
      -              "e al "
                                delimited by   size
                     v-edt      delimited by   spaces
                     " !"       delimited by   size
                                          into w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * Visualizzazione messaggio           *
      *                          *-------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-dat-emi-100.
       acc-dat-emi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-emi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-emi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-emi-100.
       acc-dat-emi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Data emissione fatture  *
      *    *-----------------------------------------------------------*
       vis-dat-emi-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-emi           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-emi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Codice agente                              *
      *    *-----------------------------------------------------------*
       acc-cod-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to acc-cod-age-999.
       acc-cod-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-cod-age           to   w-cod-mne-age-cod      .
           move      09                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      09                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
       acc-cod-age-110.
           perform   cod-mne-age-cll-000  thru cod-mne-age-cll-999    .
           if        w-cod-mne-age-ope    =    "F+"
                     go to acc-cod-age-115.
           if        w-cod-mne-age-ope    =    "AC"
                     go to acc-cod-age-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-age-115.
           perform   cod-mne-age-foi-000  thru cod-mne-age-foi-999    .
           go to     acc-cod-age-110.
       acc-cod-age-120.
           move      w-cod-mne-age-cod    to   v-num                  .
       acc-cod-age-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-age-999.
       acc-cod-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-age             .
       acc-cod-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-age-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [age]                          *
      *                  *---------------------------------------------*
           move      rr-cod-age           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione nominativo                   *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     move  "Tutti               "
                                          to   rr-cod-age-nom
           else      move  w-let-arc-age-nom
                                          to   rr-cod-age-nom         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo                  *
      *                  *---------------------------------------------*
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se agente non esistente : a reimpostazione  *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-age-100.
       acc-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente                           *
      *    *-----------------------------------------------------------*
       vis-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      09                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice agente , nominativo              *
      *    *-----------------------------------------------------------*
       vis-cod-age-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-age-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice cliente             *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-sav-cod-cli          .
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto funzione SELEZIONE per scegliere piu' client
      -              "i (fino ad un massimo di 36)  "
                                          to   v-nt1                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli           to   w-cod-mne-dcc-cod      .
           move      10                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      10                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      zero                 to   w-cod-mne-dcc-vln      .
           move      zero                 to   w-cod-mne-dcc-vps      .
           move      zero                 to   w-cod-mne-dcc-lln      .
           move      zero                 to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "SLCT"               to   v-pfk (11)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-cli-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cod-cli-150.
      *              *-------------------------------------------------*
      *              * Cancellazione note operative                    *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-cli             .
       acc-cod-cli-300.
      *              *-------------------------------------------------*
      *              * Se 'Select'                                     *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-cod-cli-400.
      *                  *---------------------------------------------*
      *                  * Select su codici cliente                    *
      *                  *---------------------------------------------*
           perform   acc-cli-esl-000      thru acc-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Verifica se elementi selezionati per atti-  *
      *                  * vazione del segnale di elementi selezionati *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione segnale di selezione    *
      *                      * effettuata                              *
      *                      *-----------------------------------------*
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   w-acc-ser-edd-c01      .
       acc-cod-cli-310.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to acc-cod-cli-320.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to acc-cod-cli-320.
      *                      *-----------------------------------------*
      *                      * Attivazione segnale di selezione effet- *
      *                      * tuata                                   *
      *                      *-----------------------------------------*
           move      1                    to   rr-cod-cli-sns         .
       acc-cod-cli-320.
      *                  *---------------------------------------------*
      *                  * Normalizzazione singolo codice cliente      *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Preparazione visualizzazione elementi sele- *
      *                  * zionati                                     *
      *                  *---------------------------------------------*
           perform   pcs-cli-esl-000      thru pcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codici selezionati          *
      *                  *---------------------------------------------*
           perform   vcs-cli-esl-000      thru vcs-cli-esl-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente : reimpostazione   *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    not  = spaces
                     go to acc-cod-cli-100.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione anagrafica cliente           *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     move  "Tutti               "
                                          to   rr-cod-cli-rag
           else      move  w-let-arc-dcc-rag
                                          to   rr-cod-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-cod-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione anagrafica cliente          *
      *                  *---------------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                  *---------------------------------------------*
      *                  * Se cliente esistente : oltre                *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-600.
      *                  *---------------------------------------------*
      *                  * Se cliente a zero : oltre                   *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    zero
                     go to acc-cod-cli-600.
       acc-cod-cli-500.
      *                  *---------------------------------------------*
      *                  * Se anagrafica commerciale non esistente     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione anagrafica cliente con- *
      *                      * tabile                                  *
      *                      *-----------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione anagrafica cliente      *
      *                      *-----------------------------------------*
           perform   vis-des-cli-000      thru vis-des-cli-999        .
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Anagrafica commerciale del cliente non esistente !
      -              ""
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Reimpostazione                          *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore attuale come precedente : oltre   *
      *                  *---------------------------------------------*
           if        rr-cod-cli           =    w-sav-cod-cli
                     go to acc-cod-cli-800.
      *                  *---------------------------------------------*
      *                  * Se e' solo stato cambiato il codice cli-    *
      *                  * ente : oltre                                *
      *                  *---------------------------------------------*
           if        rr-cod-cli           not  = zero and
                     w-sav-cod-cli        not  = zero
                     go to acc-cod-cli-800.
      *                  *---------------------------------------------*
      *                  * Trattamento periodicita' di fatturazione    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se gia' a zero : oltre                  *
      *                      *-----------------------------------------*
           if        rr-per-fat           =    zero or
                     rr-per-fat           =    99
                     go to acc-cod-cli-800.
      *                      *-----------------------------------------*
      *                      * Forzatura e visualizzazione a valore    *
      *                      * zero                                    *
      *                      *-----------------------------------------*
           move      zero                 to   rr-per-fat             .
           perform   vis-per-fat-000      thru vis-per-fat-999        .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Anagrafica cliente      *
      *    *-----------------------------------------------------------*
       vis-des-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-cli-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Serie elementi da selezionare              *
      *    *-----------------------------------------------------------*
       acc-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
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
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoli all'interno del box                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Codici cliente da seleziona
      -              "re                        "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura titoli                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura inferiore titoli             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero 1                              *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-n1v      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Primo elemento visualizzato : 1             *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-pev      .
      *                  *---------------------------------------------*
      *                  * Numero elemento in accettazione : 1         *
      *                  *---------------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
      *                  *---------------------------------------------*
      *                  * Ad accettazione                             *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-100.
       acc-cli-esl-080.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina se necessario            *
      *              *-------------------------------------------------*
       acc-cli-esl-082.
           move      w-acc-ser-edd-pev    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       11                   to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-nel    <    w-acc-ser-edd-c0a or
                     w-acc-ser-edd-nel    >    w-acc-ser-edd-c0b
                     go to acc-cli-esl-084
           else      go to acc-cli-esl-086.
       acc-cli-esl-084.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-n1v      .
           subtract  1                    from w-acc-ser-edd-n1v      .
           divide    12                   into w-acc-ser-edd-n1v      .
           multiply  12                   by   w-acc-ser-edd-n1v      .
           add       1                    to   w-acc-ser-edd-n1v      .
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-pev      .
           perform   acc-cli-esl-900      thru acc-cli-esl-909        .
       acc-cli-esl-086.
           go to     acc-cli-esl-100.
       acc-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Accettazione linea                              *
      *              *-------------------------------------------------*
       acc-cli-esl-120.
      *              *-------------------------------------------------*
      *              * Visualizzazione numero di pagina                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina in corso di    *
      *                  * trattamento                                 *
      *                  *---------------------------------------------*
           if        w-acc-ser-edd-nel    >    24
                     move  3              to   w-acc-ser-edd-lt1
           else if   w-acc-ser-edd-nel    >    12
                     move  2              to   w-acc-ser-edd-lt1
           else      move  1              to   w-acc-ser-edd-lt1      .
      *                  *---------------------------------------------*
      *                  * Calcolo del numero di pagina totale         *
      *                  *---------------------------------------------*
           if        rr-cod-cli-els       >    24
                     move  3              to   w-acc-ser-edd-lt2
           else if   rr-cod-cli-els       >    12
                     move  2              to   w-acc-ser-edd-lt2
           else      move  1              to   w-acc-ser-edd-lt2      .
      *
           move      3                    to   w-acc-ser-edd-lt2      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-ser-edd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cli-esl-150.
      *                  *---------------------------------------------*
      *                  * Salvataggio valori precedenti linea         *
      *                  *---------------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-acc-ser-edd-spe      .
       acc-cli-esl-200.
      *                  *---------------------------------------------*
      *                  * Accettazione codice elemento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri                  *
      *                      *-----------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-cod-mne-dcc-cod      .
           move      "<B"                 to   v-edm                  .
      *                          *-------------------------------------*
      *                          * Coordinate di posizionamento        *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           move      w-acc-ser-edd-c0r    to   w-cod-mne-dcc-lin      .
           move      09                   to   w-cod-mne-dcc-pos      .
           move      w-cod-mne-dcc-lin    to   w-cod-mne-dcc-rln      .
           move      19                   to   w-cod-mne-dcc-rps      .
      *                          *-------------------------------------*
      *                          * Tasto 'Up'                          *
      *                          *-------------------------------------*
           move      "UP  "               to   v-pfk (01)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Down'                        *
      *                          *-------------------------------------*
           move      "DOWN"               to   v-pfk (02)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Find'                        *
      *                          *-------------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Insr' : disattivato          *
      *                          *-------------------------------------*
           move      spaces               to   v-pfk (04)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Do'                          *
      *                          *-------------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Remove'                      *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   not  = zero
                     go to acc-cli-esl-202.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-204.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-204.
       acc-cli-esl-202.
           move      "REMV"               to   v-pfk (06)             .
       acc-cli-esl-204.
      *                          *-------------------------------------*
      *                          * Tasto 'Previous screen'             *
      *                          *-------------------------------------*
           move      "PRSC"               to   v-pfk (07)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Next screen'                 *
      *                          *-------------------------------------*
           move      "NXSC"               to   v-pfk (08)             .
      *                          *-------------------------------------*
      *                          * Tasto 'Back'                        *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    >    1
                     move  "BACK"         to   v-pfk (09)             .
           if        w-acc-ser-edd-nel    =    w-acc-ser-edd-max
                     go to acc-cli-esl-206.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-206.
      *                          *-------------------------------------*
      *                          * Tasto 'Tab'                         *
      *                          *-------------------------------------*
           move      "TAB "               to   v-pfk (10)             .
       acc-cli-esl-206.
      *                      *-----------------------------------------*
      *                      * Accettazione                            *
      *                      *-----------------------------------------*
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cli-esl-208.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cli-esl-210.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cli-esl-212.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cli-esl-210.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cli-esl-208.
       acc-cli-esl-212.
           move      w-cod-mne-dcc-cod    to   v-num                  .
       acc-cli-esl-215.
      *                      *-----------------------------------------*
      *                      * Valore in campo di destinazione         *
      *                      *-----------------------------------------*
           move      v-num                to   rr-cod-cli-eco
                                              (w-acc-ser-edd-nel)     .
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
           if        v-key                =    spaces
                     go to acc-cli-esl-425.
       acc-cli-esl-220.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cli-esl-225.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sul primo : uscita               *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    =    zero
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-225.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "DOWN"
                     go to acc-cli-esl-250.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se sull'ultimo : uscita             *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-fce    =    spaces
                     add   1              to   w-acc-ser-edd-nel      .
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        w-acc-ser-edd-nel    =    1
                     go to acc-cli-esl-230
           else      go to acc-cli-esl-235.
       acc-cli-esl-230.
           if        rr-cod-cli-eco (1)    =    zero and
                     rr-cod-cli-eco (2)    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-235.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero and
                     rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800
           else      go to acc-cli-esl-080.
       acc-cli-esl-250.
      *                      *-----------------------------------------*
      *                      * Se Insr                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "INSR"
                     go to acc-cli-esl-275.
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-940      thru acc-cli-esl-949        .
           go to     acc-cli-esl-080.
       acc-cli-esl-275.
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-cli-esl-300.
      *                          *-------------------------------------*
      *                          * Controlli per tasto Do              *
      *                          *-------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     go to acc-cli-esl-280
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-cli-esl-200.
       acc-cli-esl-280.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-acc-ric-sel      .
           go to     acc-cli-esl-800.
       acc-cli-esl-300.
      *                      *-----------------------------------------*
      *                      * Se Remv                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "REMV"
                     go to acc-cli-esl-325.
      *                          *-------------------------------------*
      *                          * Compattamento in ogni caso          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
      *                          *-------------------------------------*
      *                          * A reimpostare                       *
      *                          *-------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-325.
      *                      *-----------------------------------------*
      *                      * Se Prsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-cli-esl-350.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su prima facciata : uscita       *
      *                          *-------------------------------------*
           if        w-acc-ser-edd-nel    not  > 14
                     move  "UP  "         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * precedente                          *
      *                          *-------------------------------------*
           subtract  1                    from w-acc-ser-edd-nel      .
           divide    14                   into w-acc-ser-edd-nel      .
           subtract  1                    from w-acc-ser-edd-nel      .
           multiply  14                   by   w-acc-ser-edd-nel      .
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-350.
      *                      *-----------------------------------------*
      *                      * Se Nxsc                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to acc-cli-esl-375.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Se su ultima facciata : uscita      *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
           divide    12                   into w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           multiply  12                   by   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-800.
      *                          *-------------------------------------*
      *                          * Al primo elemento della facciata    *
      *                          * successiva                          *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-375.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "BACK"
                     go to acc-cli-esl-400.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Al primo elemento                   *
      *                          *-------------------------------------*
           move      1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-400.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                not  = "TAB "
                     go to acc-cli-esl-425.
      *                          *-------------------------------------*
      *                          * Compattamento se necessario         *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nec      .
           perform   acc-cli-esl-920      thru acc-cli-esl-929        .
      *                          *-------------------------------------*
      *                          * Dopo l'ultimo elemento inserito     *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-max)   not  = zero
                     move  w-acc-ser-edd-max
                                          to   w-acc-ser-edd-nel
                     go to acc-cli-esl-080.
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-nel      .
       acc-cli-esl-405.
           if        w-acc-ser-edd-nel    =    zero
                     go to acc-cli-esl-410.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     subtract  1          from w-acc-ser-edd-nel
                     go to     acc-cli-esl-405.
       acc-cli-esl-410.
           add       1                    to   w-acc-ser-edd-nel      .
           go to     acc-cli-esl-080.
       acc-cli-esl-425.
      *                      *-----------------------------------------*
      *                      * Se Return                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcc]              *
      *                          *-------------------------------------*
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Trattamento descrizione             *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-cod-cli-ers
                                              (w-acc-ser-edd-nel)     .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nel    to   w-acc-ser-edd-c0r      .
           subtract  w-acc-ser-edd-pev    from w-acc-ser-edd-c0r      .
           add       7                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
      *                          *-------------------------------------*
      *                          * Se record non trovato : reimposta-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to acc-cli-esl-200.
      *                          *-------------------------------------*
      *                          * Controllo valore impostato          *
      *                          *-------------------------------------*
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nel)   =    zero
                     go to acc-cli-esl-450
           else      go to acc-cli-esl-500.
       acc-cli-esl-450.
      *                          *-------------------------------------*
      *                          * Se impostato zero                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se il valore precedente era a   *
      *                              * zero : come per Down            *
      *                              *---------------------------------*
           if        w-acc-ser-edd-spe    =    zero
                     move  "DOWN"         to   v-key
                     go to acc-cli-esl-225.
      *                              *---------------------------------*
      *                              * Altrimenti come per Remv        *
      *                              *---------------------------------*
           move      "REMV"               to   v-key                  .
           go to     acc-cli-esl-300.
       acc-cli-esl-500.
      *                  *---------------------------------------------*
      *                  * Passaggio ad elemento successivo            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione elementi selezionati    *
      *                      *-----------------------------------------*
           perform   acc-cli-esl-960      thru acc-cli-esl-969        .
      *                      *-----------------------------------------*
      *                      * Incremento numero elemento              *
      *                      *-----------------------------------------*
           add       1                    to   w-acc-ser-edd-nel      .
      *                      *-----------------------------------------*
      *                      * Se fine elementi : uscita               *
      *                      *-----------------------------------------*
           if        w-acc-ser-edd-nel    >    w-acc-ser-edd-max
                     move  spaces         to   v-key
                     go to acc-cli-esl-800.
      *                      *-----------------------------------------*
      *                      * Altrimenti : ad impostazione linea      *
      *                      *-----------------------------------------*
           go to     acc-cli-esl-080.
       acc-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio v-key e w-cnt-acc-ric-sel       *
      *                  *---------------------------------------------*
           move      v-key                to   w-acc-ser-edd-svk      .
           move      w-cnt-acc-ric-sel    to   w-acc-ser-edd-stu      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino  immagine video                  *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ripristino v-key e w-cnt-acc-ric-sel        *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-svk    to   v-key                  .
           move      w-acc-ser-edd-stu    to   w-cnt-acc-ric-sel      .
      *                  *---------------------------------------------*
      *                  * Fine routine                                *
      *                  *---------------------------------------------*
           go to     acc-cli-esl-999.
       acc-cli-esl-890.
      *              *-------------------------------------------------*
      *              * Subroutines interne                             *
      *              *-------------------------------------------------*
       acc-cli-esl-900.
      *                  *---------------------------------------------*
      *                  * Visualizzazione pagina a partire dall'ele-  *
      *                  * mento numero w-acc-ser-edd-n1v              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-n1v    to   w-acc-ser-edd-c0p      .
           subtract  1                    from w-acc-ser-edd-c0p      .
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-c0q      .
           add       12                   to   w-acc-ser-edd-c0q      .
       acc-cli-esl-901.
           add       1                    to   w-acc-ser-edd-c0p      .
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-max
                     go to acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           move      w-acc-ser-edd-c0p    to   w-acc-ser-edd-nev      .
           move      w-acc-ser-edd-nev    to   w-acc-ser-edd-c0r      .
       acc-cli-esl-903.
           if        w-acc-ser-edd-c0r    >    12
                     subtract  12         from w-acc-ser-edd-c0r
                     go to     acc-cli-esl-903.
           add       6                    to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-901.
       acc-cli-esl-905.
           if        w-acc-ser-edd-c0p    >    w-acc-ser-edd-c0q
                     go to acc-cli-esl-909.
           add       1                    to   w-acc-ser-edd-c0r      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0p      .
           go to     acc-cli-esl-905.
       acc-cli-esl-909.
           exit.
       acc-cli-esl-910.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elemento indirizzato da     *
      *                  * w-acc-ser-edd-nev a linea w-acc-ser-edd-c0r *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-led      .
      *                      *-----------------------------------------*
      *                      * Composizione linea da visualizzare      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing numero riga                 *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-acc-ser-edd-nev    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-rig      .
      *                          *-------------------------------------*
      *                          * Editing carattere  ':'              *
      *                          *-------------------------------------*
           move      ":"                  to   w-acc-ser-edd-dpu      .
      *                          *-------------------------------------*
      *                          * Editing codice cliente              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-nev)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-acc-ser-edd-cod      .
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rr-cod-cli-ers
                    (w-acc-ser-edd-nev)   to   w-acc-ser-edd-des      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      w-acc-ser-edd-c0r    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      w-acc-ser-edd-led    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-919.
           exit.
       acc-cli-esl-920.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec se necessario             *
      *                  *---------------------------------------------*
           move      spaces               to   w-acc-ser-edd-fce      .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-nec)   not  = zero
                     go to acc-cli-esl-929.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-929.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-929.
           perform   acc-cli-esl-930      thru acc-cli-esl-939        .
           move      "#"                  to   w-acc-ser-edd-fce      .
       acc-cli-esl-929.
           exit.
       acc-cli-esl-930.
      *                  *---------------------------------------------*
      *                  * Compattamento dell' elemento indirizzato da *
      *                  * w-acc-ser-edd-nec in ogni caso              *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0b      .
       acc-cli-esl-931.
           if        w-acc-ser-edd-c0b    >    w-acc-ser-edd-max
                     go to acc-cli-esl-932.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0b)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0a)     .
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0b)   =    zero
                     go to acc-cli-esl-933.
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-931.
       acc-cli-esl-932.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
       acc-cli-esl-933.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-934.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-934.
           move      w-acc-ser-edd-c0a    to   w-acc-ser-edd-c0b      .
           add       6                    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0c      .
           add       1                    to   w-acc-ser-edd-c0c      .
       acc-cli-esl-935.
           if        w-acc-ser-edd-c0a    =    12
                     go to acc-cli-esl-936.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           add       1                    to   w-acc-ser-edd-c0a      .
           add       1                    to   w-acc-ser-edd-c0b      .
           add       1                    to   w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-935.
       acc-cli-esl-936.
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-937.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-937.
           subtract  w-acc-ser-edd-c0a    from 12
                                        giving w-acc-ser-edd-c0b      .
           add       w-acc-ser-edd-nec
                     w-acc-ser-edd-c0b  giving w-acc-ser-edd-c0c      .
           if        w-acc-ser-edd-c0c    >    w-acc-ser-edd-max
                     go to acc-cli-esl-938.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0c)   =    zero
                     go to acc-cli-esl-938.
           move      w-acc-ser-edd-c0c    to   w-acc-ser-edd-nev      .
           move      18                   to   w-acc-ser-edd-c0r      .
           perform   acc-cli-esl-910      thru acc-cli-esl-919        .
           go to     acc-cli-esl-939.
       acc-cli-esl-938.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-939.
           exit.
       acc-cli-esl-940.
      *                  *---------------------------------------------*
      *                  * Inserimento dell' elemento indirizzato da   *
      *                  * w-acc-ser-edd-nec                           *
      *                  *---------------------------------------------*
           move      w-acc-ser-edd-max    to   w-acc-ser-edd-c0b      .
           move      w-acc-ser-edd-c0b    to   w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0a      .
       acc-cli-esl-941.
           move      rr-cod-cli-ele
                    (w-acc-ser-edd-c0a)   to   rr-cod-cli-ele
                                              (w-acc-ser-edd-c0b)     .
           if        w-acc-ser-edd-c0a    =    w-acc-ser-edd-nec
                     go to acc-cli-esl-942.
           subtract  1                    from w-acc-ser-edd-c0a      .
           subtract  1                    from w-acc-ser-edd-c0b      .
           go to     acc-cli-esl-941.
       acc-cli-esl-942.
           move      zero                 to   rr-cod-cli-eco
                                              (w-acc-ser-edd-c0a)     .
           move      spaces               to   rr-cod-cli-ers
                                              (w-acc-ser-edd-c0a)     .
           move      w-acc-ser-edd-nec    to   w-acc-ser-edd-c0a      .
       acc-cli-esl-943.
           if        w-acc-ser-edd-c0a    >    12
                     subtract  12         from w-acc-ser-edd-c0a
                     go to     acc-cli-esl-943.
           add       6                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    =    18
                     go to acc-cli-esl-945.
           move      17                   to   w-acc-ser-edd-c0b      .
           move      18                   to   w-acc-ser-edd-c0c      .
       acc-cli-esl-944.
           move      "FL"                 to   v-ope                  .
           move      w-acc-ser-edd-c0b    to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-acc-ser-edd-fcl      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0c    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-ser-edd-070    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        w-acc-ser-edd-c0b    =    w-acc-ser-edd-c0a
                     go to acc-cli-esl-945.
           subtract  1                    from w-acc-ser-edd-c0b      .
           subtract  1                    from w-acc-ser-edd-c0c      .
           go to     acc-cli-esl-944.
       acc-cli-esl-945.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      70                   to   v-car                  .
           move      w-acc-ser-edd-c0a    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cli-esl-949.
           exit.
       acc-cli-esl-960.
      *                  *---------------------------------------------*
      *                  * Visualizzazione elementi da trattare        *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c0a      .
       acc-cli-esl-962.
           add       1                    to   w-acc-ser-edd-c0a      .
           if        w-acc-ser-edd-c0a    >    w-acc-ser-edd-max
                     go to acc-cli-esl-964.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c0a)   =    zero
                     go to acc-cli-esl-964.
           go to     acc-cli-esl-962.
       acc-cli-esl-964.
           subtract  1                    from w-acc-ser-edd-c0a      .
           move      w-acc-ser-edd-c0a    to   rr-cod-cli-els         .
       acc-cli-esl-969.
           exit.
       acc-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Preparazione codici selezionati                           *
      *    *-----------------------------------------------------------*
       pcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Preparazione di un campo text di massimo 3 li-  *
      *              * nee da 40 caratteri ciascuna                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-ser-edd-txt      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-cod-cli-sns       =    zero
                     go to pcs-cli-esl-900.
           move      zero                 to   w-acc-ser-edd-c01      .
       pcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt
                    (119:1)               not  = spaces
                     go to pcs-cli-esl-900.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pcs-cli-esl-900.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Editing codice elemento                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "("            to   w-all-str-cat (1)
           else      move  spaces         to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (3)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (4)      .
           if        w-acc-ser-edd-txt    =    spaces
                     move  spaces         to   w-all-str-cat (5)
           else      move  ","            to   w-all-str-cat (5)      .
           move      v-edt                to   w-all-str-cat (6)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
       pcs-cli-esl-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-100.
       pcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           if        w-acc-ser-edd-txt    =    spaces
                     go to pcs-cli-esl-999.
      *              *-------------------------------------------------*
      *              * Completamento valore in uscita                  *
      *              *-------------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      w-acc-ser-edd-rtr (1)
                                          to   w-all-str-cat (1)      .
           move      w-acc-ser-edd-rtr (2)
                                          to   w-all-str-cat (2)      .
           move      w-acc-ser-edd-rtr (3)
                                          to   w-all-str-cat (3)      .
           move      ")"                  to   w-all-str-cat (4)      .   
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-acc-ser-edd-txt      .
           if        w-acc-ser-edd-txt
                    (120:1)               not  = ")"  and
                     w-acc-ser-edd-txt
                    (120:1)               not  = spaces
                     move  "...)"         to   w-acc-ser-edd-txt
                                              (117:4)                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pcs-cli-esl-999.
       pcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione codici selezionati                        *
      *    *-----------------------------------------------------------*
       vcs-cli-esl-000.
      *              *-------------------------------------------------*
      *              * Pulizia della prima riga e segnale di selezione *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           if        w-acc-ser-edd-txt    =    spaces
                     move  "Tutti     "   to   v-alf
           else      move  "+         "   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-100.
      *              *-------------------------------------------------*
      *              * Preparazione contatore                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-ser-edd-c01      .
       vcs-cli-esl-200.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione                           *
      *              *-------------------------------------------------*
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    1
                     go to vcs-cli-esl-900.
       vcs-cli-esl-300.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           add       w-acc-ser-edd-c01    to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-acc-ser-edd-rtr
                    (w-acc-ser-edd-c01)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vcs-cli-esl-400.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-200.
       vcs-cli-esl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vcs-cli-esl-999.
       vcs-cli-esl-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data massima documenti da  *
      *    * fatturare                                                 *
      *    *-----------------------------------------------------------*
       acc-dat-ddf-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-ddf-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-ddf           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-ddf-999.
       acc-dat-ddf-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-ddf             .
       acc-dat-ddf-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-ddf-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-dat-ddf-100.
       acc-dat-ddf-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        rr-dat-ddf           not  = zero
                     go to acc-dat-ddf-600.
           if        v-key                =    "UP  "
                     go to acc-dat-ddf-600
           else      go to acc-dat-ddf-100.
      *                  *---------------------------------------------*
      *                  * Confronto con data emissione fatture        *
      *                  *---------------------------------------------*
           if        rr-dat-ddf           >    rr-dat-emi
                     go to acc-dat-ddf-100.
       acc-dat-ddf-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-ddf-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-ddf-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-ddf-100.
       acc-dat-ddf-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Data massima documenti  *
      *    * da fatturare                                              *
      *    *-----------------------------------------------------------*
       vis-dat-ddf-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-ddf           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddf-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Data minima documenti da   *
      *    * fatturare                                                 *
      *    *-----------------------------------------------------------*
       acc-dat-ddi-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 1
                     go to acc-dat-ddi-999.
       acc-dat-ddi-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-ddi           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-ddi-999.
       acc-dat-ddi-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-ddi             .
       acc-dat-ddi-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-ddi-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
           go to     acc-dat-ddi-100.
       acc-dat-ddi-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se a zero : reimpostazione                  *
      *                  *---------------------------------------------*
           if        rr-dat-ddi           not  = zero
                     go to acc-dat-ddi-600.
           if        v-key                =    "UP  "
                     go to acc-dat-ddi-600
           else      go to acc-dat-ddi-100.
      *                  *---------------------------------------------*
      *                  * Confronto con data emissione fatture        *
      *                  *---------------------------------------------*
           if        rr-dat-ddi           >    rr-dat-emi
                     go to acc-dat-ddi-100.
       acc-dat-ddi-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-ddi-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-ddi-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-ddi-100.
       acc-dat-ddi-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Data minima documenti   *
      *    * da fatturare                                              *
      *    *-----------------------------------------------------------*
       vis-dat-ddi-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-ddi           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-ddi-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Periodicita' fatturazione  *
      *    *-----------------------------------------------------------*
       acc-per-fat-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to acc-per-fat-999.
       acc-per-fat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-per-fat-lun    to   v-car                  .
           move      w-exp-per-fat-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-per-fat-tbl    to   v-txt                  .
           if        rr-per-fat           =    99
                     move  01             to   v-num
           else if   rr-per-fat           =    01
                     move  02             to   v-num
           else if   rr-per-fat           =    02
                     move  03             to   v-num
           else if   rr-per-fat           =    03
                     move  04             to   v-num
           else if   rr-per-fat           =    04
                     move  05             to   v-num
           else if   rr-per-fat           =    05
                     move  06             to   v-num
           else if   rr-per-fat           =    06
                     move  07             to   v-num
           else if   rr-per-fat           =    07
                     move  08             to   v-num
           else if   rr-per-fat           =    08
                     move  09             to   v-num
           else if   rr-per-fat           =    09
                     move  10             to   v-num
           else if   rr-per-fat           =    10
                     move  11             to   v-num
           else if   rr-per-fat           =    11
                     move  12             to   v-num
           else      move  zero           to   v-num                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-per-fat-999.
       acc-per-fat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           if        v-num                =    01
                     move  99             to   rr-per-fat
           else if   v-num                =    02
                     move  01             to   rr-per-fat
           else if   v-num                =    03
                     move  02             to   rr-per-fat
           else if   v-num                =    04
                     move  03             to   rr-per-fat
           else if   v-num                =    05
                     move  04             to   rr-per-fat
           else if   v-num                =    06
                     move  05             to   rr-per-fat
           else if   v-num                =    07
                     move  06             to   rr-per-fat
           else if   v-num                =    08
                     move  07             to   rr-per-fat
           else if   v-num                =    09
                     move  08             to   rr-per-fat
           else if   v-num                =    10
                     move  09             to   rr-per-fat
           else if   v-num                =    11
                     move  10             to   rr-per-fat
           else if   v-num                =    12
                     move  11             to   rr-per-fat
           else      move  zero           to   rr-per-fat             .
       acc-per-fat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se zero : reimpostazione                    *
      *                  *---------------------------------------------*
           if        rr-per-fat           not  = zero
                     go to acc-per-fat-600.
           if        v-key                =    "UP  "
                     go to acc-per-fat-600
           else      go to acc-per-fat-100.
       acc-per-fat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-per-fat-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-per-fat-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-per-fat-100.
       acc-per-fat-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Periodicita' fatturazione         *
      *    *-----------------------------------------------------------*
       vis-per-fat-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-per-fat-lun    to   v-car                  .
           move      w-exp-per-fat-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-per-fat-tbl    to   v-txt                  .
           if        rr-per-fat           =    99
                     move  01             to   v-num
           else if   rr-per-fat           =    01
                     move  02             to   v-num
           else if   rr-per-fat           =    02
                     move  03             to   v-num
           else if   rr-per-fat           =    03
                     move  04             to   v-num
           else if   rr-per-fat           =    04
                     move  05             to   v-num
           else if   rr-per-fat           =    05
                     move  06             to   v-num
           else if   rr-per-fat           =    06
                     move  07             to   v-num
           else if   rr-per-fat           =    07
                     move  08             to   v-num
           else if   rr-per-fat           =    08
                     move  09             to   v-num
           else if   rr-per-fat           =    09
                     move  10             to   v-num
           else if   rr-per-fat           =    10
                     move  11             to   v-num
           else if   rr-per-fat           =    11
                     move  12             to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-per-fat-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo movimento da ricercare                *
      *    *-----------------------------------------------------------*
       acc-cod-tmo-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmo-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zfi-ope      .
           move      rr-cod-tmo           to   w-cod-des-zfi-cod      .
           move      17                   to   w-cod-des-zfi-lin      .
           move      30                   to   w-cod-des-zfi-pos      .
           move      17                   to   w-cod-des-zfi-dln      .
           move      37                   to   w-cod-des-zfi-dps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
       acc-cod-tmo-110.
           perform   cod-des-zfi-cll-000  thru cod-des-zfi-cll-999    .
           if        w-cod-des-zfi-ope    =    "F+"
                     go to acc-cod-tmo-115.
           if        w-cod-des-zfi-ope    =    "AC"
                     go to acc-cod-tmo-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-tmo-115.
           perform   cod-des-zfi-foi-000  thru cod-des-zfi-foi-999    .
           go to     acc-cod-tmo-110.
       acc-cod-tmo-120.
           move      w-cod-des-zfi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-tmo-999.
       acc-cod-tmo-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-cod-tmo             .
       acc-cod-tmo-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfi]                      *
      *                  *---------------------------------------------*
           move      rr-cod-tmo           to   w-let-arc-zfi-cod      .
           move      zero                 to   w-let-arc-zfi-dpz      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
           if        rr-cod-tmo           =    spaces
                     move  "Tutti"        to   rr-cod-tmo-des
           else      move  w-let-arc-zfi-des
                                          to   rr-cod-tmo-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmo-des-000  thru vis-cod-tmo-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zfi-flg    not  = spaces
                     go to acc-cod-tmo-100.
       acc-cod-tmo-450.
      *                  *---------------------------------------------*
      *                  * Test se documento adatto per la fatturazio- *
      *                  * ne differita                                *
      *                  *---------------------------------------------*
           if        rr-cod-tmo           =    spaces
                     go to acc-cod-tmo-600.
           if        w-let-arc-zfi-org    =    11
                     go to acc-cod-tmo-600.
      *                  *---------------------------------------------*
      *                  * Se l'origine del documento non e' automati- *
      *                  * ca : messaggio di errore e reimpostazione   *
      *                  *---------------------------------------------*
           move      "Tipo documento non adatto per la fatturazione diff
      -              "erita !"            to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio                   *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-tmo-100.
       acc-cod-tmo-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-tmo-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-tmo-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-tmo-100.
       acc-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : tipo movimento da ricercare             *
      *    *-----------------------------------------------------------*
       vis-cod-tmo-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-tmo           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmo-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : descrizione tipo movimento da ricercare *
      *    *-----------------------------------------------------------*
       vis-cod-tmo-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      rr-cod-tmo-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmo-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Codice dipendenza          *
      *    *-----------------------------------------------------------*
       acc-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        not  > 1
                     go to acc-cod-dpz-999.
       acc-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dpz-ope      .
           move      rr-cod-dpz           to   w-cod-cod-dpz-cod      .
           move      20                   to   w-cod-cod-dpz-lin      .
           move      30                   to   w-cod-cod-dpz-pos      .
           move      20                   to   w-cod-cod-dpz-dln      .
           move      34                   to   w-cod-cod-dpz-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
       acc-cod-dpz-110.
           perform   cod-cod-dpz-cll-000  thru cod-cod-dpz-cll-999    .
           if        w-cod-cod-dpz-ope    =    "F+"
                     go to acc-cod-dpz-115.
           if        w-cod-cod-dpz-ope    =    "AC"
                     go to acc-cod-dpz-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-dpz-115.
           perform   cod-cod-dpz-foi-000  thru cod-cod-dpz-foi-999    .
           go to     acc-cod-dpz-110.
       acc-cod-dpz-120.
           move      w-cod-cod-dpz-cod    to   v-num                  .
       acc-cod-dpz-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-dpz-999.
       acc-cod-dpz-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-dpz             .
       acc-cod-dpz-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [ada]                      *
      *                  *---------------------------------------------*
           move      rr-cod-dpz           to   w-let-arc-dpz-cod      .
           perform   let-arc-dpz-000      thru let-arc-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           move      w-let-arc-dpz-des    to   rr-cod-dpz-des         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-des-dpz-000      thru vis-des-dpz-999        .
       acc-cod-dpz-430.
      *                  *---------------------------------------------*
      *                  * Se lettura errata : reimpostazione          *
      *                  *---------------------------------------------*
           if        w-let-arc-dpz-flg    not  = spaces
                     go to acc-cod-dpz-100.
       acc-cod-dpz-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-dpz-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-dpz-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-dpz-100.
       acc-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo selezione : Descrizione dipendenza  *
      *    *-----------------------------------------------------------*
       vis-des-dpz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rr-cod-dpz-des       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-dpz-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controlli                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo esecuzione                             *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = zero
                     go to tdo-ric-sel-110.
           move      "Manca il tipo di esecuzione programma"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-110.
      *                  *---------------------------------------------*
      *                  * Data emissione fatture                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se a zero                          *
      *                      *-----------------------------------------*
           if        rr-dat-emi           not  = zero
                     go to tdo-ric-sel-130.
           move      "Manca la data emissione fatture"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-130.
      *                      *-----------------------------------------*
      *                      * Test che non sia superiore alla data at-*
      *                      * tuale in caso di emissione definitiva   *
      *                      *-----------------------------------------*
           if        rr-tip-exe           =    1
                     go to tdo-ric-sel-140.
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        rr-dat-emi           not  > s-dat
                     go to tdo-ric-sel-140.
           move      "Data emissione fatture inaccettabile !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-140.
      *                  *---------------------------------------------*
      *                  * Data documenti da fatturare massima         *
      *                  *---------------------------------------------*
           if        rr-dat-ddf           not  > rr-dat-emi
                     go to tdo-ric-sel-145.
           move      "Data documenti da fatturare inaccettabile !"
                                          to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-145.
      *                  *---------------------------------------------*
      *                  * Confronto con data minima : solo se stampa  *
      *                  * di simulazione                              *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 1
                     go to tdo-ric-sel-150.
           if        rr-dat-ddi           =    zero or
                     rr-dat-ddf           =    zero
                     go to tdo-ric-sel-150.
           if        rr-dat-ddi           not  > rr-dat-ddf
                     go to tdo-ric-sel-150.
           move      "Data documenti iniziale maggiore di quella finale 
      -              "!"                  to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-150.
      *              *-------------------------------------------------*
      *              * Test di accettabilita' per la data di emissione *
      *              * in funzione della presenza o meno del codice    *
      *              * tipo movimento da selezionare                   *
      *              *-------------------------------------------------*
           if        rr-cod-tmo           not  = spaces
                     go to tdo-ric-sel-300.
       tdo-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Test per tutti i tipi documento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su archivio [zfi]                 *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTMO    "         to   f-key                  .
           move      spaces               to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                          *-------------------------------------*
      *                          * Se start errata : uscita            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to tdo-ric-sel-290.
       tdo-ric-sel-220.
      *                      *-----------------------------------------*
      *                      * Lettura sequenziale archivio [zfi]      *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                          *-------------------------------------*
      *                          * Se lettura errata : uscita          *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to tdo-ric-sel-290.
       tdo-ric-sel-230.
      *                      *-----------------------------------------*
      *                      * Selezione su tipo documento             *
      *                      *                                         *
      *                      * Esclusione Fatture Pro-Forma            *
      *                      *-----------------------------------------*
           if        rf-zfi-tip-doc       =    04
                     go to tdo-ric-sel-220.
      *                      *-----------------------------------------*
      *                      * Selezione su origine del documento      *
      *                      *-----------------------------------------*
           if        rf-zfi-org-doc       not  = 11
                     go to tdo-ric-sel-220.
       tdo-ric-sel-240.
      *                      *-----------------------------------------*
      *                      * Determinazione data ultimo documento    *
      *                      * emesso per il tipo di documento in      *
      *                      * corso di trattamento                    *
      *                      *-----------------------------------------*
           move      rr-dpz-emi           to   w-det-ult-doc-dpe      .
           move      rf-zfi-vld-dpz       to   w-det-ult-doc-vld      .
           move      rr-dat-emi           to   w-det-ult-doc-dte      .
           move      rf-zfi-num-giv       to   w-det-ult-doc-giv      .
           move      rf-zfi-sgl-num       to   w-det-ult-doc-sgl      .
           perform   det-ult-doc-000      thru det-ult-doc-999        .
      *                      *-----------------------------------------*
      *                      * Confronto con il valore determinato     *
      *                      *-----------------------------------------*
           if        w-det-ult-doc-ctr    =    zero
                     go to tdo-ric-sel-280.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore speciale            *
      *                      *-----------------------------------------*
           move      "Attenzione: la data di emissione per le fatture ri
      -              "sulta essere   "    to   w-err-box-err-msg      .
           move      "            inferiore a quella gia' utilizzata per
      -              " altri documen-"    to   w-err-box-err-m02      .
           move      "            ti. Si consiglia pertanto di effettuar
      -              "e dei controlli"    to   w-err-box-err-m03      .
           perform   box-msg-e03-000      thru box-msg-e03-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-280.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura [zfi]                 *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-220.
       tdo-ric-sel-290.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Test per un solo tipo documento             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura archivio [zfi]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      rr-cod-tmo           to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : ad uscita           *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to tdo-ric-sel-800.
      *                      *-----------------------------------------*
      *                      * Determinazione data ultimo documento    *
      *                      * emesso per il tipo di documento in      *
      *                      * corso di trattamento                    *
      *                      *-----------------------------------------*
           move      rr-dpz-emi           to   w-det-ult-doc-dpe      .
           move      rf-zfi-vld-dpz       to   w-det-ult-doc-vld      .
           move      rr-dat-emi           to   w-det-ult-doc-dte      .
           move      rf-zfi-num-giv       to   w-det-ult-doc-giv      .
           move      rf-zfi-sgl-num       to   w-det-ult-doc-sgl      .
           perform   det-ult-doc-000      thru det-ult-doc-999        .
      *                      *-----------------------------------------*
      *                      * Confronto con il valore determinato     *
      *                      *-----------------------------------------*
           if        w-det-ult-doc-ctr    =    zero
                     go to tdo-ric-sel-800.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore speciale            *
      *                      *-----------------------------------------*
           move      "Attenzione: la data di emissione per le fatture ri
      -              "sulta essere   "    to   w-err-box-err-msg      .
           move      "            inferiore a quella gia' utilizzata per
      -              " altri documen-"    to   w-err-box-err-m02      .
           move      "            ti. Si consiglia pertanto di effettuar
      -              "e dei controlli"    to   w-err-box-err-m03      .
           perform   box-msg-e03-000      thru box-msg-e03-999        .
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     tdo-ric-sel-800.
       tdo-ric-sel-800.
      *              *-------------------------------------------------*
      *              * Uscita per controlli superati                   *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio                   *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Regolarizzazione codice cliente singolo o mul-  *
      *              * tiplo                                           *
      *              *-------------------------------------------------*
           if        rr-cod-cli-sns       not  = zero
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
                     go to reg-ric-sel-050.
           move      zero                 to   rr-cod-cli-els         .
       reg-ric-sel-027.
           add       1                    to   rr-cod-cli-els         .
           if        rr-cod-cli-els       >    36
                     go to reg-ric-sel-029.
           move      zero                 to   rr-cod-cli-eco
                                              (rr-cod-cli-els)        .
           move      spaces               to   rr-cod-cli-ers
                                              (rr-cod-cli-els)        .
           go to     reg-ric-sel-027.
       reg-ric-sel-029.
           move      zero                 to   rr-cod-cli-els         .
       reg-ric-sel-050.
      *              *-------------------------------------------------*
      *              * Normalizzazione periodicita' di fatturazione    *
      *              *-------------------------------------------------*
           if        rr-per-fat           =    99
                     move  zero           to   rr-per-fat             .
      *              *-------------------------------------------------*
      *              * Attribuzione progressivo lotto di fatturazione  *
      *              *-------------------------------------------------*
           perform   att-fti-lfd-000      thru att-fti-lfd-999        .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di ipc : progressivo lotto  *
      *              * di fatturazione                                 *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-lfd"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "P"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      w-att-fti-lfd-num    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Determinazione parametri associati all'esecu-   *
      *              * zione programma                                 *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    1
                     go to reg-ric-sel-100
           else if   rr-tip-exe           =    2
                     go to reg-ric-sel-200.
       reg-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Tipo stampa : Stampa di simulazione             *
      *              *-------------------------------------------------*
           move      "pfat400a"           to   i-exe-pro              .
           move      "pgm/fat/prg/obj/pfat400a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           move      "S"                  to   w-cnt-fun-snx-stp      .
           go to     reg-ric-sel-300.
       reg-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Tipo stampa : Emissione definitiva              *
      *              *-------------------------------------------------*
           move      "pfat400b"           to   i-exe-pro              .
           move      "pgm/fat/prg/obj/pfat400b"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           move      s-pat                to   i-exe-pat              .
           move      "N"                  to   w-cnt-fun-snx-stp      .
           go to     reg-ric-sel-300.
       reg-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     reg-ric-sel-999.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
           perform   nor-ric-sel-cli-000  thru nor-ric-sel-cli-999    .
           move      zero                 to   rr-tip-exe             .
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
           move      zero                 to   rr-dat-ddi             .
           move      zero                 to   rr-dat-ddf             .
           move      zero                 to   rr-cod-dpz             .
           move      spaces               to   rr-cod-dpz-des         .
           move      zero                 to   rr-per-fat             .
           move      zero                 to   rr-dat-emi             .
           move      spaces               to   rr-cod-tmo             .
           move      spaces               to   rr-cod-tmo-des         .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *                                                           *
      *    * Normalizzazione specifica per codici cliente              *
      *    *-----------------------------------------------------------*
       nor-ric-sel-cli-000.
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
           move      zero                 to   rr-cod-cli-sns         .
           move      zero                 to   rr-cod-cli-els         .
       nor-ric-sel-cli-100.
           add       1                    to   rr-cod-cli-els         .
           if        rr-cod-cli-els       >    36
                     go to nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-eco
                                              (rr-cod-cli-els)        .
           move      spaces               to   rr-cod-cli-ers
                                              (rr-cod-cli-els)        .
           go to     nor-ric-sel-cli-100.
       nor-ric-sel-cli-200.
           move      zero                 to   rr-cod-cli-els         .
           go to     nor-ric-sel-cli-999.
       nor-ric-sel-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-nom      .
           go to     let-arc-age-999.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all"."               to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ada]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/larcada0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zfi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    spaces
                     go to let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-zfi-cod    to   rf-zfi-cod-tmo         .
           move      w-let-arc-zfi-dpz    to   rf-zfi-cod-dpz         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfi-400.
       let-arc-zfi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfi-des-tmo       to   w-let-arc-zfi-des      .
           move      rf-zfi-org-doc       to   w-let-arc-zfi-org      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfi-999.
       let-arc-zfi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfi-flg      .
           move      all   "."            to   w-let-arc-zfi-des      .
           go to     let-arc-zfi-600.
       let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-des      .
       let-arc-zfi-600.
           move      zero                 to   w-let-arc-zfi-org      .
       let-arc-zfi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [lic]                         *
      *    *-----------------------------------------------------------*
       let-arc-lic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-lic-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-lic-cod    =    zero
                     go to let-arc-lic-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-lic-cod    to   rf-lic-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-lic-400.
       let-arc-lic-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-lic-drf-ini       to   w-let-arc-lic-dri      .
           move      rf-lic-drf-fin       to   w-let-arc-lic-drf      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-lic-999.
       let-arc-lic-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-lic-flg      .
           go to     let-arc-lic-520.
       let-arc-lic-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
       let-arc-lic-520.
           move      zero                 to   w-let-arc-lic-dri      .
           move      zero                 to   w-let-arc-lic-drf      .
       let-arc-lic-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-cod    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-cod    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-dui      .
           move      rf-dcp-tip-pro       to   w-let-arc-dcp-tpr      .
           move      rf-dcp-cod-iva       to   w-let-arc-dcp-civ      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
           move      rf-dcp-dec-prz       to   w-let-arc-dcp-dlb      .
           move      rf-dcp-prz-lst       to   w-let-arc-dcp-plb      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-dui      .
           go to     let-arc-dcp-520.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-dui      .
       let-arc-dcp-520.
           move      zero                 to   w-let-arc-dcp-tpr      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
           move      zero                 to   w-let-arc-dcp-dlb      .
           move      zero                 to   w-let-arc-dcp-plb      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zac]                         *
      *    *-----------------------------------------------------------*
       let-arc-zac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zac-tip    =    zero or
                     w-let-arc-zac-cod    =    zero
                     go to let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAOC    "         to   f-key                  .
           move      w-let-arc-zac-tip    to   rf-zac-tip-rec         .
           move      w-let-arc-zac-cod    to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zac-400.
       let-arc-zac-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zac-des-aoc       to   w-let-arc-zac-des      .
           move      rf-zac-cod-iva       to   w-let-arc-zac-civ      .
           move      rf-zac-cod-ctp       to   w-let-arc-zac-ccp      .
           move      rf-zac-tip-tot       to   w-let-arc-zac-tot      .
           if        rf-zac-tip-tot       not  numeric
                     move  zero           to   w-let-arc-zac-tot      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zac-999.
       let-arc-zac-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zac-flg      .
           move      spaces               to   w-let-arc-zac-des      .
           move      all   "."            to   w-let-arc-zac-drg (1)  .
           go to     let-arc-zac-600.
       let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-des      .
       let-arc-zac-600.
           move      zero                 to   w-let-arc-zac-civ      .
           move      zero                 to   w-let-arc-zac-ccp      .
           move      zero                 to   w-let-arc-zac-tot      .
       let-arc-zac-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zvf]                         *
      *    *-----------------------------------------------------------*
       let-arc-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zvf-num    =    zero or
                     w-let-arc-zvf-cod    =    spaces
                     go to let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-let-arc-zvf-num    to   rf-zvf-num-def         .
           move      w-let-arc-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zvf-400.
       let-arc-zvf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zvf-des-stp       to   w-let-arc-zvf-des      .
           move      rf-zvf-num-spf       to   w-let-arc-zvf-spf      .
      *
           if        rf-zvf-num-spf       not  numeric
                     move  zero           to   w-let-arc-zvf-spf      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zvf-999.
       let-arc-zvf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zvf-flg      .
           move      all   "."            to   w-let-arc-zvf-des      .
           go to     let-arc-zvf-600.
       let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-des      .
       let-arc-zvf-600.
           move      zero                 to   w-let-arc-zvf-spf      .
       let-arc-zvf-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su tre righe          *
      *    *-----------------------------------------------------------*
       box-msg-e03-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      10                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m03    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e03-999.
           exit.

      *    *===========================================================*
      *    * Attribuzione progressivo lotto di fatturazione            *
      *    *-----------------------------------------------------------*
       att-fti-lfd-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-att-fti-lfd-flg      .
      *              *-------------------------------------------------*
      *              * Determinazione secolo/anno attuali              *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-saa                to   w-att-fti-lfd-saa      .
       att-fti-lfd-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [ftilfd]                 *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Get record [ftilfd]                             *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      w-att-fti-lfd-saa    to   rn-fti-lfd-scl-ann     .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-fti-lfd-300.
       att-fti-lfd-200.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Rilascio record [ftilfd]                    *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'operazione                 *
      *                  *---------------------------------------------*
           go to     att-fti-lfd-100.
       att-fti-lfd-300.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ftilfd]               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero progressivo        *
      *                      *-----------------------------------------*
           move      rn-fti-lfd-num-lfd   to   w-att-fti-lfd-wnu      .
           add       1                    to   w-att-fti-lfd-wpr      .
           if        w-att-fti-lfd-wpr    =    zero
                     move  1              to   w-att-fti-lfd-wpr      .
      *                      *-----------------------------------------*
      *                      * Forzatura secolo/anno                   *
      *                      *-----------------------------------------*
           move      w-att-fti-lfd-saa    to   w-att-fti-lfd-wsa      .
      *                      *-----------------------------------------*
      *                      * Movimento da work a record              *
      *                      *-----------------------------------------*
           move      w-att-fti-lfd-wnu    to   rn-fti-lfd-num-lfd     .
      *                      *-----------------------------------------*
      *                      * Status di avanzamento : in esecuzione   *
      *                      *-----------------------------------------*
           move      "#"                  to   rn-fti-lfd-sts-avz     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Release record                              *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *                  *---------------------------------------------*
      *                  * Se errori si ripete l'operazione            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to att-fti-lfd-100.
      *              *-------------------------------------------------*
      *              * Numero progressivo in uscita                    *
      *              *-------------------------------------------------*
           move      rn-fti-lfd-num-lfd   to   w-att-fti-lfd-num      .
       att-fti-lfd-999.
           exit.

      *    *===========================================================*
      *    * Ripristino progressivo lotto di fatturazione              *
      *    *-----------------------------------------------------------*
       rip-fti-lfd-000.
      *              *-------------------------------------------------*
      *              * Get record [ftilfd]                             *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      w-att-fti-lfd-saa    to   rn-fti-lfd-scl-ann     .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
      *              *-------------------------------------------------*
      *              * Test su esito operazione                        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rip-fti-lfd-300.
       rip-fti-lfd-200.
      *              *-------------------------------------------------*
      *              * Se record non trovato                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-fti-lfd-999.
       rip-fti-lfd-300.
      *              *-------------------------------------------------*
      *              * Se record trovato                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto con il numero attuale; se diverso *
      *                  * unlock e uscita                             *
      *                  *---------------------------------------------*
           if        rn-fti-lfd-num-lfd   not  = w-att-fti-lfd-num
                     go to  rip-fti-lfd-900.
      *                  *---------------------------------------------*
      *                  * Se uguale si esegue il controaggiornamento  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero progressivo                      *
      *                      *-----------------------------------------*
           move      w-att-fti-lfd-num    to   w-att-fti-lfd-wnu      .
           subtract  1                    from w-att-fti-lfd-wpr      .
           if        w-att-fti-lfd-wpr    =    zero
                     move  zero           to   w-att-fti-lfd-wnu      .
           move      w-att-fti-lfd-wnu    to   rn-fti-lfd-num-lfd     .
      *                      *-----------------------------------------*
      *                      * Status di avanzamento                   *
      *                      *-----------------------------------------*
           move      spaces               to   rn-fti-lfd-sts-avz     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
       rip-fti-lfd-900.
      *              *-------------------------------------------------*
      *              * Unlock                                          *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/num/ioc/obj/inftilfd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-fti-lfd             .
       rip-fti-lfd-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione pathname completo per il file di appoggio  *
      *    * per Bolle da Fatturare                                    *
      *    *-----------------------------------------------------------*
       det-pat-fde-000.
      *              *-------------------------------------------------*
      *              * Determinazione nome del direttorio di base      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estrazione pathname di base per spool-files *
      *                  * per richiesta al modulo di segreteria       *
      *                  *---------------------------------------------*
           move      ".S"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione del pathname ottenuto        *
      *                  *---------------------------------------------*
           move      s-pat                to   w-det-pat-fde-dir      .
       det-pat-fde-300.
      *              *-------------------------------------------------*
      *              * Determinazione nome del file                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione anno del lotto di fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-det-pat-fde-lfd
                    (02 : 02)             to   w-det-pat-fde-lfa      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero del lotto di fattura- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-det-pat-fde-lfd
                    (04 : 04)             to   w-det-pat-fde-lfn      .
      *                  *---------------------------------------------*
      *                  * Composizione di una stringa composta come   *
      *                  * segue :                                     *
      *                  *                                             *
      *                  *        Fnnnnbb.FDE                          *
      *                  *                                             *
      *                  * dove : F          = prefisso fisso          *
      *                  *        nnnn       = progressivo lotto di    *
      *                  *                     fatturazione            *
      *                  *                     la presentazione        *
      *                  *        bb         = anno del lotto di fat-  *
      *                  *                     turazione               *
      *                  *        .FDE       = estensione fissa        *
      *                  *                                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-pat-fde-nam      .
           string    "F"        delimited by   size
                     w-det-pat-fde-lfn
                                delimited by   size
                     w-det-pat-fde-lfa
                                delimited by   size
                     ".FDE"     delimited by   size
                                          into w-det-pat-fde-nam      .
       det-pat-fde-600.
      *              *-------------------------------------------------*
      *              * Determinazione pathname completo del file       *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-pat-fde-pat      .
           string    w-det-pat-fde-dir
                                delimited by   spaces
                     "/"
                                delimited by   size
                     w-det-pat-fde-nam
                                delimited by   spaces
                                          into w-det-pat-fde-pat      .
       det-pat-fde-999.
           exit.

      *    *===========================================================*
      *    * Preparazione del file di appoggio records fatture da e-   *
      *    * mettere                                                   *
      *    *-----------------------------------------------------------*
       pre-fil-app-000.
           sort      srt                  on   ascending srt-key
                     input  procedure     is   pre-srt-fil-000
                                          thru pre-srt-fil-999
                     output procedure     is   pre-fil-fde-000
                                          thru pre-fil-fde-999        .
       pre-fil-app-999.
           exit.

      *    *===========================================================*
      *    * Preparazione del sort file bolle da fatturare             *
      *    *-----------------------------------------------------------*
       pre-srt-fil-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
       pre-srt-fil-050.
      *              *-------------------------------------------------*
      *              * Start su file [bit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "FATARC    "         to   f-key                  .
           move      "S"                  to   rf-bit-fat-snx         .
           move      zero                 to   rf-bit-fat-dat         .
           move      zero                 to   rf-bit-fat-num         .
           move      zero                 to   rf-bit-fat-npb         .
           move      "C"                  to   rf-bit-tip-arc         .
           move      rr-cod-cli           to   rf-bit-arc-plf         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-999.
       pre-srt-fil-100.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bit]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-999.
       pre-srt-fil-105.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bit-fat-snx       not  = "S"  or
                     rf-bit-fat-dat       not  = zero or
                     rf-bit-fat-num       not  = zero or
                     rf-bit-fat-npb       not  = zero or
                     rf-bit-tip-arc       not  = "C"
                     go to pre-srt-fil-999.
           if        rr-cod-cli           =    zero
                     go to pre-srt-fil-110.
           if        rf-bit-arc-plf       not  = rr-cod-cli
                     go to pre-srt-fil-999.
       pre-srt-fil-110.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza da fatturare      *
      *                  *---------------------------------------------*
           if        rr-cod-dpz           =    zero
                     go to pre-srt-fil-115.
           if        rf-bit-cod-dpz       not  = rr-cod-dpz
                     go to pre-srt-fil-100.
       pre-srt-fil-115.
      *                  *---------------------------------------------*
      *                  * Test su codice agente                       *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     go to pre-srt-fil-120.
           if        rf-bit-cod-age       =    rr-cod-age
                     go to pre-srt-fil-120
           else      go to pre-srt-fil-100.
       pre-srt-fil-120.
      *                  *---------------------------------------------*
      *                  * Test su movimento che interessa la fattura- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        rf-bit-int-ftr       not  = 02
                     go to pre-srt-fil-100.
       pre-srt-fil-122.
      *                  *---------------------------------------------*
      *                  * Test su segnale di si/no momentanea esclu-  *
      *                  * sione della bolla dalla fatturazione dif-   *
      *                  * ferita automatica                           *
      *                  *---------------------------------------------*
           if        rf-bit-flg-nbx (1)   =    spaces
                     go to pre-srt-fil-125.
      *                      *-----------------------------------------*
      *                      * Messaggio di avviso                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing numero documento            *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-bit-num-doc (6:6) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Preparazione messaggio 1            *
      *                          *-------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      rf-bit-cod-tmb       to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "del"                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * Editing data documento              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-bit-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Preparazione messaggio 2            *
      *                          *-------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "momentaneamente sospesa dalla fatturazione differi
      -              "ta"                 to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * Scrittura messaggio                 *
      *                          *-------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     pre-srt-fil-100.
       pre-srt-fil-125.
      *                  *---------------------------------------------*
      *                  * Test su tipo movimento per la fatturazione  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spazi : selezione non superata     *
      *                      *-----------------------------------------*
           if        rf-bit-tmo-ftr       =    spaces
                     go to pre-srt-fil-100.
      *                      *-----------------------------------------*
      *                      * Confronto con il tipo movimento da se-  *
      *                      * lezionare espresso nelle richieste      *
      *                      *-----------------------------------------*
           if        rr-cod-tmo           =    spaces
                     go to pre-srt-fil-126.
           if        rf-bit-tmo-ftr       not  = rr-cod-tmo
                     go to pre-srt-fil-100.
       pre-srt-fil-126.
      *                      *-----------------------------------------*
      *                      * Lettura archivio [zfi]                  *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      rf-bit-tmo-ftr       to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : selezione non supe- *
      *                      * rata                                    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-100.
      *                      *-----------------------------------------*
      *                      * Se documento valido per una sola dipen- *
      *                      * denza                                   *
      *                      *-----------------------------------------*
           if        rf-zfi-vld-dpz       not  = 02
                     go to pre-srt-fil-128.
      *                          *-------------------------------------*
      *                          * Controllo che la dipendenza per cui *
      *                          * il documento e' valido sia pari a   *
      *                          * quella di emissione                 *
      *                          *-------------------------------------*
           if        rf-zfi-cod-dpz       not  = rr-dpz-emi
                     go to pre-srt-fil-100.
       pre-srt-fil-128.
      *                      *-----------------------------------------*
      *                      * Test su tipo documento : che sia 'Fat-  *
      *                      * tura'                                   *
      *                      *-----------------------------------------*
           if        rf-zfi-tip-doc       not  = 01
                     go to pre-srt-fil-100.
      *                  *---------------------------------------------*
      *                  * Test su data documento da fatturare minima  *
      *                  * solo se simulazione in stampa               *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    2
                     go to pre-srt-fil-129.
           if        rr-dat-ddi           =    zero
                     go to pre-srt-fil-129.
           if        rf-bit-dat-doc       <    rr-dat-ddi
                     go to pre-srt-fil-100.
       pre-srt-fil-129.
      *                  *---------------------------------------------*
      *                  * Test su data documento da fatturare massima *
      *                  *---------------------------------------------*
           if        rr-dat-ddf           =    zero
                     go to pre-srt-fil-130.
           if        rf-bit-dat-doc       >    rr-dat-ddf
                     go to pre-srt-fil-100.
       pre-srt-fil-130.
      *                  *---------------------------------------------*
      *                  * Selezione su codice cliente                 *
      *                  *---------------------------------------------*
           if        rr-cod-cli-sns       =    zero
                     go to pre-srt-fil-132
           else      go to pre-srt-fil-134.
       pre-srt-fil-132.
           if        rr-cod-cli           not  = zero   and
                     rf-bit-arc-plf       not  = rr-cod-cli
                     go to pre-srt-fil-999
           else      go to pre-srt-fil-138.
       pre-srt-fil-134.
           move      zero                 to   w-acc-ser-edd-c01      .
       pre-srt-fil-136.
           add       1                    to   w-acc-ser-edd-c01      .
           if        w-acc-ser-edd-c01    >    w-acc-ser-edd-max
                     go to pre-srt-fil-100.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    zero
                     go to pre-srt-fil-100.
           if        rr-cod-cli-eco
                    (w-acc-ser-edd-c01)   =    rf-bit-arc-plf
                     go to pre-srt-fil-138
           else      go to pre-srt-fil-136.
       pre-srt-fil-138.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc] relativo al cliente    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-bit-arc-plf       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to pre-srt-fil-140.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing codice cliente              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-dcc-cod-cli       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Scrittura messaggio                 *
      *                          *-------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ", manca l'anagrafica commerciale"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                          *-------------------------------------*
      *                          * A Restart su [bit] per prossimo cli-*
      *                          * ente                                *
      *                          *-------------------------------------*
           go to     pre-srt-fil-800.
       pre-srt-fil-140.
      *                  *---------------------------------------------*
      *                  * Selezioni su record [dcc]                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su periodicita' di fatturazione,   *
      *                      * se non superato a Restart su [bit] per  *
      *                      * prossimo cliente                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se richiesta fatturazione di un so- *
      *                          * lo cliente : test superato          *
      *                          *-------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to pre-srt-fil-200.
      *                          *-------------------------------------*
      *                          * Se richiesta a zero : test superato *
      *                          *-------------------------------------*
           if        rr-per-fat           =    zero
                     go to pre-srt-fil-200.
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           if        rf-dcc-per-fat       =    rr-per-fat
                     go to pre-srt-fil-200
           else      go to pre-srt-fil-800.
       pre-srt-fil-200.
      *                  *---------------------------------------------*
      *                  * Lettura record [cli] relativo al cliente    *
      *                  *---------------------------------------------*
           move      rf-bit-arc-plf       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                      *-----------------------------------------*
      *                      * Se record non esistente                 *
      *                      *-----------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to pre-srt-fil-210.
      *                          *-------------------------------------*
      *                          * Editing codice cliente              *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-bit-arc-plf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Scrittura messaggio                 *
      *                          *-------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ", manca l'anagrafica contabile"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                          *-------------------------------------*
      *                          * A Restart su [bit] per prossimo     *
      *                          * cliente                             *
      *                          *-------------------------------------*
           go to     pre-srt-fil-800.
       pre-srt-fil-210.
      *                  *---------------------------------------------*
      *                  * Controllo assoggettamento iva cliente       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da eseguire                     *
      *                      *-----------------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to pre-srt-fil-290.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo assog-  *
      *                      * gettamento iva del cliente              *
      *                      *-----------------------------------------*
           if        rf-dcc-tas-ivc       =    01
                     go to pre-srt-fil-220
           else if   rf-dcc-tas-ivc       =    02
                     go to pre-srt-fil-240
           else if   rf-dcc-tas-ivc       =    03
                     go to pre-srt-fil-260
           else if   rf-dcc-tas-ivc       =    04
                     go to pre-srt-fil-280.
       pre-srt-fil-220.
      *                      *-----------------------------------------*
      *                      * Tipo assoggettamento iva cliente : sem- *
      *                      * pre soggetto ad iva                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura a zero                    *
      *                          *-------------------------------------*
           move      zero                 to   rf-bit-ass-iva         .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     pre-srt-fil-290.
       pre-srt-fil-240.
      *                      *-----------------------------------------*
      *                      * Tipo assoggettamento iva cliente : sem- *
      *                      * pre esente da iva                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Controllo che il codice iva in a-   *
      *                          * nagrafica cliente sia un titolo di  *
      *                          * esenzione                           *
      *                          *-------------------------------------*
           move      w-let-arc-cli-ass    to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to pre-srt-fil-242.
      *                              *---------------------------------*
      *                              * Editing codice cliente          *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-bit-arc-plf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Scrittura messaggio             *
      *                              *---------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ", assoggettamento iva errato in anagrafica contabi-
      -              "le"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                              *---------------------------------*
      *                              * A Restart su [bit] per prossimo *
      *                              * cliente                         *
      *                              *---------------------------------*
           go to     pre-srt-fil-800.
       pre-srt-fil-242.
      *                          *-------------------------------------*
      *                          * Bufferizzazione assoggettamento iva *
      *                          *-------------------------------------*
           move      w-let-arc-cli-ass    to   rf-bit-ass-iva         .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     pre-srt-fil-290.
       pre-srt-fil-260.
      *                      *-----------------------------------------*
      *                      * Tipo assoggettamento iva cliente : con  *
      *                      * plafond                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su personalizzazione relativa  *
      *                          * alla gestione lettera d'intenti     *
      *                          *-------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to pre-srt-fil-262.
      *                          *-------------------------------------*
      *                          * Lettura record [lic]                *
      *                          *-------------------------------------*
           move      rf-bit-arc-plf       to   w-let-arc-lic-cod      .
           perform   let-arc-lic-000      thru let-arc-lic-999        .
      *                          *-------------------------------------*
      *                          * Se data documento non compresa tra  *
      *                          * data riferimento iniziale e finale, *
      *                          * si procede come per cliente sempre  *
      *                          * soggetto                            *
      *                          *-------------------------------------*
           if        rf-bit-dat-doc       not  < w-let-arc-lic-dri and
                     rf-bit-dat-doc       not  > w-let-arc-lic-drf
                     go to pre-srt-fil-262.
      *                              *---------------------------------*
      *                              * Preparazione messaggio          *
      *                              *---------------------------------*
           move      spaces               to   w-pre-srt-fil-x80      .
      *                                  *-----------------------------*
      *                                  * Editing numero bolla        *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-bit-num-doc (6:6) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Concatenamento              *
      *                                  *-----------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "Bolla"              to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "non ricade entro gli estremi della lettera d'inten
      -              "ti"                 to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                                  *-----------------------------*
      *                                  * Editing codice cliente      *
      *                                  *-----------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-bit-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                  *-----------------------------*
      *                                  * Concatenamento              *
      *                                  *-----------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "-Cl.:"              to   w-all-str-cat (2)      .
           move      v-edt                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                                  *-----------------------------*
      *                                  * Messaggio                   *
      *                                  *-----------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                              *---------------------------------*
      *                              * Si procede come per cliente     *
      *                              * sempre soggetto                 *
      *                              *---------------------------------*
           go to     pre-srt-fil-220.
       pre-srt-fil-262.
      *                          *-------------------------------------*
      *                          * Controllo che il codice iva in a-   *
      *                          * nagrafica cliente sia un titolo di  *
      *                          * esenzione                           *
      *                          *-------------------------------------*
           move      w-let-arc-cli-ass    to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to pre-srt-fil-264.
      *                              *---------------------------------*
      *                              * Editing codice cliente          *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-bit-arc-plf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Scrittura messaggio             *
      *                              *---------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Cliente : "
                                delimited by   size
                     v-edt
                                delimited by   spaces
                     ", assoggettamento iva errato in anagrafica contabi-
      -              "le"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                              *---------------------------------*
      *                              * A Restart su [bit] per prossimo *
      *                              * cliente                         *
      *                              *---------------------------------*
           go to     pre-srt-fil-800.
       pre-srt-fil-264.
      *                          *-------------------------------------*
      *                          * Bufferizzazione assoggettamento iva *
      *                          *-------------------------------------*
           move      w-let-arc-cli-ass    to   rf-bit-ass-iva         .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     pre-srt-fil-290.
       pre-srt-fil-280.
      *                      *-----------------------------------------*
      *                      * Tipo assoggettamento iva cliente : in   *
      *                      * regime di scissione pagamenti           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     pre-srt-fil-290.
       pre-srt-fil-290.
      *              *-------------------------------------------------*
      *              * Verifica se le voci descrittiva in bolla sono   *
      *              * legate ad una specifica spesa: in tal caso si   *
      *              * controlla che la spesa sia addebitata           *
      *              *-------------------------------------------------*
           perform   pre-srt-fil-vds-000  thru pre-srt-fil-vds-999    .
       pre-srt-fil-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero righe contenute nel do-   *
      *              * cumento                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore righe             *
      *                  *---------------------------------------------*
           move      zero                 to   w-pre-srt-fil-ctr      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione numero primo protocollo    *
      *                  * conferma d'ordine cliente incontrata        *
      *                  *---------------------------------------------*
           move      zero                 to   w-pre-srt-fil-pco      .
      *                  *---------------------------------------------*
      *                  * Start su file [bir]                         *
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
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-340.
       pre-srt-fil-320.
      *                  *---------------------------------------------*
      *                  * Indicatore di programma in esecuzione       *
      *                  *---------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale record [bir]            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-340.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to pre-srt-fil-340.
      *                  *---------------------------------------------*
      *                  * Aggiornamento comodi di visualizzazione     *
      *                  *---------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-bir-qta-ven       to   w-edt-qta-inc-qta      .
           move      rf-bir-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento protocollo prima conferma or- *
      *                  * dine cliente incontrata                     *
      *                  *---------------------------------------------*
           if        w-pre-srt-fil-pco    =    zero
                     move  rf-bir-coc-prt to   w-pre-srt-fil-pco      .
      *                  *---------------------------------------------*
      *                  * Incremento contatore righe del documento    *
      *                  *---------------------------------------------*
           if        w-pre-srt-fil-ctr    <    99999
                     add   1              to   w-pre-srt-fil-ctr      .
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale [bir]        *
      *                  *---------------------------------------------*
           go to     pre-srt-fil-320.
       pre-srt-fil-340.
      *                  *---------------------------------------------*
      *                  * Test su contatore righe documento           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se zero righe : riciclo su prossimo re- *
      *                      * cord [bit]                              *
      *                      *-----------------------------------------*
           if        w-pre-srt-fil-ctr    =    zero
                     go to pre-srt-fil-100.
       pre-srt-fil-380.
      *              *-------------------------------------------------*
      *              * Normalizzazione record per Sort                 *
      *              *-------------------------------------------------*
           move      zero                 to   srt-cli-plf            .
           move      spaces               to   srt-cod-lng            .
           move      spaces               to   srt-snx-fts            .
           move      zero                 to   srt-coc-prt-011        .
           move      zero                 to   srt-tip-frn            .
           move      zero                 to   srt-cod-cli-tpf        .
           move      spaces               to   srt-dpz-cli-tpf        .
           move      spaces               to   srt-dpc-plf-003        .
           move      zero                 to   srt-dat-doc-003        .
           move      zero                 to   srt-num-doc-003        .
           move      spaces               to   srt-tmo-ftr-1o2        .
           move      spaces               to   srt-sgl-vpf-1o2        .
           move      zero                 to   srt-dec-vpf-1o2        .
           move      spaces               to   srt-tdc-vpf-1o2        .
           move      zero                 to   srt-ass-iva-1o2        .
           move      zero                 to   srt-cod-age-1o2        .
           move      zero                 to   srt-fsp-doc-1o2        .
           move      zero                 to   srt-pvf-age-1o2        .
           move      zero                 to   srt-tip-vpa-1o2        .
           move      zero                 to   srt-cod-ime-1o2        .
           move      spaces               to   srt-snx-pfi-1o2        .
           move      spaces               to   srt-dpc-plf-002        .
           move      zero                 to   srt-inl-pgt-1o2        .
           move      zero                 to   srt-cod-fop-1o2        .
           move      spaces               to   srt-snx-qaf-1o2        .
           move      spaces               to   srt-snx-dsm-1o2        .
           move      zero                 to   srt-cod-abi-1o2        .
           move      zero                 to   srt-cod-cab-1o2        .
           move      spaces               to   srt-ccc-app-1o2        .
           move      zero                 to   srt-pag-dsm-1o2        .
           move      spaces               to   srt-add-spi-1o2        .
           move      spaces               to   srt-add-spb-1o2        .
           move      zero                 to   srt-spe-snx-1o2 (1)    .
           move      zero                 to   srt-spe-snx-1o2 (2)    .
           move      zero                 to   srt-spe-snx-1o2 (3)    .
           move      zero                 to   srt-spe-snx-1o2 (4)    .
           move      zero                 to   srt-spe-snx-1o2 (5)    .
           move      zero                 to   srt-spe-snx-1o2 (6)    .
           move      spaces               to   srt-dpc-plf-001        .
           move      zero                 to   srt-dat-doc            .
           move      zero                 to   srt-num-doc            .
           move      zero                 to   srt-prt-bit            .
           move      zero                 to   srt-rag-bft            .
           move      zero                 to   srt-tas-ivc            .
           move      zero                 to   srt-ass-iva            .
           move      zero                 to   srt-inl-dcm            .
           move      zero                 to   srt-tip-esm            .
           move      zero                 to   srt-ggg-alt            .
           move      zero                 to   srt-mmm-e01            .
           move      zero                 to   srt-mmm-e02            .
           move      spaces               to   srt-snx-cts            .
           move      zero                 to   srt-cod-abi            .
           move      zero                 to   srt-cod-cab            .
           move      spaces               to   srt-ccc-app            .
           move      zero                 to   srt-ctr-bir            .
       pre-srt-fil-400.
      *              *-------------------------------------------------*
      *              * Composizione record di Sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiave del sort                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente per fatturazione         *
      *                      *-----------------------------------------*
           move      rf-bit-arc-plf       to   srt-cli-plf            .
      *                      *-----------------------------------------*
      *                      * Codice lingua                           *
      *                      *-----------------------------------------*
           move      rf-bit-cod-lng       to   srt-cod-lng            .
      *                      *-----------------------------------------*
      *                      * Fatturazione separata per il documento  *
      *                      *-----------------------------------------*
           if        rf-bit-fat-sep       =    "S"
                     move  "S"            to   srt-snx-fts
           else if   rf-bit-fat-sep       =    "N"
                     move  spaces         to   srt-snx-fts
           else      move  spaces         to   srt-snx-fts            .
      *                      *-----------------------------------------*
      *                      * Fatturazione separata per il documento  *
      *                      *                                         *
      *                      * Indicatore anagrafico                   *
      *                      *-----------------------------------------*
           move      rf-dcc-rag-bft       to   srt-rag-bft            .
           if        rf-bit-fat-sep       =    "O"
                     move  11             to   srt-rag-bft            .
      *                      *-----------------------------------------*
      *                      * Numero protocollo conferma d'ordine     *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           if        rf-dcc-rag-bft       =    11 or
                     rf-bit-fat-sep       =    "O"
                     move  w-pre-srt-fil-pco
                                          to   srt-coc-prt-011
           else      move  zero           to   srt-coc-prt-011        .
      *                      *-----------------------------------------*
      *                      * Tipo fornitura                          *
      *                      *-----------------------------------------*
           move      rf-bit-tip-frn       to   srt-tip-frn            .
      *                      *-----------------------------------------*
      *                      * Codice e dipendenza del cliente per     *
      *                      * tipo fornitura                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo for-  *
      *                          * nitura                              *
      *                          *-------------------------------------*
           if        rf-bit-tip-frn       =    01
                     go to pre-srt-fil-402
           else if   rf-bit-tip-frn       =    11
                     go to pre-srt-fil-404
           else if   rf-bit-tip-frn       =    21
                     go to pre-srt-fil-406.
       pre-srt-fil-402.
      *                          *-------------------------------------*
      *                          * Tipo fornitura : 01 Diretta         *
      *                          *-------------------------------------*
           move      zero                 to   srt-cod-cli-tpf        .
           move      spaces               to   srt-dpz-cli-tpf        .
           go to     pre-srt-fil-408.
       pre-srt-fil-404.
      *                          *-------------------------------------*
      *                          * Tipo fornitura : 11 Tramite Leasing *
      *                          *-------------------------------------*
           move      rf-bit-cod-arc       to   srt-cod-cli-tpf        .
           move      rf-bit-dpz-arc       to   srt-dpz-cli-tpf        .
           go to     pre-srt-fil-408.
       pre-srt-fil-406.
      *                          *-------------------------------------*
      *                          * Tipo fornitura : 21 Tramite Gruppo  *
      *                          * d'Acquisto                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Valore in funzione del tipo     *
      *                              * fatturazione                    *
      *                              *---------------------------------*
           if        rf-bit-tip-ftz       =    01
                     move  rf-bit-cod-arc to   srt-cod-cli-tpf
                     move  rf-bit-dpz-arc to   srt-dpz-cli-tpf
           else if   rf-bit-tip-ftz       =    02
                     move  zero           to   srt-cod-cli-tpf
                     move  spaces         to   srt-dpz-cli-tpf        .
           go to     pre-srt-fil-408.
       pre-srt-fil-408.
      *                      *-----------------------------------------*
      *                      * Altri valori per chiave del Sort        *
      *                      *-----------------------------------------*
           if        rf-dcc-rag-bft       not  = 03
                     go to pre-srt-fil-410.
           move      rf-bit-dpz-plf       to   srt-dpc-plf-003        .
           move      rf-bit-dat-doc       to   srt-dat-doc-003        .
           move      rf-bit-num-doc       to   srt-num-doc-003        .
       pre-srt-fil-410.
           if        rf-dcc-rag-bft       not  = 01 and
                     rf-dcc-rag-bft       not  = 02
                     go to pre-srt-fil-420.
           move      rf-bit-tmo-ftr       to   srt-tmo-ftr-1o2        .
           move      rf-bit-ass-iva       to   srt-ass-iva-1o2        .
           move      rf-bit-sgl-vpf       to   srt-sgl-vpf-1o2        .
           move      rf-bit-dec-vpf       to   srt-dec-vpf-1o2        .
           move      rf-bit-tdc-vpf       to   srt-tdc-vpf-1o2        .
           move      rf-bit-cod-age       to   srt-cod-age-1o2        .
           move      rf-bit-fsp-doc       to   srt-fsp-doc-1o2        .
           move      rf-bit-pvf-age       to   srt-pvf-age-1o2        .
           move      rf-bit-tip-vpa       to   srt-tip-vpa-1o2        .
           move      rf-bit-cod-ime       to   srt-cod-ime-1o2        .
           if        rf-bit-pvf-ime       not  = zero
                     move  "S"            to   srt-snx-pfi-1o2        .
       pre-srt-fil-420.
           if        rf-dcc-rag-bft       not  = 02
                     go to pre-srt-fil-430.
           move      rf-bit-dpz-plf       to   srt-dpc-plf-002        .
       pre-srt-fil-430.
           if        rf-dcc-rag-bft       not  = 01 and
                     rf-dcc-rag-bft       not  = 02
                     go to pre-srt-fil-440.
           move      rf-bit-inl-pgt       to   srt-inl-pgt-1o2        .
           move      rf-bit-cod-fop       to   srt-cod-fop-1o2        .
           if        rf-bit-pag-qaf       not  = zero
                     move  "S"            to   srt-snx-qaf-1o2        .
           if        rf-bit-pag-dsm       not  = zero
                     move  "S"            to   srt-snx-dsm-1o2        .
           move      rf-bit-cod-abi       to   srt-cod-abi-1o2        .
           move      rf-bit-cod-cab       to   srt-cod-cab-1o2        .
           move      rf-bit-ccc-app       to   srt-ccc-app-1o2        .
           move      rf-bit-pag-dsm       to   srt-pag-dsm-1o2        .
           move      rf-bit-add-spi       to   srt-add-spi-1o2        .
           move      rf-bit-add-spb       to   srt-add-spb-1o2        .
           if        w-prs-tac-spf        =    01
                     move  rf-bit-spe-snx (1)
                                          to   srt-spe-snx-1o2 (1)
                     move  rf-bit-spe-snx (2)
                                          to   srt-spe-snx-1o2 (2)
                     move  rf-bit-spe-snx (3)
                                          to   srt-spe-snx-1o2 (3)
                     move  rf-bit-spe-snx (4)
                                          to   srt-spe-snx-1o2 (4)
                     move  rf-bit-spe-snx (5)
                                          to   srt-spe-snx-1o2 (5)
                     move  rf-bit-spe-snx (6)
                                          to   srt-spe-snx-1o2 (6)    .
       pre-srt-fil-440.
           if        rf-dcc-rag-bft       not  = 01
                     go to pre-srt-fil-450.
           move      rf-bit-dpz-plf       to   srt-dpc-plf-001        .
       pre-srt-fil-450.
           move      rf-bit-dat-doc       to   srt-dat-doc            .
           move      rf-bit-num-doc       to   srt-num-doc            .
      *                  *---------------------------------------------*
      *                  * Dati del sort                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice e dipendenza del cliente         *
      *                      *                                         *
      *                      * - Se tipo fornitura 01 Diretta oppure   *
      *                      *      tipo fornitura 11 Tramite Leasing  *
      *                      *      srt-cod-cli = rf-bit-cod-arc       *
      *                      *      srt-dpz-cli = rf-bit-dpz-arc       *
      *                      * - Se tipo fornitura 21 Tramite Gruppo   *
      *                      *   d'Acquisto)                           *
      *                      *   - Se tipo fatturazione 01 Separata    *
      *                      *      srt-cod-cli = rf-bit-cod-arc       *
      *                      *      srt-dpz-cli = rf-bit-dpz-arc       *
      *                      *   - Se tipo fatturazione 02 Cumulativa  *
      *                      *      srt-cod-cli = rf-bit-arc-plf       *
      *                      *      srt-dpz-cli = rf-bit-dpz-plf       *
      *                      *-----------------------------------------*
           if        rf-bit-tip-frn       =    21 and
                     rf-bit-tip-ftz       =    02
                     move  rf-bit-arc-plf to   srt-cod-cli
                     move  rf-bit-dpz-plf to   srt-dpz-cli
           else      move  rf-bit-cod-arc to   srt-cod-cli
                     move  rf-bit-dpz-arc to   srt-dpz-cli            .
      *                      *-----------------------------------------*
      *                      * Altri dati del sort                     *
      *                      *-----------------------------------------*
           move      rf-bit-num-prt       to   srt-prt-bit            .
           move      rf-dcc-tas-ivc       to   srt-tas-ivc            .
           move      rf-bit-ass-iva       to   srt-ass-iva            .
           move      rf-dcc-inl-dcm       to   srt-inl-dcm            .
           move      rf-dcc-tip-esm       to   srt-tip-esm            .
           move      rf-dcc-ggg-alt       to   srt-ggg-alt            .
           move      rf-dcc-mmm-e01       to   srt-mmm-e01            .
           move      rf-dcc-mmm-e02       to   srt-mmm-e02            .
           move      rf-dcc-cod-abi       to   srt-cod-abi            .
           move      rf-dcc-cod-cab       to   srt-cod-cab            .
           move      rf-dcc-ccc-app       to   srt-ccc-app            .
           move      w-pre-srt-fil-ctr    to   srt-ctr-bir            .
      *              *-------------------------------------------------*
      *              * Rilascio record al Sort                         *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       pre-srt-fil-500.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [bit]       *
      *              *-------------------------------------------------*
           go to     pre-srt-fil-100.
       pre-srt-fil-800.
      *              *-------------------------------------------------*
      *              * Restart su file [bit] sul prossimo cliente      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "GT"                 to   f-cfr                  .
           move      "FATARC    "         to   f-key                  .
           move      "S"                  to   rf-bit-fat-snx         .
           move      zero                 to   rf-bit-fat-dat         .
           move      zero                 to   rf-bit-fat-num         .
           move      zero                 to   rf-bit-fat-npb         .
           move      "C"                  to   rf-bit-tip-arc         .
           move      rf-bit-arc-plf       to   rf-bit-arc-plf         .
           move      99999999999          to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-srt-fil-999.
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura sequenziale record [bit] *
      *                  *---------------------------------------------*
           go to     pre-srt-fil-100.
       pre-srt-fil-999.
           exit.

      *    *===========================================================*
      *    * Preparazione del sort file bolle da fatturare             *
      *    *                                                           *
      *    * Subroutine di controllo incrociato voci descrittive e     *
      *    * spese                                                     *
      *    *                                                           *
      *    * Nota bene: il controllo verifica se alla voce descrittiva *
      *    *           (dcc110) e' associato un codice spesa (dcc030)  *
      *    *-----------------------------------------------------------*
       pre-srt-fil-vds-000.
      *              *-------------------------------------------------*
      *              * Scansione voci descrittive in bolla             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-spe-zvf-ctr      .
       pre-srt-fil-vds-200.
      *                  *---------------------------------------------*
      *                  * Incremento contatore 1..6                   *
      *                  *---------------------------------------------*
           add       1                    to   w-det-spe-zvf-ctr      .
           if        w-det-spe-zvf-ctr    >    6
                     go to pre-srt-fil-vds-900.
      *                  *---------------------------------------------*
      *                  * Test su voce di addebito                    *
      *                  *---------------------------------------------*
           if        rf-bit-voc-des
                    (w-det-spe-zvf-ctr)   =    spaces
                     go to pre-srt-fil-vds-200.
      *                  *---------------------------------------------*
      *                  * Lettura tabella [zvf]                       *
      *                  *---------------------------------------------*
           move      w-det-spe-zvf-ctr    to   w-let-arc-zvf-num      .
           move      rf-bit-voc-des
                    (w-det-spe-zvf-ctr)   to   w-let-arc-zvf-cod      .
           perform   let-arc-zvf-000      thru let-arc-zvf-999        .
      *                  *---------------------------------------------*
      *                  * Test se rilevato un codice spesa            *
      *                  *---------------------------------------------*
           if        w-let-arc-zvf-spf    =    zero
                     go to pre-srt-fil-vds-200.
           if        w-let-arc-zvf-spf    >    6
                     go to pre-srt-fil-vds-200.
      *                  *---------------------------------------------*
      *                  * Test se il codice spesa associato e' adde-  *
      *                  * bitato                                      *
      *                  *---------------------------------------------*
           if        rf-bit-spe-snx
                    (w-let-arc-zvf-spf)   not  = zero  and
                     rf-bit-spe-imp
                    (w-let-arc-zvf-spf)   not  = zero
                     go to pre-srt-fil-vds-200.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing numero documento                *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-bit-num-doc
                    (06 : 06)             to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (3)      .
      *                      *-----------------------------------------*
      *                      * Editing data documento                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-bit-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-edt                to   w-all-str-cat (5)      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
           move      "ATTENZIONE : nella bolla"
                                          to   w-all-str-cat (1)      .
           move      rf-bit-cod-tmb       to   w-all-str-cat (2)      .
           move      "del"                to   w-all-str-cat (4)      .
           move      "manca l'addebito di una spesa!"
                                          to   w-all-str-cat (6)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Scrittura messaggio                     *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura messaggio                *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      all "*"              to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       pre-srt-fil-vds-600.
      *                  *---------------------------------------------*
      *                  * Riciclo a voce descrittiva successiva       *
      *                  *---------------------------------------------*
           go to     pre-srt-fil-vds-200.
       pre-srt-fil-vds-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-srt-fil-vds-999.
       pre-srt-fil-vds-999.
           exit.

      *    *===========================================================*
      *    * Preparazione file per fatture da emettere                 *
      *    *-----------------------------------------------------------*
       pre-fil-fde-000.
      *              *-------------------------------------------------*
      *              * Determinazione pathname line sequential file    *
      *              *-------------------------------------------------*
           move      w-att-fti-lfd-num    to   w-det-pat-fde-lfd      .
           perform   det-pat-fde-000      thru det-pat-fde-999        .
      *              *-------------------------------------------------*
      *              * Open output line sequential file                *
      *              *-------------------------------------------------*
           move      "fde "               to   f-lsf-nam              .
           move      w-det-pat-fde-pat    to   f-lsf-pat              .
           open      output lsf                                       .
       pre-fil-fde-050.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di primo passaggio                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-pre-fil-fde-uno      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area parametri di rottura   *
      *                  *---------------------------------------------*
           move      zero                 to   w-rot-cli-plf          .
           move      spaces               to   w-rot-cod-lng          .
           move      zero                 to   w-rot-coc-prt          .
           move      spaces               to   w-rot-dpc-plf          .
           move      zero                 to   w-rot-tip-frn          .
           move      zero                 to   w-rot-cod-cli          .
           move      spaces               to   w-rot-dpz-cli          .
           move      spaces               to   w-rot-tmo-ftr          .
           move      spaces               to   w-rot-sgl-vpf          .
           move      zero                 to   w-rot-dec-vpf          .
           move      spaces               to   w-rot-tdc-vpf          .
           move      zero                 to   w-rot-ass-iva          .
           move      zero                 to   w-rot-cod-age          .
           move      zero                 to   w-rot-fsp-doc          .
           move      zero                 to   w-rot-pvf-age          .
           move      zero                 to   w-rot-tip-vpa          .
           move      zero                 to   w-rot-cod-ime          .
           move      spaces               to   w-rot-snx-pfi          .
           move      zero                 to   w-rot-inl-pgt          .
           move      zero                 to   w-rot-cod-fop          .
           move      spaces               to   w-rot-snx-qaf          .
           move      spaces               to   w-rot-snx-dsm          .
           move      zero                 to   w-rot-cod-abi          .
           move      zero                 to   w-rot-cod-cab          .
           move      spaces               to   w-rot-ccc-app          .
           move      zero                 to   w-rot-pag-dsm          .
           move      spaces               to   w-rot-add-spi          .
           move      spaces               to   w-rot-add-spb          .
           move      zero                 to   w-rot-spe-snx (1)      .
           move      zero                 to   w-rot-spe-snx (2)      .
           move      zero                 to   w-rot-spe-snx (3)      .
           move      zero                 to   w-rot-spe-snx (4)      .
           move      zero                 to   w-rot-spe-snx (5)      .
           move      zero                 to   w-rot-spe-snx (6)      .
           move      zero                 to   w-rot-ctr-rig          .
           move      zero                 to   w-rot-ctr-tes          .
       pre-fil-fde-100.
      *              *-------------------------------------------------*
      *              * Lettura record da sort                          *
      *              *-------------------------------------------------*
           return    srt   at end
                     go to pre-fil-fde-900.
      *              *-------------------------------------------------*
      *              * Rilettura record [bit]                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-prt-bit          to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Se errore : riciclo                             *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-fil-fde-100.
      *              *-------------------------------------------------*
      *              * Test se rottura fattura                         *
      *              *-------------------------------------------------*
           perform   tst-rot-fat-000      thru tst-rot-fat-999        .
      *              *-------------------------------------------------*
      *              * Se rottura                                      *
      *              *-------------------------------------------------*
           if        w-tst-rot-fat-snx    =    "N"
                     go to pre-fil-fde-120.
      *                  *---------------------------------------------*
      *                  * Fine fattura precedente                     *
      *                  *---------------------------------------------*
           perform   rou-fin-fat-000      thru rou-fin-fat-999        .
      *                  *---------------------------------------------*
      *                  * Inizio nuova fattura                        *
      *                  *---------------------------------------------*
           perform   rou-ini-fat-000      thru rou-ini-fat-999        .
       pre-fil-fde-120.
      *              *-------------------------------------------------*
      *              * Aggiornamento totali righe fattura              *
      *              *-------------------------------------------------*
           perform   agg-tri-fat-000      thru agg-tri-fat-999        .
       pre-fil-fde-200.
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo record di Sort              *
      *              *-------------------------------------------------*
           go to     pre-fil-fde-100.
       pre-fil-fde-900.
      *              *-------------------------------------------------*
      *              * Fine ultima fattura                             *
      *              *-------------------------------------------------*
           perform   rou-fin-fat-000      thru rou-fin-fat-999        .
      *              *-------------------------------------------------*
      *              * Close line sequential file                      *
      *              *-------------------------------------------------*
           close     lsf                                              .
       pre-fil-fde-999.
           exit.

      *    *===========================================================*
      *    * Test se rottura fattura                                   *
      *    *-----------------------------------------------------------*
       tst-rot-fat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-tst-rot-fat-snx      .
      *              *-------------------------------------------------*
      *              * Test su codice cliente                          *
      *              *-------------------------------------------------*
           if        srt-cli-plf          not  = w-rot-cli-plf
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Test su codice lingua                           *
      *              *-------------------------------------------------*
           if        srt-cod-lng          not  = w-rot-cod-lng
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Test su fatturazione separata per il documento  *
      *              *-------------------------------------------------*
           if        srt-snx-fts          =   "S"
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Test su tipo fornitura                          *
      *              *-------------------------------------------------*
           if        srt-tip-frn          not  = w-rot-tip-frn
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Test su codice cliente per tipo fornitura       *
      *              *-------------------------------------------------*
           if        srt-cod-cli-tpf      not  = w-rot-cod-cli
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Test su codice dipendenza cliente per tipo for- *
      *              * nitura                                          *
      *              *-------------------------------------------------*
           if        srt-dpz-cli-tpf      not  = w-rot-dpz-cli
                     go to tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo raggruppamento  *
      *              * bolle in fattura                                *
      *              *-------------------------------------------------*
           if        srt-rag-bft          =    01
                     go to tst-rot-fat-100
           else if   srt-rag-bft          =    02
                     go to tst-rot-fat-200
           else if   srt-rag-bft          =    03
                     go to tst-rot-fat-300
           else if   srt-rag-bft          =    11
                     go to tst-rot-fat-500
           go to     tst-rot-fat-300.
       tst-rot-fat-100.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una unica fattura per tut-*
      *              *                       te le bolle e per qual-   *
      *              *                       siasi destinatario del    *
      *              *                       cliente                   *
      *              *-------------------------------------------------*
           if        srt-tmo-ftr-1o2      not  = w-rot-tmo-ftr or
                     srt-sgl-vpf-1o2      not  = w-rot-sgl-vpf or
                     srt-dec-vpf-1o2      not  = w-rot-dec-vpf or
                     srt-tdc-vpf-1o2      not  = w-rot-tdc-vpf or
                     srt-ass-iva-1o2      not  = w-rot-ass-iva or
                     srt-cod-age-1o2      not  = w-rot-cod-age or
                     srt-fsp-doc-1o2      not  = w-rot-fsp-doc or
                     srt-pvf-age-1o2      not  = w-rot-pvf-age or
                     srt-tip-vpa-1o2      not  = w-rot-tip-vpa or
                     srt-cod-ime-1o2      not  = w-rot-cod-ime or
                     srt-snx-pfi-1o2      not  = w-rot-snx-pfi or
                     srt-inl-pgt-1o2      not  = w-rot-inl-pgt or
                     srt-cod-fop-1o2      not  = w-rot-cod-fop or
                     srt-snx-qaf-1o2      not  = w-rot-snx-qaf or
                     srt-snx-dsm-1o2      not  = w-rot-snx-dsm or
                     srt-cod-abi-1o2      not  = w-rot-cod-abi or
                     srt-cod-cab-1o2      not  = w-rot-cod-cab or
                     srt-ccc-app-1o2      not  = w-rot-ccc-app or
                     srt-pag-dsm-1o2      not  = w-rot-pag-dsm or
                     srt-add-spi-1o2      not  = w-rot-add-spi or
                     srt-add-spb-1o2      not  = w-rot-add-spb or
                     srt-spe-snx-1o2 (1)  not  = w-rot-spe-snx (1) or
                     srt-spe-snx-1o2 (2)  not  = w-rot-spe-snx (2) or
                     srt-spe-snx-1o2 (3)  not  = w-rot-spe-snx (3) or
                     srt-spe-snx-1o2 (4)  not  = w-rot-spe-snx (4) or
                     srt-spe-snx-1o2 (5)  not  = w-rot-spe-snx (5) or
                     srt-spe-snx-1o2 (6)  not  = w-rot-spe-snx (6)
                     go to tst-rot-fat-900
           else      go to tst-rot-fat-800.
       tst-rot-fat-200.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni de-  *
      *              *                       stinatario del cliente    *
      *              *-------------------------------------------------*
           if        srt-tmo-ftr-1o2      not  = w-rot-tmo-ftr or
                     srt-sgl-vpf-1o2      not  = w-rot-sgl-vpf or
                     srt-dec-vpf-1o2      not  = w-rot-dec-vpf or
                     srt-tdc-vpf-1o2      not  = w-rot-tdc-vpf or
                     srt-ass-iva-1o2      not  = w-rot-ass-iva or
                     srt-cod-age-1o2      not  = w-rot-cod-age or
                     srt-fsp-doc-1o2      not  = w-rot-fsp-doc or
                     srt-pvf-age-1o2      not  = w-rot-pvf-age or
                     srt-tip-vpa-1o2      not  = w-rot-tip-vpa or
                     srt-cod-ime-1o2      not  = w-rot-cod-ime or
                     srt-snx-pfi-1o2      not  = w-rot-snx-pfi or
                     srt-dpc-plf-002      not  = w-rot-dpc-plf or
                     srt-inl-pgt-1o2      not  = w-rot-inl-pgt or
                     srt-cod-fop-1o2      not  = w-rot-cod-fop or
                     srt-snx-qaf-1o2      not  = w-rot-snx-qaf or
                     srt-snx-dsm-1o2      not  = w-rot-snx-dsm or
                     srt-cod-abi-1o2      not  = w-rot-cod-abi or
                     srt-cod-cab-1o2      not  = w-rot-cod-cab or
                     srt-ccc-app-1o2      not  = w-rot-ccc-app or
                     srt-pag-dsm-1o2      not  = w-rot-pag-dsm or
                     srt-add-spi-1o2      not  = w-rot-add-spi or
                     srt-add-spb-1o2      not  = w-rot-add-spb or
                     srt-spe-snx-1o2 (1)  not  = w-rot-spe-snx (1) or
                     srt-spe-snx-1o2 (2)  not  = w-rot-spe-snx (2) or
                     srt-spe-snx-1o2 (3)  not  = w-rot-spe-snx (3) or
                     srt-spe-snx-1o2 (4)  not  = w-rot-spe-snx (4) or
                     srt-spe-snx-1o2 (5)  not  = w-rot-spe-snx (5) or
                     srt-spe-snx-1o2 (6)  not  = w-rot-spe-snx (6)
                     go to tst-rot-fat-900
           else      go to tst-rot-fat-800.
       tst-rot-fat-300.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni bol- *
      *              *                       la                        *
      *              *-------------------------------------------------*
           go to     tst-rot-fat-900.
       tst-rot-fat-500.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni or-  *
      *              *                       dine                      *
      *              *-------------------------------------------------*
           if        srt-coc-prt-011      not  = w-rot-coc-prt
                     go to tst-rot-fat-900
           else      go to tst-rot-fat-800.
       tst-rot-fat-800.
      *              *-------------------------------------------------*
      *              * Se non rottura                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero righe della fattura    *
      *                  *---------------------------------------------*
           add       srt-ctr-bir          to   w-rot-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo consentito per una fat- *
      *                  * tura : uscita con flag di rottura a Si      *
      *                  *---------------------------------------------*
           if        w-rot-ctr-rig        >    w-lim-fat-max-rig
                     go to tst-rot-fat-900.
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero bolle nella fattura    *
      *                  *---------------------------------------------*
           add       1                    to   w-rot-ctr-tes          .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo consentito per una fat- *
      *                  * tura : uscita con flag di rottura a Si      *
      *                  *---------------------------------------------*
           if        w-rot-ctr-tes        >    w-lim-fat-max-tes
                     go to tst-rot-fat-900.
       tst-rot-fat-880.
      *              *-------------------------------------------------*
      *              * Uscita con flag di rottura a No                 *
      *              *-------------------------------------------------*
           move      "N"                  to   w-tst-rot-fat-snx      .
           go to     tst-rot-fat-999.
       tst-rot-fat-900.
      *              *-------------------------------------------------*
      *              * Uscita con flag di rottura a Si                 *
      *              *-------------------------------------------------*
           move      "S"                  to   w-tst-rot-fat-snx      .
       tst-rot-fat-999.
           exit.

      *    *===========================================================*
      *    * Routine per fine fattura                                  *
      *    *-----------------------------------------------------------*
       rou-fin-fat-000.
      *              *-------------------------------------------------*
      *              * Se primo passaggio : uscita                     *
      *              *-------------------------------------------------*
           if        w-pre-fil-fde-uno    =    spaces
                     move  "#"            to   w-pre-fil-fde-uno
                     go to rou-fin-fat-999.
      *              *-------------------------------------------------*
      *              * Determinazione codici iva per piede documento   *
      *              *-------------------------------------------------*
           perform   det-civ-pie-000      thru det-civ-pie-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali fattura                   *
      *              *-------------------------------------------------*
           perform   det-tot-fat-000      thru det-tot-fat-999        .
      *              *-------------------------------------------------*
      *              * Scrittura in file di appoggio                   *
      *              *-------------------------------------------------*
           perform   scr-fil-app-000      thru scr-fil-app-999        .
       rou-fin-fat-999.
           exit.

      *    *===========================================================*
      *    * Routine per inizio fattura                                *
      *    *-----------------------------------------------------------*
       rou-ini-fat-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio documento per le sub-   *
      *              * routines per il trattamento del file relative   *
      *              * di appoggio [rlt]                               *
      *              *-------------------------------------------------*
           perform   fil-rlt-new-000      thru fil-rlt-new-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento area per rottura                  *
      *              *-------------------------------------------------*
           perform   agg-rot-are-000      thru agg-rot-are-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione totali fattura                  *
      *              *-------------------------------------------------*
           perform   nor-tot-fat-000      thru nor-tot-fat-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione numero elementi in buffer pro-  *
      *              * tocolli bolle in fattura                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-pbf-num-ele      .
       rou-ini-fat-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori contenuti nella prima    *
      *              * bolla incontrata utili per tutta la fattura     *
      *              * riepilogativa                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Cliente                 *
      *                  *---------------------------------------------*
           move      srt-cod-cli          to   w-vpb-cod-cli          .
           move      rf-bit-cod-lng       to   w-vpb-cod-lng          .
           move      srt-dpz-cli          to   w-vpb-dpz-cli          .
           move      srt-tas-ivc          to   w-vpb-tas-ivc          .
           move      srt-ass-iva          to   w-vpb-ass-iva          .
           move      rf-bit-flg-rfp       to   w-vpb-flg-rfp          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Cliente per fatturazio- *
      *                  *                     ne                      *
      *                  *---------------------------------------------*
           move      rf-bit-tip-frn       to   w-vpb-tip-frn          .
           move      rf-bit-arc-plf       to   w-vpb-cli-plf          .
           move      rf-bit-dpz-plf       to   w-vpb-dpc-plf          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Tipo movimento per fat- *
      *                  *                     tura                    *
      *                  *---------------------------------------------*
           move      rf-bit-tmo-ftr       to   w-vpb-tmo-ftr          .
           perform   det-vpb-tmo-000      thru det-vpb-tmo-999        .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Valuta per fatturazione *
      *                  *---------------------------------------------*
           move      rf-bit-sgl-vpf       to   w-vpb-sgl-vpf          .
           move      rf-bit-dec-vpf       to   w-vpb-dec-vpf          .
           move      rf-bit-tdc-vpf       to   w-vpb-tdc-vpf          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Altri valori testata    *
      *                  *---------------------------------------------*
           move      rf-bit-ctp-ven       to   w-vpb-ctp-ven          .
           move      srt-inl-dcm          to   w-vpb-inl-dcm          .
           move      rf-bit-inl-pgt       to   w-vpb-inl-pgt          .
           move      rf-bit-cod-lst       to   w-vpb-cod-lst          .
           move      rf-bit-csr-aac       to   w-vpb-csr-aac          .
           move      rf-bit-psr-aac (1)   to   w-vpb-psr-aac (1)      .
           move      rf-bit-psr-aac (2)   to   w-vpb-psr-aac (2)      .
           move      rf-bit-psr-aac (3)   to   w-vpb-psr-aac (3)      .
           move      rf-bit-psr-aac (4)   to   w-vpb-psr-aac (4)      .
           move      rf-bit-psr-aac (5)   to   w-vpb-psr-aac (5)      .
           move      rf-bit-csc-aac       to   w-vpb-csc-aac          .
           move      rf-bit-psc-aac       to   w-vpb-psc-aac          .
           move      rf-bit-cpv-aac       to   w-vpb-cpv-aac          .
           move      rf-bit-ppv-aac (1)   to   w-vpb-ppv-aac (1)      .
           move      rf-bit-ppv-aac (2)   to   w-vpb-ppv-aac (2)      .
           move      rf-bit-ppv-aac (3)   to   w-vpb-ppv-aac (3)      .
           move      rf-bit-voc-des (1)   to   w-vpb-voc-des (1)      .
           move      rf-bit-voc-des (2)   to   w-vpb-voc-des (2)      .
           move      rf-bit-voc-des (3)   to   w-vpb-voc-des (3)      .
           move      rf-bit-voc-des (4)   to   w-vpb-voc-des (4)      .
           move      rf-bit-voc-des (5)   to   w-vpb-voc-des (5)      .
           move      rf-bit-voc-des (6)   to   w-vpb-voc-des (6)      .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Agente ed intermediario *
      *                  *---------------------------------------------*
           move      rf-bit-cod-age       to   w-vpb-cod-age          .
           move      rf-bit-fsp-doc       to   w-vpb-fsp-doc          .
           move      rf-bit-tip-vpa       to   w-vpb-tip-vpa          .
           move      rf-bit-cpv-aaa       to   w-vpb-cpv-aaa          .
           move      rf-bit-ppv-aaa (1)   to   w-vpb-ppv-aaa (1)      .
           move      rf-bit-ppv-aaa (2)   to   w-vpb-ppv-aaa (2)      .
           move      rf-bit-ppv-aaa (3)   to   w-vpb-ppv-aaa (3)      .
           move      rf-bit-cod-ime       to   w-vpb-cod-ime          .
           move      rf-bit-pvf-ime       to   w-vpb-pvf-ime          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Pagamento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Subroutine di preparazione dei codici   *
      *                      * ABI e CAB e del conto corrente per      *
      *                      * l'appoggio in base alla personalizza-   *
      *                      * zione                                   *
      *                      *                                         *
      *                      * I valori interessati sono :             *
      *                      *  - 'cod-abi' = Codice Abi               *
      *                      *  - 'cod-cab' = Codice Cab               *
      *                      *  - 'ccc-app' = Conto corrente di appog- *
      *                      *                gio                      *
      *                      *-----------------------------------------*
           perform   rou-ini-fat-abi-000  thru rou-ini-fat-abi-999    .
      *                      *-----------------------------------------*
      *                      * Altri valori                            *
      *                      *-----------------------------------------*
           move      rf-bit-nos-ban       to   w-vpb-nos-ban          .
           move      rf-bit-nos-ccp       to   w-vpb-nos-ccp          .
           move      rf-bit-pag-qaf       to   w-vpb-pag-qaf          .
           move      rf-bit-pag-dsm       to   w-vpb-pag-dsm          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Forma di pagamento      *
      *                  *---------------------------------------------*
           move      rf-bit-cod-fop       to   w-vpb-cod-fop          .
           move      rf-bit-scp-aap       to   w-vpb-scp-aap          .
           perform   det-vpb-fop-000      thru det-vpb-fop-999        .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Sconto in chiusura      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice Iva                              *
      *                      *                                         *
      *                      * Da referenze                            *
      *                      *-----------------------------------------*
           move      w-ref-sco-chi-civ    to   w-vpb-civ-scc          .
      *                      *-----------------------------------------*
      *                      * Contropartita                           *
      *                      *                                         *
      *                      * Da referenze                            *
      *                      *-----------------------------------------*
           move      w-ref-sco-chi-ccp    to   w-vpb-ccp-scc          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Sconto pagamento        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice Iva                              *
      *                      *                                         *
      *                      * Da referenze                            *
      *                      *-----------------------------------------*
           move      w-ref-sco-pag-civ    to   w-vpb-civ-scp          .
      *                      *-----------------------------------------*
      *                      * Contropartita                           *
      *                      *                                         *
      *                      * Da referenze                            *
      *                      *-----------------------------------------*
           move      w-ref-sco-pag-ccp    to   w-vpb-ccp-scp          .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Spese in fattura        *
      *                  *---------------------------------------------*
           move      zero                 to   w-rou-ini-fat-ctr      .
       rou-ini-fat-510.
           add       1                    to   w-rou-ini-fat-ctr      .
           if        w-rou-ini-fat-ctr    >    6
                     go to rou-ini-fat-520.
      *                      *-----------------------------------------*
      *                      * Modalita' di addebito                   *
      *                      *-----------------------------------------*
           move      rf-bit-spe-mad
                    (w-rou-ini-fat-ctr)   to   w-vpb-spe-mad
                                              (w-rou-ini-fat-ctr)     .
      *                      *-----------------------------------------*
      *                      * Imponibile spesa                        *
      *                      *-----------------------------------------*
           move      rf-bit-spe-ibl
                    (w-rou-ini-fat-ctr)   to   w-vpb-spe-ibl
                                              (w-rou-ini-fat-ctr)     .
      *                      *-----------------------------------------*
      *                      * Totalizzatori da considerare            *
      *                      *-----------------------------------------*
           move      rf-bit-ibt-spe
                    (w-rou-ini-fat-ctr)   to   w-vpb-spe-ibt
                                              (w-rou-ini-fat-ctr)     .
      *                      *-----------------------------------------*
      *                      * Codice iva e contropartita per la spesa *
      *                      *                                         *
      *                      * Da personalizzazioni                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ricerca indice della spesa in ta-   *
      *                          * bella personalizzazioni spese per   *
      *                          * fatturazione                        *
      *                          *-------------------------------------*
           move      zero                 to   w-rou-ini-fat-inx      .
       rou-ini-fat-512.
           add       1                    to   w-rou-ini-fat-inx      .
           if        w-rou-ini-fat-inx    >    w-prs-spe-fat-nst
                     go to rou-ini-fat-514.
           if        w-rou-ini-fat-ctr    =    w-prs-spe-fat-npt
                                              (w-rou-ini-fat-inx)
                     go to rou-ini-fat-516.
           go to     rou-ini-fat-512.
       rou-ini-fat-514.
      *                          *-------------------------------------*
      *                          * Se spesa non trovata                *
      *                          *-------------------------------------*
           move      zero                 to   w-vpb-spe-civ
                                              (w-rou-ini-fat-ctr)     .
           move      zero                 to   w-vpb-spe-ccp
                                              (w-rou-ini-fat-ctr)     .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     rou-ini-fat-518.
       rou-ini-fat-516.
      *                          *-------------------------------------*
      *                          * Se spesa trovata                    *
      *                          *-------------------------------------*
           move      w-prs-spe-fat-civ
                    (w-rou-ini-fat-inx)   to   w-vpb-spe-civ
                                              (w-rou-ini-fat-ctr)     .
           move      w-prs-spe-fat-ccp
                    (w-rou-ini-fat-inx)   to   w-vpb-spe-ccp
                                              (w-rou-ini-fat-ctr)     .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     rou-ini-fat-518.
       rou-ini-fat-518.
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     rou-ini-fat-510.
       rou-ini-fat-520.
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Spese incasso           *
      *                  *---------------------------------------------*
           move      rf-bit-add-spi       to   w-vpb-add-spi          .
           perform   det-vpb-spi-000      thru det-vpb-spi-999        .
      *                  *---------------------------------------------*
      *                  * Valori relativi a : Spese bollo             *
      *                  *---------------------------------------------*
           move      rf-bit-add-spb       to   w-vpb-add-spb          .
           perform   det-vpb-spb-000      thru det-vpb-spb-999        .
       rou-ini-fat-530.
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento codice dipendenza   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se raggruppamento di tutte le bolle in- *
      *                      * dipendentemente dalla dipendenza : il   *
      *                      * codice dipendenza deve essere forzato   *
      *                      * a spaces                                *
      *                      *-----------------------------------------*
           if        srt-rag-bft          =    01
                     move  spaces         to   w-vpb-dpz-cli          .
       rou-ini-fat-540.
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento inoltro documento   *
      *                  *---------------------------------------------*
           if        w-vpb-inl-dcm        not  = 03
                     go to rou-ini-fat-600.
      *                      *-----------------------------------------*
      *                      * Se raggruppamento di tutte le bolle in- *
      *                      * dipendentemente dalla dipendenza : l'i- *
      *                      * noltro documento non puo' essere 'Alla  *
      *                      * Dipendenza' e viene forzato il valore   *
      *                      * 'Alla Sede'                             *
      *                      *-----------------------------------------*
           if        srt-rag-bft          =    01
                     move  01             to   w-vpb-inl-dcm
                     go to rou-ini-fat-600.
      *                      *-----------------------------------------*
      *                      * Negli altri casi se il codice dipenden- *
      *                      * za e' a spazi : l'inoltro documento non *
      *                      * puo' essere 'Alla Dipendenza' e viene   *
      *                      * forzato il valore 'Alla Sede'           *
      *                      *-----------------------------------------*
           if        w-vpb-dpz-cli        =    spaces
                     move  01             to   w-vpb-inl-dcm          .
       rou-ini-fat-600.
      *              *-------------------------------------------------*
      *              * Inizializzazione percentuale sconto in chiusu-  *
      *              * ra di riferimento per le bolle successive       *
      *              *-------------------------------------------------*
           move      rf-bit-per-scc       to   w-tot-per-scc          .
      *              *-------------------------------------------------*
      *              * Inizializzazione percentuale sconto pagamento   *
      *              * di riferimento per le bolle successive          *
      *              *-------------------------------------------------*
           move      rf-bit-per-scp       to   w-tot-per-scp          .
      *              *-------------------------------------------------*
      *              * Inizializzazione percentuali spese in fattura   *
      *              * di riferimento per le bolle successive          *
      *              *-------------------------------------------------*
           move      zero                 to   w-rou-ini-fat-ctr      .
       rou-ini-fat-610.
           add       1                    to   w-rou-ini-fat-ctr      .
           if        w-rou-ini-fat-ctr    >    6
                     go to rou-ini-fat-620.
           move      rf-bit-spe-snx
                    (w-rou-ini-fat-ctr)   to   w-tot-spe-snx
                                              (w-rou-ini-fat-ctr)     .
           move      rf-bit-spe-per
                    (w-rou-ini-fat-ctr)   to   w-tot-spe-per
                                              (w-rou-ini-fat-ctr)     .
           go to     rou-ini-fat-610.
       rou-ini-fat-620.
      *              *-------------------------------------------------*
      *              * Determinazione coefficiente di cambio valuta    *
      *              * per fatturazione                                *
      *              *-------------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-vpb-sgl-vpf        to   w-coe-cmb-vlt-sdv      .
           move      w-vpb-tdc-vpf        to   w-coe-cmb-vlt-tdc      .
           move      rr-dat-emi           to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [bit]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bit-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione coefficiente di cambio          *
      *              *-------------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-ini-fat-999.
       rou-ini-fat-999.
           exit.

      *    *===========================================================*
      *    * Routine per inizio fattura                                *
      *    *                                                           *
      *    * Subroutine per la modalita' di utilizzo dei codici ABI e  *
      *    * CAB come definito dall'apposita personalizzazione         *
      *    *-----------------------------------------------------------*
       rou-ini-fat-abi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore della perso-  *
      *              * nalizzazione                                    *
      *              *-------------------------------------------------*
           if        w-prs-dcc-abi        =    01
                     go to rou-ini-fat-abi-100
           else      go to rou-ini-fat-abi-200.
       rou-ini-fat-abi-100.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dall'anagrafica commerciale cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      srt-cod-abi          to   w-vpb-cod-abi          .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      srt-cod-cab          to   w-vpb-cod-cab          .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      srt-ccc-app          to   w-vpb-ccc-app          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-ini-fat-abi-900.
       rou-ini-fat-abi-200.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dal documento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      rf-bit-cod-abi       to   w-vpb-cod-abi          .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      rf-bit-cod-cab       to   w-vpb-cod-cab          .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      rf-bit-ccc-app       to   w-vpb-ccc-app          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     rou-ini-fat-abi-900.
       rou-ini-fat-abi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-ini-fat-abi-999.
       rou-ini-fat-abi-999.
           exit.

      *    *===========================================================*
      *    * Routine per aggiornamento totali righe fattura            *
      *    *-----------------------------------------------------------*
       agg-tri-fat-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione protocollo bolla in esame       *
      *              *                                                 *
      *              * N.B.: non viene eseguito nessun controllo sul   *
      *              *       numero massimo elementi in tabella, in    *
      *              *       quanto tale controllo viene gia' effet-   *
      *              *       tuato in sede di test di rottura fattura  *
      *              *                                                 *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-pbf-num-ele      .
           move      srt-prt-bit          to   w-buf-pbf-num-prt
                                              (w-buf-pbf-num-ele)     .
       agg-tri-fat-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori che variano di bolla in  *
      *              * bolla                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Contropartita vendite                       *
      *                  *---------------------------------------------*
           if        rf-bit-ctp-ven       not  = w-vpb-ctp-ven
                     move  zero           to   w-vpb-ctp-ven          .
      *                  *---------------------------------------------*
      *                  * Codice listino                              *
      *                  *---------------------------------------------*
           if        rf-bit-cod-lst       not  = w-vpb-cod-lst
                     move  spaces         to   w-vpb-cod-lst          .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in riga e relative % asso- *
      *                  * ciate                                       *
      *                  *---------------------------------------------*
           if        rf-bit-csr-aac       not  = w-vpb-csr-aac
                     move  zero           to   w-vpb-csr-aac
                     move  zero           to   w-vpb-psr-aac (1)
                     move  zero           to   w-vpb-psr-aac (2)
                     move  zero           to   w-vpb-psr-aac (3)
                     move  zero           to   w-vpb-psr-aac (4)
                     move  zero           to   w-vpb-psr-aac (5)      .
      *                  *---------------------------------------------*
      *                  * Categoria sconto in chiusura e percentuale  *
      *                  * associata                                   *
      *                  *---------------------------------------------*
           if        rf-bit-csc-aac       not  = w-vpb-csc-aac
                     move  zero           to   w-vpb-csc-aac
                     move  zero           to   w-vpb-psc-aac          .
      *                  *---------------------------------------------*
      *                  * Voci descrittive in fattura                 *
      *                  *---------------------------------------------*
           if        rf-bit-voc-des (1)   not  = w-vpb-voc-des (1)
                     move  spaces         to   w-vpb-voc-des (1)      .
           if        rf-bit-voc-des (2)   not  = w-vpb-voc-des (2)
                     move  spaces         to   w-vpb-voc-des (2)      .
           if        rf-bit-voc-des (3)   not  = w-vpb-voc-des (3)
                     move  spaces         to   w-vpb-voc-des (3)      .
           if        rf-bit-voc-des (4)   not  = w-vpb-voc-des (4)
                     move  spaces         to   w-vpb-voc-des (4)      .
           if        rf-bit-voc-des (5)   not  = w-vpb-voc-des (5)
                     move  spaces         to   w-vpb-voc-des (5)      .
           if        rf-bit-voc-des (6)   not  = w-vpb-voc-des (6)
                     move  spaces         to   w-vpb-voc-des (6)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-bit-per-scc       not  = w-tot-per-scc
                     move  zero           to   w-tot-per-scc          .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto pagamento associata alla *
      *                  * forma di pagamento                          *
      *                  *---------------------------------------------*
           if        rf-bit-scp-aap       not  = w-vpb-scp-aap
                     move  zero           to   w-vpb-scp-aap          .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto pagamento                *
      *                  *---------------------------------------------*
           if        rf-bit-per-scp       not  = w-tot-per-scp
                     move  zero           to   w-tot-per-scp          .
      *                  *---------------------------------------------*
      *                  * Si/No addebito spese in fattura             *
      *                  *---------------------------------------------*
           if        w-prs-tac-spf        =    01
                     go to agg-tri-fat-110.
           if        rf-bit-spe-imp (1)   not  = zero
                     move  1              to   w-tot-spe-snx (1)      .
           if        rf-bit-spe-imp (2)   not  = zero
                     move  1              to   w-tot-spe-snx (2)      .
           if        rf-bit-spe-imp (3)   not  = zero
                     move  1              to   w-tot-spe-snx (3)      .
           if        rf-bit-spe-imp (4)   not  = zero
                     move  1              to   w-tot-spe-snx (4)      .
           if        rf-bit-spe-imp (5)   not  = zero
                     move  1              to   w-tot-spe-snx (5)      .
           if        rf-bit-spe-imp (6)   not  = zero
                     move  1              to   w-tot-spe-snx (6)      .
       agg-tri-fat-110.
      *                  *---------------------------------------------*
      *                  * Percentuali per spese in fattura            *
      *                  *---------------------------------------------*
           if        rf-bit-spe-snx (1)   =    0 or
                     rf-bit-spe-per (1)   not  = w-tot-spe-per (1)
                     move  zero           to   w-tot-spe-per (1)      .
           if        rf-bit-spe-snx (2)   =    0 or
                     rf-bit-spe-per (2)   not  = w-tot-spe-per (2)
                     move  zero           to   w-tot-spe-per (2)      .
           if        rf-bit-spe-snx (3)   =    0 or
                     rf-bit-spe-per (3)   not  = w-tot-spe-per (3)
                     move  zero           to   w-tot-spe-per (3)      .
           if        rf-bit-spe-snx (4)   =    0 or
                     rf-bit-spe-per (4)   not  = w-tot-spe-per (4)
                     move  zero           to   w-tot-spe-per (4)      .
           if        rf-bit-spe-snx (5)   =    0 or
                     rf-bit-spe-per (5)   not  = w-tot-spe-per (5)
                     move  zero           to   w-tot-spe-per (5)      .
           if        rf-bit-spe-snx (6)   =    0 or
                     rf-bit-spe-per (6)   not  = w-tot-spe-per (6)
                     move  zero           to   w-tot-spe-per (6)      .
       agg-tri-fat-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento totali fattura                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Provvigione a forfait per l'agente          *
      *                  *---------------------------------------------*
           if        rf-bit-fsp-doc       =    03
                     add  rf-bit-pvf-age  to   w-tot-pvf-age
           else if   rf-bit-fsp-doc       =    04
                     move rf-bit-pvf-age  to   w-tot-pvf-age
           else      move zero            to   w-tot-pvf-age          .
      *                  *---------------------------------------------*
      *                  * Acconti                                     *
      *                  *---------------------------------------------*
           add       rf-bit-pag-act       to   w-tot-pag-act          .
      *                  *---------------------------------------------*
      *                  * Importo sconto in chiusura                  *
      *                  *---------------------------------------------*
           add       rf-bit-tot-scc       to   w-tot-tot-scc          .
      *                  *---------------------------------------------*
      *                  * Importo sconto pagamento                    *
      *                  *---------------------------------------------*
           add       rf-bit-tot-scp       to   w-tot-tot-scp          .
      *                  *---------------------------------------------*
      *                  * Spese in fattura                            *
      *                  *---------------------------------------------*
           add       rf-bit-spe-imp (1)   to   w-tot-spe-imp (1)      .
           add       rf-bit-spe-imp (2)   to   w-tot-spe-imp (2)      .
           add       rf-bit-spe-imp (3)   to   w-tot-spe-imp (3)      .
           add       rf-bit-spe-imp (4)   to   w-tot-spe-imp (4)      .
           add       rf-bit-spe-imp (5)   to   w-tot-spe-imp (5)      .
           add       rf-bit-spe-imp (6)   to   w-tot-spe-imp (6)      .
      *                  *---------------------------------------------*
      *                  * Spese incasso aggiuntive                    *
      *                  *---------------------------------------------*
           add       rf-bit-tot-sia       to   w-tot-tot-sia          .
       agg-tri-fat-300.
      *              *-------------------------------------------------*
      *              * Start su file [bir]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-prt-bit          to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to agg-tri-fat-999.
       agg-tri-fat-320.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [bir]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to agg-tri-fat-999.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bir-num-prt       not  = srt-prt-bit
                     go to agg-tri-fat-999.
       agg-tri-fat-330.
      *              *-------------------------------------------------*
      *              * Composizione record [fir], ad esclusione del    *
      *              * progressivo riga                                *
      *              *-------------------------------------------------*
           perform   cmp-rec-fir-000      thru cmp-rec-fir-999        .
       agg-tri-fat-340.
      *              *-------------------------------------------------*
      *              * Eventuale trattamento record [fix]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [fix]                *
      *                  *---------------------------------------------*
           move      spaces               to   rf-fix                 .
      *                  *---------------------------------------------*
      *                  * Se non necessario : oltre                   *
      *                  *---------------------------------------------*
           if        rf-bir-des-ext       not  = 01
                     go to agg-tri-fat-350.
      *                  *---------------------------------------------*
      *                  * Lettura record [bix]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT"             to   f-key                  .
           move      rf-bir-num-prt       to   rf-bix-num-prt         .
           move      rf-bir-num-prg       to   rf-bix-num-prg         .
           move      11                   to   rf-bix-tip-rec         .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *                  *---------------------------------------------*
      *                  * Se esito operazione negativo : oltre        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to agg-tri-fat-350.
      *                  *---------------------------------------------*
      *                  * Composizione record [fix], ad esclusione    *
      *                  * del progressivo riga                        *
      *                  *---------------------------------------------*
           perform   cmp-rec-fix-000      thru cmp-rec-fix-999        .
       agg-tri-fat-350.
      *              *-------------------------------------------------*
      *              * Scrittura file relative di appoggio [rlt]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record                         *
      *                  *---------------------------------------------*
           move      rf-fir               to   rlt-rec-fir            .
           move      rf-fix               to   rlt-rec-fix            .
      *                  *---------------------------------------------*
      *                  * Richiamo subroutine per la scrittura        *
      *                  *---------------------------------------------*
           perform   fil-rlt-wrt-000      thru fil-rlt-wrt-999        .
       agg-tri-fat-400.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-fir-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to agg-tri-fat-500.
      *                  *---------------------------------------------*
      *                  * Se tipo prodotto non definito : a totaliz-  *
      *                  * zazione eventuali addebiti                  *
      *                  *---------------------------------------------*
           if        rf-fir-tip-pro       =    zero
                     go to agg-tri-fat-450.
      *                  *---------------------------------------------*
      *                  * Aggiornamento                               *
      *                  *---------------------------------------------*
           add       rf-fir-imp-rig       to   w-tot-tot-rig
                                              (rf-fir-tip-pro)        .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     agg-tri-fat-500.        
       agg-tri-fat-450.
      *                  *---------------------------------------------*
      *                  * Test su indicatore tipo di totale da ag-    *
      *                  * giornare                                    *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-tpr    not  = "A"
                     go to agg-tri-fat-500.
           if        w-dec-tip-rig-cac    =    zero
                     go to agg-tri-fat-500.
           if        w-dec-tip-rig-tot    =    zero
                     go to agg-tri-fat-500.
      *                  *---------------------------------------------*
      *                  * Aggiornamento                               *
      *                  *---------------------------------------------*
           add       rf-fir-imp-rig       to   w-tot-tot-rig
                                              (w-dec-tip-rig-tot)     .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     agg-tri-fat-500.        
       agg-tri-fat-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-fir-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-fir-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       agg-tri-fat-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no aggiornamento       *
      *                  *---------------------------------------------*
           move      rf-fir-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to agg-tri-fat-700.
      *                  *---------------------------------------------*
      *                  * Aggiornamento                               *
      *                  *---------------------------------------------*
           move      "+"                  to   w-agg-cst-ctp-tip      .
           move      rf-fir-ctp-ven       to   w-agg-cst-ctp-cod      .
           move      rf-fir-imp-rig       to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       agg-tri-fat-700.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale [bir]            *
      *              *-------------------------------------------------*
           go to     agg-tri-fat-320.
       agg-tri-fat-999.
           exit.

      *    *===========================================================*
      *    * Composizione record righe [fir]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-fir-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fir                 .
       cmp-rec-fir-100.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
       cmp-rec-fir-200.
      *                  *---------------------------------------------*
      *                  * Valori diretti da record [bir] o da testata *
      *                  * prima bolla letta                           *
      *                  *---------------------------------------------*
           move      zero                 to   rf-fir-num-prt         .
           move      zero                 to   rf-fir-num-prg         .
           move      w-vpb-tmo-ftr        to   rf-fir-cod-tmo         .
           move      rr-dpz-emi           to   rf-fir-cod-dpz         .
           move      rr-dat-emi           to   rf-fir-dat-doc         .
           move      zero                 to   rf-fir-num-doc         .
           move      w-vpb-cod-cli        to   rf-fir-cod-cli         .
           move      w-vpb-dpz-cli        to   rf-fir-dpz-cli         .
           move      rf-bir-cod-arc       to   rf-fir-cli-pls         .
           move      rf-bir-dpz-arc       to   rf-fir-dpc-pls         .
           move      w-vpb-cod-lng        to   rf-fir-cod-lng         .
           move      w-vpb-sgl-vpf        to   rf-fir-sgl-vpf         .
           move      w-vpb-dec-vpf        to   rf-fir-dec-vpf         .
           move      w-vpb-tdc-vpf        to   rf-fir-tdc-vpf         .
           move      w-tot-cdc-vpf        to   rf-fir-cdc-vpf         .
           move      rf-bir-bld-flb       to   rf-fir-bld-flb         .
           move      rf-bir-bld-tpb       to   rf-fir-bld-tpb         .
           move      rf-bir-bld-rgb       to   rf-fir-bld-rgb         .
           move      rf-bir-tip-rig       to   rf-fir-tip-rig         .
           move      rf-bir-tip-mag       to   rf-fir-tip-mag         .
           move      rf-bir-num-pro       to   rf-fir-num-pro         .
           move      rf-bir-alf-pro       to   rf-fir-alf-pro         .
           move      rf-bir-sgl-vrn       to   rf-fir-sgl-vrn         .
           move      rf-bir-cop-scl       to   rf-fir-cop-scl         .
           move      rf-bir-des-ext       to   rf-fir-des-ext         .
           move      rf-bir-des-rig       to   rf-fir-des-rig         .
           move      rf-bir-tip-pro       to   rf-fir-tip-pro         .
           move      rf-bir-cod-iva       to   rf-fir-cod-iva         .
           move      rf-bir-ctp-ven       to   rf-fir-ctp-ven         .
           move      rf-bir-umi-ven       to   rf-fir-umi-ven         .
           move      rf-bir-dec-qta       to   rf-fir-dec-qta         .
           move      rf-bir-qta-ven       to   rf-fir-qta-ven         .
           move      rf-bir-snx-2qt       to   rf-fir-snx-2qt         .
           move      rf-bir-dec-2qt       to   rf-fir-dec-2qt         .
           move      rf-bir-qta-a02       to   rf-fir-qta-a02         .
           move      rf-bir-snx-3qt       to   rf-fir-snx-3qt         .
           move      rf-bir-dec-3qt       to   rf-fir-dec-3qt         .
           move      rf-bir-qta-a03       to   rf-fir-qta-a03         .
           move      rf-bir-dec-prz       to   rf-fir-dec-prz         .
           move      rf-bir-sgl-vps       to   rf-fir-sgl-vps         .
           move      rf-bir-dec-vps       to   rf-fir-dec-vps         .
           move      rf-bir-tdc-vps       to   rf-fir-tdc-vps         .
           move      rf-bir-prz-lrs       to   rf-fir-prz-lrs         .
           move      rf-bir-prz-nts       to   rf-fir-prz-nts         .
           move      w-vpb-sgl-vpf        to   rf-fir-sgl-vpp         .
           move      w-vpb-dec-vpf        to   rf-fir-dec-vpp         .
           move      w-vpb-tdc-vpf        to   rf-fir-tdc-vpp         .
           move      w-tot-cdc-vpf        to   rf-fir-cdc-vpp         .
           move      rf-bir-snx-2pz       to   rf-fir-snx-2pz         .
           move      rf-bir-prz-a02       to   rf-fir-prz-a02         .
           move      rf-bir-sgl-vpl       to   rf-fir-sgl-vpl         .
           move      rf-bir-dec-vpl       to   rf-fir-dec-vpl         .
           move      rf-bir-tdc-vpl       to   rf-fir-tdc-vpl         .
           move      rf-bir-prz-vpl       to   rf-fir-prz-vpl         .
           move      rf-bir-cdc-vpl       to   rf-fir-cdc-vpl         .
           move      rf-bir-ccr-vpl       to   rf-fir-ccr-vpl         .
           move      rf-bir-plm-vpl       to   rf-fir-plm-vpl         .
           move      rf-bir-tlm-vpl       to   rf-fir-tlm-vpl         .
           move      rf-bir-map-vpl       to   rf-fir-map-vpl         .
           move      rf-bir-epz-rgf       to   rf-fir-epz-rgf         .
           move      rf-bir-csr-aap       to   rf-fir-csr-aap         .
           move      rf-bir-psr-aap (1)   to   rf-fir-psr-aap (1)     .
           move      rf-bir-psr-aap (2)   to   rf-fir-psr-aap (2)     .
           move      rf-bir-psr-aap (3)   to   rf-fir-psr-aap (3)     .
           move      rf-bir-psr-aap (4)   to   rf-fir-psr-aap (4)     .
           move      rf-bir-psr-aap (5)   to   rf-fir-psr-aap (5)     .
           move      rf-bir-per-scr (1)   to   rf-fir-per-scr (1)     .
           move      rf-bir-per-scr (2)   to   rf-fir-per-scr (2)     .
           move      rf-bir-per-scr (3)   to   rf-fir-per-scr (3)     .
           move      rf-bir-per-scr (4)   to   rf-fir-per-scr (4)     .
           move      rf-bir-per-scr (5)   to   rf-fir-per-scr (5)     .
           move      rf-bir-epz-pes       to   rf-fir-epz-pes         .
           move      rf-bir-sgl-vpc       to   rf-fir-sgl-vpc         .
           move      rf-bir-dec-vpc       to   rf-fir-dec-vpc         .
           move      rf-bir-tdc-vpc       to   rf-fir-tdc-vpc         .
           move      rf-bir-cdc-vpc       to   rf-fir-cdc-vpc         .
           move      rf-bir-dec-cos       to   rf-fir-dec-cos         .
           move      rf-bir-cos-rif       to   rf-fir-cos-rif         .
           move      rf-bir-imp-rig       to   rf-fir-imp-rig         .
           move      rf-bir-iau-rig       to   rf-fir-iau-rig         .
           move      rf-bir-cpv-aap       to   rf-fir-cpv-aap         .
           move      rf-bir-ppv-aap (1)   to   rf-fir-ppv-aap (1)     .
           move      rf-bir-ppv-aap (2)   to   rf-fir-ppv-aap (2)     .
           move      rf-bir-ppv-aap (3)   to   rf-fir-ppv-aap (3)     .
           move      rf-bir-fsp-rig       to   rf-fir-fsp-rig         .
           move      rf-bir-cpv-rig       to   rf-fir-cpv-rig         .
           move      rf-bir-ppv-rig (1)   to   rf-fir-ppv-rig (1)     .
           move      rf-bir-ppv-rig (2)   to   rf-fir-ppv-rig (2)     .
           move      rf-bir-ppv-rig (3)   to   rf-fir-ppv-rig (3)     .
           move      rf-bir-pvf-rig       to   rf-fir-pvf-rig         .
           move      rf-bir-ocl-dat       to   rf-fir-ocl-dat         .
           move      rf-bir-ocl-num       to   rf-fir-ocl-num         .
           move      rf-bir-cmc-tip       to   rf-fir-cmc-tip         .
           move      rf-bir-cmc-dat       to   rf-fir-cmc-dat         .
           move      rf-bir-cmc-num       to   rf-fir-cmc-num         .
           move      rf-bir-coc-tip       to   rf-fir-coc-tip         .
           move      rf-bir-coc-dat       to   rf-fir-coc-dat         .
           move      rf-bir-coc-num       to   rf-fir-coc-num         .
           move      rf-bir-cod-tmb       to   rf-fir-bcc-tip         .
           move      rf-bir-dat-doc       to   rf-fir-bcc-dat         .
           move      rf-bir-num-doc       to   rf-fir-bcc-num         .
           move      rf-bir-flg-puq       to   rf-fir-flg-puq         .
           move      spaces               to   rf-fir-flg-ela         .
           move      spaces               to   rf-fir-flg-pul         .
           move      spaces               to   rf-fir-alx-exp         .
       cmp-rec-fir-300.
      *                  *---------------------------------------------*
      *                  * Valori indiretti                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo riga in comodo ridefinito          *
      *                      *-----------------------------------------*
           move      rf-fir-tip-rig       to   w-cmp-rec-fir-wtr      .
       cmp-rec-fir-310.
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio valuta per i     *
      *                      * prezzi standard alla data del documento *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da determinare              *
      *                          *-------------------------------------*
           if        w-cmp-rec-fir-wtp    not  = "P" and
                     w-cmp-rec-fir-wtp    not  = "M" and
                     w-cmp-rec-fir-wtp    not  = "S"
                     go to cmp-rec-fir-320.
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
           move      w-tot-cdc-vpf        to   rf-fir-cdc-vps         .
       cmp-rec-fir-320.
      *                      *-----------------------------------------*
      *                      * Prezzo di vendita e prezzo netto        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da determinare              *
      *                          *-------------------------------------*
           if        w-cmp-rec-fir-wtp    not  = "P" and
                     w-cmp-rec-fir-wtp    not  = "M" and
                     w-cmp-rec-fir-wtp    not  = "S"
                     go to cmp-rec-fir-340.
      *                          *-------------------------------------*
      *                          * Se valuta per il prezzo contenuta   *
      *                          * nella riga bolla e' pari alla valu- *
      *                          * ta per fatturazione del documento,  *
      *                          * vuol dire che l'eventuale cambio e' *
      *                          * gia' stato eseguito al momento del- *
      *                          * la bolla e quindi il prezzo rimane  *
      *                          * quello contenuto nel record [bir]   *
      *                          *-------------------------------------*
           if        rf-bir-sgl-vpp       =    w-vpb-sgl-vpf and
                     rf-bir-dec-vpp       =    w-vpb-dec-vpf and
                     rf-bir-tdc-vpp       =    w-vpb-tdc-vpf
                     move  rf-bir-prz-ven to   rf-fir-prz-ven
                     move  rf-bir-prz-net to   rf-fir-prz-net
                     go to cmp-rec-fir-340.
      *                          *-------------------------------------*
      *                          * Trasformazione prezzo di vendita    *
      *                          * espresso nella valuta per il prezzo *
      *                          * nel valore espresso nella valuta di *
      *                          * fatturazione                        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio per la valuta per il     *
      *                              * prezzo alla data di emissione   *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bir-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-bir-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      rr-dat-emi           to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [bir]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-bir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-bir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-bir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-bir-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Conversione da valuta base a    *
      *                              * valuta per fatturazione         *
      *                              *---------------------------------*
           move      w-vpb-sgl-vpf        to   w-cvs-vlt-sgl          .
           move      w-vpb-dec-vpf        to   w-cvs-vlt-dec          .
           move      w-vpb-tdc-vpf        to   w-cvs-vlt-tdc          .
           move      w-tot-cdc-vpf        to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-aav        to   rf-fir-prz-ven         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-fir-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-fir-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-fir-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-fir-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-fir-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-fir-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-fir-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione nuovo importo in *
      *                              * riga                            *
      *                              *---------------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
       cmp-rec-fir-340.
      *                      *-----------------------------------------*
      *                      * Trattamento legame valutario            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-bir-sgl-vpl       =    spaces
                     go to cmp-rec-fir-360.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se cambio da   *
      *                          * applicare o meno                    *
      *                          *-------------------------------------*
           if        rf-bir-map-vpl       =    "F"
                     go to cmp-rec-fir-342
           else      go to cmp-rec-fir-346.
       cmp-rec-fir-342.
      *                          *-------------------------------------*
      *                          * Se cambio da applicare              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio effettivo per il legame  *
      *                              * valutario alla data del docu-   *
      *                              * mento                           *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bir-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-bir-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rr-dat-emi           to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [bir]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-fir-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-fir-prz-ven       to   w-sav-prz-ven          .
      *                              *---------------------------------*
      *                              * Determinazione legame valutario *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Applicazione cambio per le- *
      *                                  * game valutario              *
      *                                  *-----------------------------*
           move      rf-fir-prz-vpl       to   rf-fir-prz-ven         .
      *                                  *-----------------------------*
      *                                  * Parametri in input          *
      *                                  *-----------------------------*
           move      rf-fir-prz-ven       to   w-lvl-prz-prz          .
           move      rf-fir-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-fir-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-fir-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-fir-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-fir-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-fir-tlm-vpl       to   w-lvl-prz-tlm          .
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
      *                                  *-----------------------------*
      *                                  * Valore determinato          *
      *                                  *-----------------------------*
           move      w-lvl-prz-prz        to   rf-fir-prz-ven         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-fir-prz-ven       =    w-sav-prz-ven
                     go to cmp-rec-fir-344.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-fir-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-fir-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-fir-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-fir-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-fir-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-fir-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-fir-prz-net         .
      *                              *---------------------------------*
      *                              * Provvigioni in riga             *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se prodotto non di vendita, *
      *                                  * oltre                       *
      *                                  *-----------------------------*
           if        w-cmp-rec-fir-wtp    not  = "P"
                     go to cmp-rec-fir-344.
      *                                  *-----------------------------*
      *                                  * Se prodotto similare : ol-  *
      *                                  * tre                         *
      *                                  *-----------------------------*
           if        w-cmp-rec-fir-wtf    not  = spaces
                     go to cmp-rec-fir-344.
      *                                  *-----------------------------*
      *                                  * Test se da trattare         *
      *                                  *-----------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to cmp-rec-fir-344.
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   det-ppv-rig-000      thru det-ppv-rig-999        .
       cmp-rec-fir-344.
      *                              *---------------------------------*
      *                              * Determinazione nuovo importo in *
      *                              * riga                            *
      *                              *---------------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     cmp-rec-fir-360.
       cmp-rec-fir-346.
      *                              *---------------------------------*
      *                              * Se cambio da non applicare      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     cmp-rec-fir-360.
       cmp-rec-fir-360.
      *                      *-----------------------------------------*
      *                      * Codice iva                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da eseguire                 *
      *                          *-------------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to cmp-rec-fir-400.
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-cmp-rec-fir-wtp    =    "C" 
                     go to cmp-rec-fir-400.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del confronto  *
      *                          * tra assoggettamento iva del docu-   *
      *                          * mento e codice iva in riga          *
      *                          *-------------------------------------*
           move      rf-bir-cod-iva       to   w-edt-iva-cod          .
      *
           if       (w-vpb-ass-iva        =    zero    ) and
                    (w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7   )
                     go to cmp-rec-fir-370
           else if   w-vpb-ass-iva        >    zero
                     go to cmp-rec-fir-380
           else      go to cmp-rec-fir-400.
       cmp-rec-fir-370.
      *                          *-------------------------------------*
      *                          * Se documento soggetto e codice iva  *
      *                          * in riga esente                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-cmp-rec-fir-wtp    =    "P"
                     go to cmp-rec-fir-372
           else if   w-cmp-rec-fir-wtp    =    "A"
                     go to cmp-rec-fir-374
           else      go to cmp-rec-fir-400.
       cmp-rec-fir-372.
      *                              *---------------------------------*
      *                              * Tipo riga : Prodotto di vendita *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      rf-bir-num-pro       to   w-let-arc-dcp-cod      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                                  *-----------------------------*
      *                                  * Se lettura errata o codice  *
      *                                  * iva letto a zero : oltre    *
      *                                  *-----------------------------*
           if        w-let-arc-dcp-flg    not  = spaces or
                     w-let-arc-dcp-civ    =    zero
                     go to cmp-rec-fir-400.
      *                                  *-----------------------------*
      *                                  * Bufferizzazione codice iva  *
      *                                  *-----------------------------*
           move      w-let-arc-dcp-civ    to   rf-fir-cod-iva         .
           go to     cmp-rec-fir-400.
       cmp-rec-fir-374.
      *                              *---------------------------------*
      *                              * Tipo riga : Addebito            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Decomposizione tipo riga    *
      *                                  *-----------------------------*
           move      rf-bir-tip-rig       to   w-dec-tip-rig-str      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
      *                                  *-----------------------------*
      *                                  * Lettura archivio [zac]      *
      *                                  *-----------------------------*
           move      01                   to   w-let-arc-zac-tip      .
           move      w-dec-tip-rig-cac    to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
      *                                  *-----------------------------*
      *                                  * Se lettura errata o codice  *
      *                                  * iva letto a zero : oltre    *
      *                                  *-----------------------------*
           if        w-let-arc-zac-flg    not  = spaces or
                     w-let-arc-zac-civ    =    zero
                     go to cmp-rec-fir-400.
      *                                  *-----------------------------*
      *                                  * Bufferizzazione codice iva  *
      *                                  *-----------------------------*
           move      w-let-arc-zac-civ    to   rf-fir-cod-iva         .
           go to     cmp-rec-fir-400.
       cmp-rec-fir-380.
      *                          *-------------------------------------*
      *                          * Se documento esente e codice iva    *
      *                          * in riga soggetto                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Forzatura esenzione in codice   *
      *                              * iva in riga                     *
      *                              *---------------------------------*
           move      w-vpb-ass-iva        to   rf-fir-cod-iva         .
           go to     cmp-rec-fir-400.
       cmp-rec-fir-400.
      *                  *---------------------------------------------*
      *                  * Fine valori indiretti                       *
      *                  *---------------------------------------------*
       cmp-rec-fir-999.
           exit.

      *    *===========================================================*
      *    * Composizione record righe [fix]                           *
      *    *-----------------------------------------------------------*
       cmp-rec-fix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fix                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      zero                 to   rf-fix-num-prt         .
           move      zero                 to   rf-fix-num-prg         .
           move      01                   to   rf-fix-tip-rec         .
           move      rf-bix-des-400       to   rf-fix-des-400         .
       cmp-rec-fix-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento area per rottura                            *
      *    *-----------------------------------------------------------*
       agg-rot-are-000.
      *              *-------------------------------------------------*
      *              * Valori indipendenti dal tipo raggruppamento     *
      *              * bolle in fattura                                *
      *              *-------------------------------------------------*
           move      srt-cli-plf          to   w-rot-cli-plf          .
           move      srt-cod-lng          to   w-rot-cod-lng          .
           move      srt-tip-frn          to   w-rot-tip-frn          .
           move      srt-cod-cli-tpf      to   w-rot-cod-cli          .
           move      srt-dpz-cli-tpf      to   w-rot-dpz-cli          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo raggruppamento  *
      *              * bolle in fattura                                *
      *              *-------------------------------------------------*
           if        srt-rag-bft          =    01
                     go to agg-rot-are-100
           else if   srt-rag-bft          =    02
                     go to agg-rot-are-200
           else if   srt-rag-bft          =    03
                     go to agg-rot-are-300
           else if   srt-rag-bft          =    11
                     go to agg-rot-are-500.
           go to     agg-rot-are-300.
       agg-rot-are-100.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una unica fattura per tut-*
      *              *                       te le bolle e per qual-   *
      *              *                       siasi destinatario del    *
      *              *                       cliente                   *
      *              *-------------------------------------------------*
           move      srt-tmo-ftr-1o2      to   w-rot-tmo-ftr          .
           move      srt-sgl-vpf-1o2      to   w-rot-sgl-vpf          .
           move      srt-dec-vpf-1o2      to   w-rot-dec-vpf          .
           move      srt-tdc-vpf-1o2      to   w-rot-tdc-vpf          .
           move      srt-ass-iva-1o2      to   w-rot-ass-iva          .
           move      srt-cod-age-1o2      to   w-rot-cod-age          .
           move      srt-fsp-doc-1o2      to   w-rot-fsp-doc          .
      *
           if        srt-fsp-doc-1o2      =    04
                     move  srt-pvf-age-1o2
                                          to   w-rot-pvf-age
           else      move  zero           to   w-rot-pvf-age          .
      *
           move      srt-tip-vpa-1o2      to   w-rot-tip-vpa          .
           move      srt-cod-ime-1o2      to   w-rot-cod-ime          .
           move      srt-snx-pfi-1o2      to   w-rot-snx-pfi          .
           move      srt-inl-pgt-1o2      to   w-rot-inl-pgt          .
           move      srt-cod-fop-1o2      to   w-rot-cod-fop          .
           move      srt-snx-qaf-1o2      to   w-rot-snx-qaf          .
           move      srt-snx-dsm-1o2      to   w-rot-snx-dsm          .
           move      srt-cod-abi-1o2      to   w-rot-cod-abi          .
           move      srt-cod-cab-1o2      to   w-rot-cod-cab          .
           move      srt-ccc-app-1o2      to   w-rot-ccc-app          .
           move      srt-pag-dsm-1o2      to   w-rot-pag-dsm          .
           move      srt-add-spi-1o2      to   w-rot-add-spi          .
           move      srt-add-spb-1o2      to   w-rot-add-spb          .
           move      srt-spe-snx-1o2 (1)  to   w-rot-spe-snx (1)      .
           move      srt-spe-snx-1o2 (2)  to   w-rot-spe-snx (2)      .
           move      srt-spe-snx-1o2 (3)  to   w-rot-spe-snx (3)      .
           move      srt-spe-snx-1o2 (4)  to   w-rot-spe-snx (4)      .
           move      srt-spe-snx-1o2 (5)  to   w-rot-spe-snx (5)      .
           move      srt-spe-snx-1o2 (6)  to   w-rot-spe-snx (6)      .
           move      srt-dpc-plf-001      to   w-rot-dpc-plf          .
           go to     agg-rot-are-900.
       agg-rot-are-200.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni de-  *
      *              *                       stinatario del cliente    *
      *              *-------------------------------------------------*
           move      srt-tmo-ftr-1o2      to   w-rot-tmo-ftr          .
           move      srt-sgl-vpf-1o2      to   w-rot-sgl-vpf          .
           move      srt-dec-vpf-1o2      to   w-rot-dec-vpf          .
           move      srt-tdc-vpf-1o2      to   w-rot-tdc-vpf          .
           move      srt-ass-iva-1o2      to   w-rot-ass-iva          .
           move      srt-cod-age-1o2      to   w-rot-cod-age          .
           move      srt-fsp-doc-1o2      to   w-rot-fsp-doc          .
      *
           if        srt-fsp-doc-1o2      =    04
                     move  srt-pvf-age-1o2
                                          to   w-rot-pvf-age
           else      move  zero           to   w-rot-pvf-age          .
      *
           move      srt-tip-vpa-1o2      to   w-rot-tip-vpa          .
           move      srt-cod-ime-1o2      to   w-rot-cod-ime          .
           move      srt-snx-pfi-1o2      to   w-rot-snx-pfi          .
           move      srt-dpc-plf-002      to   w-rot-dpc-plf          .
           move      srt-inl-pgt-1o2      to   w-rot-inl-pgt          .
           move      srt-cod-fop-1o2      to   w-rot-cod-fop          .
           move      srt-snx-qaf-1o2      to   w-rot-snx-qaf          .
           move      srt-snx-dsm-1o2      to   w-rot-snx-dsm          .
           move      srt-cod-abi-1o2      to   w-rot-cod-abi          .
           move      srt-cod-cab-1o2      to   w-rot-cod-cab          .
           move      srt-ccc-app-1o2      to   w-rot-ccc-app          .
           move      srt-pag-dsm-1o2      to   w-rot-pag-dsm          .
           move      srt-add-spi-1o2      to   w-rot-add-spi          .
           move      srt-add-spb-1o2      to   w-rot-add-spb          .
           move      srt-spe-snx-1o2 (1)  to   w-rot-spe-snx (1)      .
           move      srt-spe-snx-1o2 (2)  to   w-rot-spe-snx (2)      .
           move      srt-spe-snx-1o2 (3)  to   w-rot-spe-snx (3)      .
           move      srt-spe-snx-1o2 (4)  to   w-rot-spe-snx (4)      .
           move      srt-spe-snx-1o2 (5)  to   w-rot-spe-snx (5)      .
           move      srt-spe-snx-1o2 (6)  to   w-rot-spe-snx (6)      .
           go to     agg-rot-are-900.
       agg-rot-are-300.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni bol- *
      *              *                       la                        *
      *              *-------------------------------------------------*
           move      srt-dpc-plf-003      to   w-rot-dpc-plf          .
           go to     agg-rot-are-900.
       agg-rot-are-500.
      *              *-------------------------------------------------*
      *              * Tipo raggruppamento : Una fattura per ogni or-  *
      *              *                       dine                      *
      *              *-------------------------------------------------*
           move      srt-coc-prt-011      to   w-rot-coc-prt          .
           go to     agg-rot-are-900.
       agg-rot-are-900.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore righe della fattura  *
      *              *-------------------------------------------------*
           move      srt-ctr-bir          to   w-rot-ctr-rig          .
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore testate bolle della  *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-rot-ctr-tes          .
       agg-rot-are-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione totali fattura                            *
      *    *-----------------------------------------------------------*
       nor-tot-fat-000.
           move      zero                 to   w-tot-cdc-vpf          .
           move      zero                 to   w-tot-pvf-age          .
           move      zero                 to   w-tot-pag-act          .
           move      zero                 to   w-nor-tot-fat-ctr      .
       nor-tot-fat-020.
           add       1                    to   w-nor-tot-fat-ctr      .
           if        w-nor-tot-fat-ctr    >    9
                     go to nor-tot-fat-040.
           move      zero                 to   w-tot-tot-rig
                                              (w-nor-tot-fat-ctr)     .
           go to     nor-tot-fat-020.
       nor-tot-fat-040.
           move      zero                 to   w-tot-tot-scc          .
           move      zero                 to   w-tot-per-scc          .
           move      zero                 to   w-tot-tot-scp          .
           move      zero                 to   w-tot-per-scp          .
           move      zero                 to   w-nor-tot-fat-ctr      .
       nor-tot-fat-060.
           add       1                    to   w-nor-tot-fat-ctr      .
           if        w-nor-tot-fat-ctr    >    6
                     go to nor-tot-fat-080.
           move      zero                 to   w-tot-spe-snx
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-spe-per
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-spe-imp
                                              (w-nor-tot-fat-ctr)     .
           go to     nor-tot-fat-060.
       nor-tot-fat-080.
           move      zero                 to   w-tot-tot-sia          .
           move      zero                 to   w-tot-tot-sic          .
           move      zero                 to   w-tot-tot-spb          .
           move      zero                 to   w-tot-tot-lor          .
           move      zero                 to   w-tot-tot-spe          .
           move      zero                 to   w-tot-tot-net          .
           move      zero                 to   w-tot-tot-ibl          .
           move      zero                 to   w-tot-tot-imp          .
           move      zero                 to   w-tot-tot-doc          .
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-nor-tot-fat-ctr      .
       nor-tot-fat-100.
           add       1                    to   w-nor-tot-fat-ctr      .
           if        w-nor-tot-fat-ctr    >    12
                     go to nor-tot-fat-120.
           move      zero                 to   w-tot-iva-cod
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-nor-tot-fat-ctr)     .
           go to     nor-tot-fat-100.
       nor-tot-fat-120.
           move      zero                 to   w-tot-ctp-ele          .
           move      zero                 to   w-nor-tot-fat-ctr      .
       nor-tot-fat-140.
           add       1                    to   w-nor-tot-fat-ctr      .
           if        w-nor-tot-fat-ctr    >    20
                     go to nor-tot-fat-160.
           move      zero                 to   w-tot-ctp-cod
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-ctp-imp
                                              (w-nor-tot-fat-ctr)     .
           go to     nor-tot-fat-140.
       nor-tot-fat-160.
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-nor-tot-fat-ctr      .
       nor-tot-fat-180.
           add       1                    to   w-nor-tot-fat-ctr      .
           if        w-nor-tot-fat-ctr    >    96
                     go to nor-tot-fat-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-nor-tot-fat-ctr)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-nor-tot-fat-ctr)     .
           go to     nor-tot-fat-180.
       nor-tot-fat-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento castelletto iva                  *
      *    *-----------------------------------------------------------*
       cmp-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatori                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-cst-iva-ctr      .
           move      zero                 to   w-cmp-cst-iva-num      .
       cmp-cst-iva-100.
           add       1                    to   w-cmp-cst-iva-ctr      .
           if        w-cmp-cst-iva-ctr    >    w-tot-iva-ele
                     go to cmp-cst-iva-900.
           if        w-tot-iva-cod
                    (w-cmp-cst-iva-ctr)   =    zero
                     go to cmp-cst-iva-100.
           add       1                    to   w-cmp-cst-iva-num      .
           move      w-tot-iva-cod
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-cod
                                              (w-cmp-cst-iva-num)     .
           move      w-tot-iva-ibl
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-ibl
                                              (w-cmp-cst-iva-num)     .
           move      w-tot-iva-imp
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-imp
                                              (w-cmp-cst-iva-num)     .
           go to     cmp-cst-iva-100.
       cmp-cst-iva-900.
           move      w-cmp-cst-iva-num    to   w-tot-iva-ele          .
       cmp-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento castelletto contropartite        *
      *    *-----------------------------------------------------------*
       cmp-cst-ctp-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatori                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-cst-ctp-ctr      .
           move      zero                 to   w-cmp-cst-ctp-num      .
       cmp-cst-ctp-100.
           add       1                    to   w-cmp-cst-ctp-ctr      .
           if        w-cmp-cst-ctp-ctr    >    w-tot-ctp-ele
                     go to cmp-cst-ctp-900.
           if        w-tot-ctp-imp
                    (w-cmp-cst-ctp-ctr)   =    zero
                     go to cmp-cst-ctp-100.
           add       1                    to   w-cmp-cst-ctp-num      .
           move      w-tot-ctp-cod
                    (w-cmp-cst-ctp-ctr)   to   w-tot-ctp-cod
                                              (w-cmp-cst-ctp-num)     .
           move      w-tot-ctp-imp
                    (w-cmp-cst-ctp-ctr)   to   w-tot-ctp-imp
                                              (w-cmp-cst-ctp-num)     .

           go to     cmp-cst-ctp-100.
       cmp-cst-ctp-900.
           move      w-cmp-cst-ctp-num    to   w-tot-ctp-ele          .
       cmp-cst-ctp-999.
           exit.

      *    *===========================================================*
      *    * Scrittura in file di appoggio                             *
      *    *-----------------------------------------------------------*
       scr-fil-app-000.
      *              *-------------------------------------------------*
      *              * Scrittura record di testata                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione record [fit]                   *
      *                  *---------------------------------------------*
           perform   cmp-rec-fit-000      thru cmp-rec-fit-999        .
      *                  *---------------------------------------------*
      *                  * Composizione record in file di appoggio     *
      *                  *---------------------------------------------*
           move      spaces               to   w-rfa                  .
           move      "fit "               to   w-rfa-tip-rec          .
      *                      *-----------------------------------------*
      *                      * Castelletto scadenze                    *
      *                      *-----------------------------------------*
           move      w-tot-scd-cst        to   w-rfa-scd-cst          .
      *                      *-----------------------------------------*
      *                      * Protocolli bolle in fattura             *
      *                      *-----------------------------------------*
           move      w-buf-pbf-num-ele    to   w-rfa-pbf-num          .
           move      zero                 to   w-scr-fil-app-ctr      .
       scr-fil-app-020.
           add       1                    to   w-scr-fil-app-ctr      .
           if        w-scr-fil-app-ctr    >    w-buf-pbf-num-ele
                     go to scr-fil-app-040.
           move      w-buf-pbf-num-prt
                    (w-scr-fil-app-ctr)   to   w-rfa-pbf-prt
                                              (w-scr-fil-app-ctr)     .
           go to     scr-fil-app-020.
       scr-fil-app-040.
      *                      *-----------------------------------------*
      *                      * Record [fit]                            *
      *                      *-----------------------------------------*
           move      rf-fit               to   w-rfa-dti-rec          .
      *                  *---------------------------------------------*
      *                  * Scrittura record in file di appoggio        *
      *                  *---------------------------------------------*
           move      w-rfa                to   lsf-rec                .
           write     lsf-rec                                          .
       scr-fil-app-100.
      *              *-------------------------------------------------*
      *              * Scrittura record per le righe                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero progressivo di riga  *
      *                  *---------------------------------------------*
           move      zero                 to   w-scr-fil-app-prg      .
      *                  *---------------------------------------------*
      *                  * Start su file relative di appoggio per le   *
      *                  * righe del documento                         *
      *                  *---------------------------------------------*
           perform   fil-rlt-str-000      thru fil-rlt-str-999        .
       scr-fil-app-120.
      *                  *---------------------------------------------*
      *                  * Read Next da file relative di appoggio per  *
      *                  * le righe del documento                      *
      *                  *---------------------------------------------*
           perform   fil-rlt-nxt-000      thru fil-rlt-nxt-999        .
      *                  *---------------------------------------------*
      *                  * Test se fine file relative di appoggio      *
      *                  *---------------------------------------------*
           if        w-rlt-sts            not  = e-not-err
                     go to scr-fil-app-999.
      *                  *---------------------------------------------*
      *                  * Incremento numero progressivo di riga       *
      *                  *---------------------------------------------*
           add       100                  to   w-scr-fil-app-prg      .
       scr-fil-app-140.
      *                  *---------------------------------------------*
      *                  * Trattamento record righe documento [fir]    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Dati record in record file [fir]        *
      *                      *-----------------------------------------*
           move      rlt-rec-fir          to   rf-fir                 .
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero progressivo di ri- *
      *                      * ga                                      *
      *                      *-----------------------------------------*
           move      w-scr-fil-app-prg    to   rf-fir-num-prg         .
      *                      *-----------------------------------------*
      *                      * Composizione record in file di appoggio *
      *                      *-----------------------------------------*
           move      spaces               to   w-rfa                  .
           move      "fir "               to   w-rfa-tip-rec          .
           move      rf-fir               to   w-rfa-dti-rec          .
      *                      *-----------------------------------------*
      *                      * Scrittura record in file di appoggio    *
      *                      *-----------------------------------------*
           move      w-rfa                to   lsf-rec                .
           write     lsf-rec                                          .
       scr-fil-app-160.
      *                  *---------------------------------------------*
      *                  * Trattamento record estensione alle righe    *
      *                  * documento [fix]                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non necessario : oltre               *
      *                      *-----------------------------------------*
           if        rf-fir-des-ext       not  = 01
                     go to scr-fil-app-180.
      *                      *-----------------------------------------*
      *                      * Dati record in record file [fix]        *
      *                      *-----------------------------------------*
           move      rlt-rec-fix          to   rf-fix                 .
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero progressivo di ri- *
      *                      * ga                                      *
      *                      *-----------------------------------------*
           move      w-scr-fil-app-prg    to   rf-fix-num-prg         .
      *                      *-----------------------------------------*
      *                      * Composizione record in file di appoggio *
      *                      *-----------------------------------------*
           move      spaces               to   w-rfa                  .
           move      "fix "               to   w-rfa-tip-rec          .
           move      rf-fix               to   w-rfa-dti-rec          .
      *                      *-----------------------------------------*
      *                      * Scrittura record in file di appoggio    *
      *                      *-----------------------------------------*
           move      w-rfa                to   lsf-rec                .
           write     lsf-rec                                          .
       scr-fil-app-180.
      *                  *---------------------------------------------*
      *                  * Riciclo su lettura record [rlt]             *
      *                  *---------------------------------------------*
           go to     scr-fil-app-120.
       scr-fil-app-999.
           exit.

      *    *===========================================================*
      *    * Composizione record testata [fit]                         *
      *    *-----------------------------------------------------------*
       cmp-rec-fit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Identificativi record                       *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-fit-ide-dat         .
           move      s-ute                to   rf-fit-ide-ute         .
           move      s-fas                to   rf-fit-ide-fas         .
           move      zero                 to   rf-fit-num-prt         .
           move      w-vpb-tmo-ftr        to   rf-fit-cod-tmo         .
           move      w-vpb-tip-doc        to   rf-fit-tip-doc         .
           move      w-vpb-org-doc        to   rf-fit-org-doc         .
           move      w-vpb-num-giv        to   rf-fit-num-giv         .
           move      rr-dpz-emi           to   rf-fit-cod-dpz         .
           move      rr-dat-emi           to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-num-doc         .
           move      rr-dat-emi           to   s-dat                  .
           move      s-saa                to   rf-fit-scl-ann         .
           move      w-vpb-sgl-num        to   rf-fit-sgl-num         .
           move      w-vpb-cod-cli        to   rf-fit-cod-cli         .
           move      w-vpb-dpz-cli        to   rf-fit-dpz-cli         .
           move      w-vpb-tip-frn        to   rf-fit-tip-frn         .
           move      w-vpb-cli-plf        to   rf-fit-cli-plf         .
           move      w-vpb-dpc-plf        to   rf-fit-dpc-plf         .
           move      w-vpb-cod-lng        to   rf-fit-cod-lng         .
           move      w-vpb-sgl-vpf        to   rf-fit-sgl-vpf         .
           move      w-vpb-dec-vpf        to   rf-fit-dec-vpf         .
           move      w-vpb-tdc-vpf        to   rf-fit-tdc-vpf         .
           move      w-tot-cdc-vpf        to   rf-fit-cdc-vpf         .
           move      w-vpb-ass-iva        to   rf-fit-ass-iva         .
           move      w-vpb-ctp-ven        to   rf-fit-ctp-ven         .
           move      w-vpb-inl-dcm        to   rf-fit-inl-dcm         .
           move      w-vpb-inl-pgt        to   rf-fit-inl-pgt         .
           move      w-vpb-cod-lst        to   rf-fit-cod-lst         .
           move      w-vpb-csr-aac        to   rf-fit-csr-aac         .
           move      w-vpb-psr-aac (1)    to   rf-fit-psr-aac (1)     .
           move      w-vpb-psr-aac (2)    to   rf-fit-psr-aac (2)     .
           move      w-vpb-psr-aac (3)    to   rf-fit-psr-aac (3)     .
           move      w-vpb-psr-aac (4)    to   rf-fit-psr-aac (4)     .
           move      w-vpb-psr-aac (5)    to   rf-fit-psr-aac (5)     .
           move      w-vpb-csc-aac        to   rf-fit-csc-aac         .
           move      w-vpb-psc-aac        to   rf-fit-psc-aac         .
           move      w-vpb-cpv-aac        to   rf-fit-cpv-aac         .
           move      w-vpb-ppv-aac (1)    to   rf-fit-ppv-aac (1)     .
           move      w-vpb-ppv-aac (2)    to   rf-fit-ppv-aac (2)     .
           move      w-vpb-ppv-aac (3)    to   rf-fit-ppv-aac (3)     .
           move      w-vpb-voc-des (1)    to   rf-fit-voc-des (1)     .
           move      w-vpb-voc-des (2)    to   rf-fit-voc-des (2)     .
           move      w-vpb-voc-des (3)    to   rf-fit-voc-des (3)     .
           move      w-vpb-voc-des (4)    to   rf-fit-voc-des (4)     .
           move      w-vpb-voc-des (5)    to   rf-fit-voc-des (5)     .
           move      w-vpb-voc-des (6)    to   rf-fit-voc-des (6)     .
           move      w-vpb-cod-fop        to   rf-fit-cod-fop         .
           move      w-vpb-scp-aap        to   rf-fit-scp-aap         .
           move      w-vpb-add-spb        to   rf-fit-add-spb         .
           move      w-vpb-pag-dsm        to   rf-fit-pag-dsm         .
           move      w-vpb-cod-abi        to   rf-fit-cod-abi         .
           move      w-vpb-cod-cab        to   rf-fit-cod-cab         .
           move      w-vpb-ccc-app        to   rf-fit-ccc-app         .
           move      w-vpb-nos-ban        to   rf-fit-nos-ban         .
           move      w-vpb-nos-ccp        to   rf-fit-nos-ccp         .
           move      w-vpb-pag-qaf        to   rf-fit-pag-qaf         .
           move      w-tot-pag-act        to   rf-fit-pag-act         .
           move      w-vpb-cod-age        to   rf-fit-cod-age         .
           move      w-vpb-fsp-doc        to   rf-fit-fsp-doc         .
           move      w-tot-pvf-age        to   rf-fit-pvf-age         .
           move      w-vpb-tip-vpa        to   rf-fit-tip-vpa         .
           move      w-vpb-cpv-aaa        to   rf-fit-cpv-aaa         .
           move      w-vpb-ppv-aaa (1)    to   rf-fit-ppv-aaa (1)     .
           move      w-vpb-ppv-aaa (2)    to   rf-fit-ppv-aaa (2)     .
           move      w-vpb-ppv-aaa (3)    to   rf-fit-ppv-aaa (3)     .
           move      w-vpb-cod-ime        to   rf-fit-cod-ime         .
           move      w-vpb-pvf-ime        to   rf-fit-pvf-ime         .
           move      w-tot-tot-rig (1)    to   rf-fit-tot-rig (1)     .
           move      w-tot-tot-rig (2)    to   rf-fit-tot-rig (2)     .
           move      w-tot-tot-rig (3)    to   rf-fit-tot-rig (3)     .
           move      w-tot-tot-rig (4)    to   rf-fit-tot-rig (4)     .
           move      w-tot-tot-rig (5)    to   rf-fit-tot-rig (5)     .
           move      w-tot-tot-rig (6)    to   rf-fit-tot-rig (6)     .
           move      w-tot-tot-rig (7)    to   rf-fit-tot-rig (7)     .
           move      w-tot-tot-rig (8)    to   rf-fit-tot-rig (8)     .
           move      w-tot-tot-rig (9)    to   rf-fit-tot-rig (9)     .
           move      w-tot-tot-scc        to   rf-fit-tot-scc         .
           move      w-tot-per-scc        to   rf-fit-per-scc         .
           move      w-vpb-civ-scc        to   rf-fit-civ-scc         .
           move      w-vpb-ccp-scc        to   rf-fit-ccp-scc         .
           move      w-tot-tot-scp        to   rf-fit-tot-scp         .
           move      w-tot-per-scp        to   rf-fit-per-scp         .
           move      w-vpb-civ-scp        to   rf-fit-civ-scp         .
           move      w-vpb-ccp-scp        to   rf-fit-ccp-scp         .
      *                  *---------------------------------------------*
      *                  * Castelletto spese                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmp-rec-fit-ctr      .
       cmp-rec-fit-100.
           add       1                    to   w-cmp-rec-fit-ctr      .
           if        w-cmp-rec-fit-ctr    >    6
                     go to cmp-rec-fit-120.
           move      w-tot-spe-snx
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-snx
                                              (w-cmp-rec-fit-ctr)     .
           move      w-vpb-spe-mad
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-mad
                                              (w-cmp-rec-fit-ctr)     .
           move      w-tot-spe-per
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-per
                                              (w-cmp-rec-fit-ctr)     .
           move      w-vpb-spe-ibl
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-ibl
                                              (w-cmp-rec-fit-ctr)     .
           move      w-vpb-spe-ibt
                    (w-cmp-rec-fit-ctr)   to   rf-fit-ibt-spe
                                              (w-cmp-rec-fit-ctr)     .
           move      w-tot-spe-imp
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-imp
                                              (w-cmp-rec-fit-ctr)     .
           move      w-vpb-spe-civ
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-civ
                                              (w-cmp-rec-fit-ctr)     .
           move      w-vpb-spe-ccp
                    (w-cmp-rec-fit-ctr)   to   rf-fit-spe-ccp
                                              (w-cmp-rec-fit-ctr)     .
           go to     cmp-rec-fit-100.
       cmp-rec-fit-120.
           move      w-vpb-add-spi        to   rf-fit-add-spi         .
           move      w-vpb-civ-spi        to   rf-fit-civ-spi         .
           move      w-vpb-ccp-spi        to   rf-fit-ccp-spi         .
           move      w-tot-tot-sic        to   rf-fit-tot-sic         .
           move      w-tot-tot-sia        to   rf-fit-tot-sia         .
           move      w-tot-tot-spb        to   rf-fit-tot-spb         .
           move      w-vpb-civ-spb        to   rf-fit-civ-spb         .
           move      w-vpb-ccp-spb        to   rf-fit-ccp-spb         .
           move      zero                 to   rf-fit-prt-mgd         .
           move      zero                 to   rf-fit-nrg-mgd         .
           move      zero                 to   rf-fit-dri-mgd         .
           move      spaces               to   rf-fit-nri-mgd         .
           move      zero                 to   rf-fit-ctr-sdb         .
           move      zero                 to   rf-fit-nps-sdb         .
      *                  *---------------------------------------------*
      *                  * Castelletto Iva                             *
      *                  *---------------------------------------------*
           move      zero                 to   w-cmp-rec-fit-ctr      .
       cmp-rec-fit-200.
           add       1                    to   w-cmp-rec-fit-ctr      .
           if        w-cmp-rec-fit-ctr    >    6
                     go to cmp-rec-fit-220.
           move      w-tot-iva-cod
                    (w-cmp-rec-fit-ctr)   to   rf-fit-iva-cod
                                              (w-cmp-rec-fit-ctr)     .
           move      w-tot-iva-ibl
                    (w-cmp-rec-fit-ctr)   to   rf-fit-iva-ibl
                                              (w-cmp-rec-fit-ctr)     .
      *                      *-----------------------------------------*
      *                      * Test se omaggi                          *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-cmp-rec-fit-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     move  zero           to   rf-fit-iva-ibl
                                              (w-cmp-rec-fit-ctr)     .
           move      w-tot-iva-imp
                    (w-cmp-rec-fit-ctr)   to   rf-fit-iva-imp
                                              (w-cmp-rec-fit-ctr)     .
           go to     cmp-rec-fit-200.
       cmp-rec-fit-220.
           move      w-tot-tot-doc        to   rf-fit-iva-tdo         .
           move      zero                 to   w-cmp-rec-fit-ctr      .
       cmp-rec-fit-300.
           add       1                    to   w-cmp-rec-fit-ctr      .
           if        w-cmp-rec-fit-ctr    >    10
                     go to cmp-rec-fit-320.
           move      w-tot-ctp-cod
                    (w-cmp-rec-fit-ctr)   to   rf-fit-ctp-cod
                                              (w-cmp-rec-fit-ctr)     .
           move      w-tot-ctp-imp
                    (w-cmp-rec-fit-ctr)   to   rf-fit-ctp-imp
                                              (w-cmp-rec-fit-ctr)     .
           go to     cmp-rec-fit-300.
       cmp-rec-fit-320.
           move      zero                 to   rf-fit-ctr-stp         .
           move      spaces               to   rf-fit-flg-ela         .
      *                  *---------------------------------------------*
      *                  * Eventuale segnale di fattura da verificare  *
      *                  *---------------------------------------------*
           if        w-prs-snx-fav        =    "S"
                     move  "#"            to   rf-fit-flg-nbx (2)     . 
      *                  *---------------------------------------------*
      *                  * Flag di regime fiscale particolare          *
      *                  *---------------------------------------------*
           move      w-vpb-flg-rfp        to   rf-fit-flg-rfp         .
           move      spaces               to   rf-fit-flg-pul         .
           move      spaces               to   rf-fit-alx-exp         .
       cmp-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori prima bolla relativi al tipo movi-  *
      *    * mento per fatturazione                                    *
      *    *-----------------------------------------------------------*
       det-vpb-tmo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-vpb-tip-doc          .
           move      zero                 to   w-vpb-org-doc          .
           move      zero                 to   w-vpb-num-giv          .
           move      spaces               to   w-vpb-sgl-num          .
       det-vpb-tmo-100.
      *              *-------------------------------------------------*
      *              * Lettura record [zfi]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-vpb-tmo-ftr        to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Se record non trovato : uscita                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-tmo-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfi-tip-doc       to   w-vpb-tip-doc          .
           move      rf-zfi-org-doc       to   w-vpb-org-doc          .
           move      rf-zfi-num-giv       to   w-vpb-num-giv          .
           move      rf-zfi-sgl-num       to   w-vpb-sgl-num          .
       det-vpb-tmo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori prima bolla relativi alla forma di  *
      *    * pagamento                                                 *
      *    *-----------------------------------------------------------*
       det-vpb-fop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vpb-fop-ctr      .
       det-vpb-fop-020.
           add       1                    to   w-det-vpb-fop-ctr      .
           if        w-det-vpb-fop-ctr    >    3
                     go to det-vpb-fop-040.
           move      zero                 to   w-vpb-cod-pag
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-tip-amm
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-per-toi
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-dim-act
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-tip-pag
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-num-sca
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-dec-prs
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-dap-mes
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-dap-gio
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-ggg-int
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-tip-scm
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-gio-scm
                                              (w-det-vpb-fop-ctr)     .
           move      spaces               to   w-vpb-snx-prs
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-cau-cge
                                              (w-det-vpb-fop-ctr)     .
           move      zero                 to   w-vpb-stc-cge
                                              (w-det-vpb-fop-ctr)     .
           go to     det-vpb-fop-020.
       det-vpb-fop-040.
           move      zero                 to   w-vpb-tip-esm          .
           move      zero                 to   w-vpb-ggg-alt          .
           move      zero                 to   w-vpb-mmm-e01          .
           move      zero                 to   w-vpb-mmm-e02          .
           move      spaces               to   w-vpb-snx-cts          .
      *              *-------------------------------------------------*
      *              * Se forma di pagamento a zero : uscita           *
      *              *-------------------------------------------------*
           if        w-vpb-cod-fop        =    zero
                     go to det-vpb-fop-200.
      *              *-------------------------------------------------*
      *              * Lettura record [zfp]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-vpb-cod-fop        to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-fop-200.
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vpb-fop-ctr      .
       det-vpb-fop-100.
           add       1                    to   w-det-vpb-fop-ctr      .
           if        w-det-vpb-fop-ctr    >    3
                     go to det-vpb-fop-200.
           if        rf-zfp-cod-pag
                    (w-det-vpb-fop-ctr)   =    zero
                     go to det-vpb-fop-100.
      *                  *---------------------------------------------*
      *                  * Campi derivati da record [zfp]              *
      *                  *---------------------------------------------*
           move      rf-zfp-snx-cts       to   w-vpb-snx-cts          .
           move      rf-zfp-cod-pag
                    (w-det-vpb-fop-ctr)   to   w-vpb-cod-pag
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zfp-tip-amm
                    (w-det-vpb-fop-ctr)   to   w-vpb-tip-amm
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zfp-per-toi
                    (w-det-vpb-fop-ctr)   to   w-vpb-per-toi
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zfp-dim-act
                    (w-det-vpb-fop-ctr)   to   w-vpb-dim-act
                                              (w-det-vpb-fop-ctr)     .
      *                  *---------------------------------------------*
      *                  * Campi derivati da record [zpg]              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [zpg]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [zpg]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPAG"             to   f-key                  .
           move      w-vpb-cod-pag
                    (w-det-vpb-fop-ctr)   to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      rf-zpg-tip-pag       to   w-vpb-tip-pag
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-num-sca       to   w-vpb-num-sca
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-dec-prs       to   w-vpb-dec-prs
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-dap-mes       to   w-vpb-dap-mes
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-dap-gio       to   w-vpb-dap-gio
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-ggg-int       to   w-vpb-ggg-int
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-tip-scm       to   w-vpb-tip-scm
                                              (w-det-vpb-fop-ctr)     .
           move      rf-zpg-gio-scm       to   w-vpb-gio-scm
                                              (w-det-vpb-fop-ctr)     .
      *                  *---------------------------------------------*
      *                  * Campi derivati da lettura personalizzazioni *
      *                  * su gestione portafoglio                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione indice di comodo per ti- *
      *                      * po pagamento                            *
      *                      *-----------------------------------------*
           move      w-vpb-tip-pag
                    (w-det-vpb-fop-ctr)   to   w-det-vpb-fop-wtp      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      w-prs-gep-snx-prs
                    (w-det-vpb-fop-wtp)   to   w-vpb-snx-prs
                                              (w-det-vpb-fop-ctr)     .
           move      w-prs-gep-cau-cge
                    (w-det-vpb-fop-wtp)   to   w-vpb-cau-cge
                                              (w-det-vpb-fop-ctr)     .
           move      w-prs-gep-stc-cge
                    (w-det-vpb-fop-wtp)   to   w-vpb-stc-cge
                                              (w-det-vpb-fop-ctr)     .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vpb-fop-100.
       det-vpb-fop-200.
      *              *-------------------------------------------------*
      *              * Campi derivati da record [dcc] riportati nel    *
      *              * record di Sort                                  *
      *              *-------------------------------------------------*
           move      srt-tip-esm          to   w-vpb-tip-esm          .
           move      srt-ggg-alt          to   w-vpb-ggg-alt          .
           move      srt-mmm-e01          to   w-vpb-mmm-e01          .
           move      srt-mmm-e02          to   w-vpb-mmm-e02          .
           move      srt-snx-cts          to   w-vpb-snx-cts          .
       det-vpb-fop-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori prima bolla relativi alle spese in- *
      *    * casso                                                     *
      *    *-----------------------------------------------------------*
       det-vpb-spi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-vpb-civ-spi          .
           move      zero                 to   w-vpb-ccp-spi          .
           move      zero                 to   w-vpb-eit-spi          .
           move      zero                 to   w-det-vpb-spi-c01      .
       det-vpb-spi-020.
           add       1                    to   w-det-vpb-spi-c01      .
           if        w-det-vpb-spi-c01    >    3
                     go to det-vpb-spi-040.
           move      zero                 to   w-vpb-tpg-spi
                                              (w-det-vpb-spi-c01)     .
           move      zero                 to   w-vpb-tfu-spi
                                              (w-det-vpb-spi-c01)     .
           move      zero                 to   w-vpb-amm-spi
                                              (w-det-vpb-spi-c01)     .
           move      zero                 to   w-vpb-per-spi
                                              (w-det-vpb-spi-c01)     .
           go to     det-vpb-spi-020.
       det-vpb-spi-040.
      *              *-------------------------------------------------*
      *              * Se codice spesa incasso non esistente : uscita  *
      *              *-------------------------------------------------*
           if        w-vpb-add-spi        =    spaces
                     go to det-vpb-spi-999.
      *              *-------------------------------------------------*
      *              * Lettura record [zin] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-vpb-add-spi        to   rf-zin-cod-spi         .
           move      zero                 to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-spi-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zin-civ-spi       to   w-vpb-civ-spi          .
           move      rf-zin-ccp-spi       to   w-vpb-ccp-spi          .
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vpb-spi-c01      .
       det-vpb-spi-100.
           add       1                    to   w-det-vpb-spi-c01      .
           if        w-det-vpb-spi-c01    >    3
                     go to det-vpb-spi-999.
           if        w-vpb-cod-pag
                    (w-det-vpb-spi-c01)   =    zero
                     go to det-vpb-spi-100.
      *                  *---------------------------------------------*
      *                  * Ciclo su elementi tabella gia' presenti     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-vpb-spi-c02      .
       det-vpb-spi-200.
           add       1                    to   w-det-vpb-spi-c02      .
           if        w-det-vpb-spi-c02    >    w-vpb-eit-spi
                     go to det-vpb-spi-300.
           if        w-vpb-tpg-spi
                    (w-det-vpb-spi-c02)   =    w-vpb-tip-pag
                                              (w-det-vpb-spi-c01)
                     go to det-vpb-spi-100.
       det-vpb-spi-300.
      *                  *---------------------------------------------*
      *                  * Lettura record [zin] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-vpb-add-spi        to   rf-zin-cod-spi         .
           move      w-vpb-tip-pag
                    (w-det-vpb-spi-c01)
                                          to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : riciclo             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-spi-100.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi in tabella       *
      *                  *---------------------------------------------*
           add       1                    to   w-vpb-eit-spi          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori                      *
      *                  *---------------------------------------------*
           move      rf-zin-tip-pag       to   w-vpb-tpg-spi
                                              (w-vpb-eit-spi)         .
           move      rf-zin-tfu-spi       to   w-vpb-tfu-spi
                                              (w-vpb-eit-spi)         .
           move      rf-zin-amm-spi       to   w-vpb-amm-spi
                                              (w-vpb-eit-spi)         .
           move      rf-zin-per-spi       to   w-vpb-per-spi
                                              (w-vpb-eit-spi)         .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vpb-spi-100.
       det-vpb-spi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori prima bolla relativi alle spese     *
      *    * bollo                                                     *
      *    *-----------------------------------------------------------*
       det-vpb-spb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-vpb-civ-spb          .
           move      zero                 to   w-vpb-ccp-spb          .
           move      zero                 to   w-vpb-eit-spb          .
           move      zero                 to   w-det-vpb-spb-c01      .
       det-vpb-spb-020.
           add       1                    to   w-det-vpb-spb-c01      .
           if        w-det-vpb-spb-c01    >    3
                     go to det-vpb-spb-040.
           move      zero                 to   w-vpb-tpg-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-tau-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-per-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-det-vpb-spb-c02      .
       det-vpb-spb-030.
           add       1                    to   w-det-vpb-spb-c02      .
           if        w-det-vpb-spb-c02    >    10
                     go to det-vpb-spb-035.
           move      zero                 to   w-vpb-tbe-scg
                                              (w-det-vpb-spb-c01
                                               w-det-vpb-spb-c02)     .
           move      zero                 to   w-vpb-tbe-asc
                                              (w-det-vpb-spb-c01
                                               w-det-vpb-spb-c02)     .
           move      zero                 to   w-vpb-tbe-psc
                                              (w-det-vpb-spb-c01
                                               w-det-vpb-spb-c02)     .
           go to     det-vpb-spb-030.
       det-vpb-spb-035.
           move      zero                 to   w-vpb-tet-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-min-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-max-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-tar-spb
                                              (w-det-vpb-spb-c01)     .
           move      zero                 to   w-vpb-var-spb
                                              (w-det-vpb-spb-c01)     .
           go to     det-vpb-spb-020.
       det-vpb-spb-040.
      *              *-------------------------------------------------*
      *              * Se codice spesa bollo non esistente : uscita    *
      *              *-------------------------------------------------*
           if        w-vpb-add-spb        =    spaces
                     go to det-vpb-spb-999.
      *              *-------------------------------------------------*
      *              * Lettura record [zbo] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-vpb-add-spb        to   rf-zbo-cod-spb         .
           move      zero                 to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-spb-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbo-civ-spb       to   w-vpb-civ-spb          .
           move      rf-zbo-ccp-spb       to   w-vpb-ccp-spb          .
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vpb-spb-c01      .
       det-vpb-spb-100.
           add       1                    to   w-det-vpb-spb-c01      .
           if        w-det-vpb-spb-c01    >    3
                     go to det-vpb-spb-999.
           if        w-vpb-cod-pag
                    (w-det-vpb-spb-c01)   =    zero
                     go to det-vpb-spb-100.
      *                  *---------------------------------------------*
      *                  * Ciclo su elementi tabella gia' presenti     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-vpb-spb-c02      .
       det-vpb-spb-200.
           add       1                    to   w-det-vpb-spb-c02      .
           if        w-det-vpb-spb-c02    >    w-vpb-eit-spb
                     go to det-vpb-spb-300.
           if        w-vpb-tpg-spb
                    (w-det-vpb-spb-c02)   =    w-vpb-tip-pag
                                              (w-det-vpb-spb-c01)
                     go to det-vpb-spb-100.
       det-vpb-spb-300.
      *                  *---------------------------------------------*
      *                  * Lettura record [zbo] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-vpb-add-spb        to   rf-zbo-cod-spb         .
           move      w-vpb-tip-pag
                    (w-det-vpb-spb-c01)
                                          to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : riciclo             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vpb-spb-100.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi in tabella       *
      *                  *---------------------------------------------*
           add       1                    to   w-vpb-eit-spb          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori                      *
      *                  *---------------------------------------------*
           move      rf-zbo-tip-pag       to   w-vpb-tpg-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-tau-spb       to   w-vpb-tau-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-per-spb       to   w-vpb-per-spb
                                              (w-vpb-eit-spb)         .
           move      zero                 to   w-det-vpb-spb-c02      .
       det-vpb-spb-320.
           add       1                    to   w-det-vpb-spb-c02      .
           if        w-det-vpb-spb-c02    >    10
                     go to det-vpb-spb-340.
           move      rf-zbo-tbe-scg
                    (w-det-vpb-spb-c02)   to   w-vpb-tbe-scg
                                              (w-vpb-eit-spb,
                                               w-det-vpb-spb-c02)     .
           move      rf-zbo-tbe-asc
                    (w-det-vpb-spb-c02)   to   w-vpb-tbe-asc
                                              (w-vpb-eit-spb,
                                               w-det-vpb-spb-c02)     .
           move      rf-zbo-tbe-psc
                    (w-det-vpb-spb-c02)   to   w-vpb-tbe-psc
                                              (w-vpb-eit-spb,
                                               w-det-vpb-spb-c02)     .
           go to     det-vpb-spb-320.
       det-vpb-spb-340.
           move      rf-zbo-tet-spb       to   w-vpb-tet-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-min-spb       to   w-vpb-min-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-max-spb       to   w-vpb-max-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-tar-spb       to   w-vpb-tar-spb
                                              (w-vpb-eit-spb)         .
           move      rf-zbo-var-spb       to   w-vpb-var-spb
                                              (w-vpb-eit-spb)         .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vpb-spb-100.
       det-vpb-spb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione in riga con e-  *
      *    * ventuale aggiustamento flag di significativita' provvi-   *
      *    * gioni in riga                                             *
      *    *-----------------------------------------------------------*
       det-ppv-rig-000.
      *              *-------------------------------------------------*
      *              * Test se da effettuare                           *
      *              *-------------------------------------------------*
           if        rf-fir-fsp-rig       not  = 01
                     go to det-ppv-rig-999.
       det-ppv-rig-100.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori diretti da rf-fit o rf-fir           *
      *                  *---------------------------------------------*
           move      "PP"                 to   d-pvg-age-tip-ope      .
           move      rf-fit-cod-age       to   d-pvg-age-cod-age      .
           move      rf-fit-tip-vpa       to   d-pvg-age-tip-vpa      .
           move      rf-fit-cpv-aaa       to   d-pvg-age-cpv-aaa      .
           move      rf-fit-ppv-aaa (1)   to   d-pvg-age-ppv-aaa (1)  .
           move      rf-fit-ppv-aaa (2)   to   d-pvg-age-ppv-aaa (2)  .
           move      rf-fit-ppv-aaa (3)   to   d-pvg-age-ppv-aaa (3)  .
           move      rf-fir-tip-mag       to   d-pvg-age-tip-mag      .
           move      rf-fir-num-pro       to   d-pvg-age-num-mag      .
           move      rf-fit-cod-lst       to   d-pvg-age-cod-lst      .
           move      rf-fir-cpv-aap       to   d-pvg-age-cpv-aap      .
           move      rf-fir-ppv-aap (1)   to   d-pvg-age-ppv-aap (1)  .
           move      rf-fir-ppv-aap (2)   to   d-pvg-age-ppv-aap (2)  .
           move      rf-fir-ppv-aap (3)   to   d-pvg-age-ppv-aap (3)  .
           if        rf-fit-tip-frn       =    11
                     move  rf-fit-cod-cli to   d-pvg-age-cod-cli
           else      move  rf-fit-cli-plf to   d-pvg-age-cod-cli      .
           move      rf-fit-cpv-aac       to   d-pvg-age-cpv-aac      .
           move      rf-fit-ppv-aac (1)   to   d-pvg-age-ppv-aac (1)  .
           move      rf-fit-ppv-aac (2)   to   d-pvg-age-ppv-aac (2)  .
           move      rf-fit-ppv-aac (3)   to   d-pvg-age-ppv-aac (3)  .
           move      rf-fit-sgl-vpf       to   d-pvg-age-sgl-vpp      .
           move      rf-fir-per-scr (1)   to   d-pvg-age-per-scr (1)  .
           move      rf-fir-per-scr (2)   to   d-pvg-age-per-scr (2)  .
           move      rf-fir-per-scr (3)   to   d-pvg-age-per-scr (3)  .
           move      rf-fir-per-scr (4)   to   d-pvg-age-per-scr (4)  .
           move      rf-fir-per-scr (5)   to   d-pvg-age-per-scr (5)  .
           move      rr-dat-emi           to   d-pvg-age-dat-rif      .
       det-ppv-rig-200.
      *                  *---------------------------------------------*
      *                  * Valori calcolati                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard espresso nella    *
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      rf-fir-prz-lrs       to   d-pvg-age-prz-lrs      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        rf-fir-sgl-vps       =    rf-fit-sgl-vpf
                     go to det-ppv-rig-220.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      rf-fir-sgl-vps       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vps       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vps       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vps       to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-lrs    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      rf-fit-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-fit-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-fit-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      rf-fit-cdc-vpf       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-lrs      .
       det-ppv-rig-220.
      *                      *-----------------------------------------*
      *                      * Prezzo netto standard espresso nella    *
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      rf-fir-prz-nts       to   d-pvg-age-prz-nts      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        rf-fir-sgl-vps       =    rf-fit-sgl-vpf
                     go to det-ppv-rig-240.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      rf-fir-sgl-vps       to   w-cvs-vlt-sgl          .
           move      rf-fir-dec-vps       to   w-cvs-vlt-dec          .
           move      rf-fir-tdc-vps       to   w-cvs-vlt-tdc          .
           move      rf-fir-cdc-vps       to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-nts    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      rf-fit-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-fit-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-fit-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      rf-fit-cdc-vpf       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-nts      .
       det-ppv-rig-240.
      *                      *-----------------------------------------*
      *                      * Prezzo netto effettivo, gia' espresso   *
      *                      * nella valuta per fatturazione           *
      *                      *-----------------------------------------*
           move      rf-fir-prz-net       to   d-pvg-age-prz-net      .
       det-ppv-rig-300.
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           perform   det-pvg-age-cll-000  thru det-pvg-age-cll-999    .
       det-ppv-rig-400.
      *              *-------------------------------------------------*
      *              * Memorizzazione percentuali determinate          *
      *              *-------------------------------------------------*
           move      d-pvg-age-per-pvg (1)
                                          to   rf-fir-ppv-rig (1)     .
           move      d-pvg-age-per-pvg (2)
                                          to   rf-fir-ppv-rig (2)     .
           move      d-pvg-age-per-pvg (3)
                                          to   rf-fir-ppv-rig (3)     .
       det-ppv-rig-500.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento flag di significativi-  *
      *              * ta' provvigioni in riga                         *
      *              *-------------------------------------------------*
           if        d-pvg-age-flg-pvs    not  = spaces
                     move  03             to   rf-fir-fsp-rig         .
       det-ppv-rig-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codici iva per piede documento             *
      *    *-----------------------------------------------------------*
       det-civ-pie-000.
      *              *-------------------------------------------------*
      *              * Se cliente soggetto iva : uscita                *
      *              *-------------------------------------------------*
           if        w-vpb-ass-iva        =    zero
                     go to det-civ-pie-999.
      *                  *---------------------------------------------*
      *                  * Codice iva sconto in chiusura               *
      *                  *---------------------------------------------*
           move      w-vpb-civ-scc        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    0
                     move  w-vpb-ass-iva  to   w-vpb-civ-scc          .
      *                  *---------------------------------------------*
      *                  * Codice iva sconto pagamento                 *
      *                  *---------------------------------------------*
           move      w-vpb-civ-scp        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    0
                     move  w-vpb-ass-iva  to   w-vpb-civ-scp          .
      *                  *---------------------------------------------*
      *                  * Codice iva spese in fattura                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-civ-pie-ctr      .
       det-civ-pie-010.
           add       1                    to   w-det-civ-pie-ctr      .
           if        w-det-civ-pie-ctr    >    6
                     go to det-civ-pie-020.
           if        w-tot-spe-snx
                    (w-det-civ-pie-ctr)   =    zero
                     go to det-civ-pie-010.
           if        w-tot-spe-per
                    (w-det-civ-pie-ctr)   =    zero and
                     w-tot-spe-imp
                    (w-det-civ-pie-ctr)   =    zero
                     go to det-civ-pie-010.
           move      w-vpb-spe-civ
                    (w-det-civ-pie-ctr)   to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    0
                     move  w-vpb-ass-iva  to   w-vpb-spe-civ
                                              (w-det-civ-pie-ctr)     .
           go to     det-civ-pie-010.
       det-civ-pie-020.
      *                  *---------------------------------------------*
      *                  * Codice iva spese incasso                    *
      *                  *---------------------------------------------*
           move      w-vpb-civ-spi        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    0
                     move  w-vpb-ass-iva  to   w-vpb-civ-spi          .
      *                  *---------------------------------------------*
      *                  * Codice iva spese bollo                      *
      *                  *---------------------------------------------*
           move      w-vpb-civ-spb        to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    =    0
                     move  w-vpb-ass-iva  to   w-vpb-civ-spb          .
       det-civ-pie-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali fattura                             *
      *    *-----------------------------------------------------------*
       det-tot-fat-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-000      thru det-imp-spe-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-000      thru det-tot-nsf-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           perform   det-tot-scp-000      thru det-tot-scp-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale netto                     *
      *              *-------------------------------------------------*
           perform   det-tot-net-000      thru det-tot-net-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese incasso aggiuntive         *
      *              *-------------------------------------------------*
           perform   det-tot-sia-000      thru det-tot-sia-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese incasso e spese bollo   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese incasso calcolate          *
      *              *-------------------------------------------------*
           perform   det-tot-sic-000      thru det-tot-sic-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese bollo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese bollo                      *
      *              *-------------------------------------------------*
           perform   det-tot-spb-000      thru det-tot-spb-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale spese                     *
      *              *-------------------------------------------------*
           perform   det-tot-spe-000      thru det-tot-spe-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * dopo la determinazione di spese incasso e bollo *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
           perform   det-sca-doc-000      thru det-sca-doc-999        .
       det-tot-fat-700.
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-fat-800.
      *              *-------------------------------------------------*
      *              * Compattamento castelletto contropartite         *
      *              *-------------------------------------------------*
           perform   cmp-cst-ctp-000      thru cmp-cst-ctp-999        .
       det-tot-fat-820.
      *                  *---------------------------------------------*
      *                  * Se esubero castelletto contropartite : mes- *
      *                  * saggio                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-tot-ctp-ele        not  > w-lim-fat-max-ccp
                     go to det-tot-fat-999.
      *                      *-----------------------------------------*
      *                      * Editing codice cliente                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-fir-cod-cli       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione messaggio                  *
      *                      *-----------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "ATTENZIONE : per il cliente"
                                          to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "sono stati riscontrati documenti con un"
                                          to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Scrittura messaggio 1                   *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Scrittura messaggio 2                   *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "numero di contropartite contabili superiori a quel
      -              "lo consentito (10)."
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Scrittura messaggio 3                   *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "Il programma non e' in grado di rimediare: utilizz
      -              "are il programma fat300 per"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                      *-----------------------------------------*
      *                      * Scrittura messaggio 4                   *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "emettere le fatture manualmente."
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       det-tot-fat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale lordo                               *
      *    *-----------------------------------------------------------*
       det-tot-lor-000.
           add       w-tot-tot-rig (1)
                     w-tot-tot-rig (2)
                     w-tot-tot-rig (3)
                     w-tot-tot-rig (4)
                     w-tot-tot-rig (5)
                     w-tot-tot-rig (6)
                     w-tot-tot-rig (7)
                     w-tot-tot-rig (8)
                     w-tot-tot-rig (9)
                                        giving w-tot-tot-lor          .
       det-tot-lor-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale sconto in chiusura                  *
      *    *-----------------------------------------------------------*
       det-tot-scc-000.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-tot-tot-scc        =    zero and
                     w-tot-per-scc        =    zero
                     go to det-tot-scc-999.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo dello scon- *
      *              * to in chiusura e' stato impostato manualmente   *
      *              *-------------------------------------------------*
           if        w-tot-per-scc        =    zero
                     go to det-tot-scc-100.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           multiply  w-tot-per-scc        by   w-tot-tot-lor
                                        giving w-tot-tot-scc          .
           divide    100                  into w-tot-tot-scc
                                                     rounded          .
       det-tot-scc-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-civ-scc        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-scc        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-ccp-scc        to   w-agg-cst-ctp-cod      .
           move      w-tot-tot-scc        to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       det-tot-scc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto dello sonto in chiusura    *
      *    *-----------------------------------------------------------*
       det-tot-nsc-000.
           subtract  w-tot-tot-scc        from w-tot-tot-lor
                                        giving w-tot-tot-nsc          .
       det-tot-nsc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo spese in fattura                   *
      *    *-----------------------------------------------------------*
       det-imp-spe-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        w-tot-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        w-tot-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     w-tot-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        w-tot-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-500.
      *              *-------------------------------------------------*
      *              * Calcolo in base alla percentuale                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione imponibile per la spesa     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-imp-spe-ibl      .
      *                  *---------------------------------------------*
      *                  * Determinazione imponibile per la spesa      *
      *                  *---------------------------------------------*
           if        w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-220
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   w-vpb-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-250.
       det-imp-spe-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-vpb-spe-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-225.
       det-imp-spe-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-tot-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving w-tot-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into w-tot-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      w-tot-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-spe-ccp
                    (w-det-imp-spe-c01)   to   w-agg-cst-ctp-cod      .
           move      w-tot-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-100.
       det-imp-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in fattura     *
      *    *-----------------------------------------------------------*
       det-tot-nsf-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        w-tot-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       w-tot-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-100.
       det-tot-nsf-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale sconto pagamento                    *
      *    *-----------------------------------------------------------*
       det-tot-scp-000.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-tot-tot-scp        =    zero and
                     w-tot-per-scp        =    zero
                     go to det-tot-scp-999.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo dello scon- *
      *              * to pagamento e' stato impostato manualmente     *
      *              *-------------------------------------------------*
           if        w-tot-per-scp        =    zero
                     go to det-tot-scp-100.
      *              *-------------------------------------------------*
      *              * Calcolo, a seconda della personalizzazione sul  *
      *              * tipo di imponibile, che indica se prendere o    *
      *              * no in considerazione le spese                   *
      *              *-------------------------------------------------*
           if        w-prs-tim-scp-tip    =    "N"
                     multiply  w-tot-per-scp
                                          by   w-tot-tot-nsc
                                        giving w-tot-tot-scp
           else      multiply  w-tot-per-scp
                                          by   w-tot-tot-nsf
                                        giving w-tot-tot-scp          .
           divide    100                  into w-tot-tot-scp
                                       rounded                        .
       det-tot-scp-100.
      *              *-------------------------------------------------*
      *              * Test sulla personalizzazione                    *
      *              *-------------------------------------------------*
           if        w-prs-tim-scp-tip    =    "A"
                     go to det-tot-scp-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-civ-scp        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-scp        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-ccp-scp        to   w-agg-cst-ctp-cod      .
           move      w-tot-tot-scp        to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       det-tot-scp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale netto                               *
      *    *-----------------------------------------------------------*
       det-tot-net-000.
      *              *-------------------------------------------------*
      *              * Detrazione sconto in chiusura                   *
      *              *-------------------------------------------------*
           subtract  w-tot-tot-scc        from w-tot-tot-lor
                                        giving w-tot-tot-net          .
      *              *-------------------------------------------------*
      *              * Test sulla personalizzazione                    *
      *              *-------------------------------------------------*
           if        w-prs-tim-scp-tip    =    "A"
                     go to det-tot-net-999.
      *              *-------------------------------------------------*
      *              * Detrazione sconto pagamento                     *
      *              *-------------------------------------------------*
           subtract  w-tot-tot-scp        from w-tot-tot-net          .
       det-tot-net-999.
           exit.

      *    *===========================================================*
      *    * Determinazione spese incasso calcolate                    *
      *    *-----------------------------------------------------------*
       det-tot-sic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione totale spese incasso calcolate  *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Ciclo su tabella scadenze                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-sic-c01      .
       det-tot-sic-100.
           add       1                    to   w-det-tot-sic-c01      .
           if        w-det-tot-sic-c01    >    w-tot-scd-ele
                     go to det-tot-sic-500.
      *                  *---------------------------------------------*
      *                  * Test se esistono spese incasso per il tipo  *
      *                  * scadenza in esame                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scansione tabella spese incasso         *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-sic-c02      .
       det-tot-sic-200.
           add       1                    to   w-det-tot-sic-c02      .
           if        w-det-tot-sic-c02    >    w-vpb-eit-spi
                     go to det-tot-sic-100.
      *                      *-----------------------------------------*
      *                      * Se tipo pagamento non corrisponde a     *
      *                      * quello della scadenza in esame : rici-  *
      *                      * clo su prossima scadenza                *
      *                      *-----------------------------------------*
           if        w-vpb-tpg-spi
                    (w-det-tot-sic-c02)   not  = w-tot-scd-tip
                                                (w-det-tot-sic-c01)
                     go to det-tot-sic-200.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo funzio- *
      *                      * namento spesa incasso                   *
      *                      *-----------------------------------------*
           if        w-vpb-tfu-spi
                    (w-det-tot-sic-c02)   =    01
                     go to det-tot-sic-220
           else if   w-vpb-tfu-spi
                    (w-det-tot-sic-c02)   =    02
                     go to det-tot-sic-240.
       det-tot-sic-220.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : automatico         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento accumulatore importo  *
      *                          * spese incasso                       *
      *                          *-------------------------------------*
           add       w-vpb-amm-spi
                    (w-det-tot-sic-c02)   to   w-tot-tot-sic          .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-sic-100.
       det-tot-sic-240.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : manuale            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-sic-100.
       det-tot-sic-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-civ-spi        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-sic        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-ccp-spi        to   w-agg-cst-ctp-cod      .
           move      w-tot-tot-sic        to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       det-tot-sic-999.
           exit.

      *    *===========================================================*
      *    * Determinazione spese incasso aggiuntive                   *
      *    *-----------------------------------------------------------*
       det-tot-sia-000.
      *              *-------------------------------------------------*
      *              * Se spese incasso aggiuntive zero : uscita       *
      *              *-------------------------------------------------*
           if        w-tot-tot-sia        =    zero
                     go to det-tot-sia-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-civ-spi        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-sia        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-ccp-spi        to   w-agg-cst-ctp-cod      .
           move      w-tot-tot-sia        to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       det-tot-sia-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale spese bollo                         *
      *    *-----------------------------------------------------------*
       det-tot-spb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione spese bollo                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-spb          .
      *              *-------------------------------------------------*
      *              * Ciclo su tabella scadenze                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-spb-c01      .
       det-tot-spb-100.
           add       1                    to   w-det-tot-spb-c01      .
           if        w-det-tot-spb-c01    >    w-tot-scd-ele
                     go to det-tot-spb-500.
      *                  *---------------------------------------------*
      *                  * Test se esistono spese bollo per il tipo    *
      *                  * scadenza in esame                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scansione tabella spese bollo           *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c02      .
       det-tot-spb-200.
           add       1                    to   w-det-tot-spb-c02      .
           if        w-det-tot-spb-c02    >    w-vpb-eit-spb
                     go to det-tot-spb-100.
      *                      *-----------------------------------------*
      *                      * Se tipo pagamento non corrisponde a     *
      *                      * quello della scadenza in esame : riciclo*
      *                      *-----------------------------------------*
           if        w-vpb-tpg-spb
                    (w-det-tot-spb-c02)   not  = w-tot-scd-tip
                                                (w-det-tot-spb-c01)
                     go to det-tot-spb-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione work per importo spesa      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tot-spb-wis      .
      *                  *---------------------------------------------*
      *                  * Se importo scadenza non e' maggiore del     *
      *                  * tetto di esenzione : riciclo su prossima    *
      *                  * scadenza                                    *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-det-tot-spb-c01)   not  > w-vpb-tet-spb
                                                (w-det-tot-spb-c02)
                     go to det-tot-spb-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo automatismo *
      *                  * spesa bollo                                 *
      *                  *---------------------------------------------*
           if        w-vpb-tau-spb
                    (w-det-tot-spb-c02)   =    01
                     go to det-tot-spb-220
           else if   w-vpb-tau-spb
                    (w-det-tot-spb-c02)   =    02
                     go to det-tot-spb-240
           else if   w-vpb-tau-spb
                    (w-det-tot-spb-c02)   =    03
                     go to det-tot-spb-260.
       det-tot-spb-220.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : in percentuale sul tota- *
      *                  * le scadenza                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione importo spesa            *
      *                      *-----------------------------------------*
           multiply  w-vpb-per-spb
                    (w-det-tot-spb-c02)   by   w-tot-scd-imp
                                              (w-det-tot-spb-c01)
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wis
                                                         rounded      .
      *                      *-----------------------------------------*
      *                      * A trattamento arrotondamento            *
      *                      *-----------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-240.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : a scaglioni              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo per 10 scaglioni                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c03      .
       det-tot-spb-242.
           add       1                    to   w-det-tot-spb-c03      .
           if        w-det-tot-spb-c03    >    10
                     go to det-tot-spb-244.
           if        w-vpb-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   =    zero
                     go to det-tot-spb-242.
           if        w-tot-scd-imp
                    (w-det-tot-spb-c01)   >    w-vpb-tbe-scg
                                              (w-det-tot-spb-c02,
                                               w-det-tot-spb-c03)
                     go to det-tot-spb-242.
           move      w-vpb-tbe-asc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   to   w-det-tot-spb-wis      .
       det-tot-spb-244.
      *                      *-----------------------------------------*
      *                      * A trattamento arrotondamento            *
      *                      *-----------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-260.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : a scaglioni in percentua-*
      *                  *                    le                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione work per progressivo   *
      *                      * imponibile                              *
      *                      *-----------------------------------------*
           move      w-tot-scd-imp
                    (w-det-tot-spb-c01)   to   w-det-tot-spb-wpi      .
      *                      *-----------------------------------------*
      *                      * Ciclo per 10 scaglioni                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c03      .
       det-tot-spb-262.
           add       1                    to   w-det-tot-spb-c03      .
           if        w-det-tot-spb-c03    >    10
                     go to det-tot-spb-270.
           if        w-vpb-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   =    zero
                     go to det-tot-spb-262.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che l'importo sca- *
      *                      * glione sia maggiore o meno dell'importo *
      *                      * scadenza                                *
      *                      *-----------------------------------------*
           if        w-vpb-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   >    w-tot-scd-imp
                                              (w-det-tot-spb-c01)
                     go to det-tot-spb-270
           else      go to det-tot-spb-264.
       det-tot-spb-264.
      *                      *-----------------------------------------*
      *                      * Se l'importo scaglione e' minore o u-   *
      *                      * guale dell'importo scadenza             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Calcolo quota spesa bollo relativa  *
      *                          * allo scaglione                      *
      *                          *-------------------------------------*
           multiply  w-vpb-tbe-psc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)
                                          by   w-vpb-tbe-scg
                                              (w-det-tot-spb-c02,
                                               w-det-tot-spb-c03)
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wss
                                                         rounded      .
      *                          *-------------------------------------*
      *                          * Aggiornamento importo spesa         *
      *                          *-------------------------------------*
           add       w-det-tot-spb-wss    to   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo imponibile*
      *                          *-------------------------------------*
           subtract  w-vpb-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   from w-det-tot-spb-wpi      .
      *                          *-------------------------------------*
      *                          * Riciclo su prossimo scaglione       *
      *                          *-------------------------------------*
           go to     det-tot-spb-262.
       det-tot-spb-270.
      *                      *-----------------------------------------*
      *                      * Se l'importo scaglione e' superiore al- *
      *                      * l'importo scadenza                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Calcolo quota spesa bollo relativa  *
      *                          * al residuo, cioe' il progressivo im-*
      *                          * ponibile sin qui determinato        *
      *                          *-------------------------------------*
           multiply  w-vpb-tbe-psc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)
                                          by   w-det-tot-spb-wpi
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wss
                                                         rounded      .
      *                          *-------------------------------------*
      *                          * Aggiornamento importo spesa         *
      *                          *-------------------------------------*
           add       w-det-tot-spb-wss    to   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A trattamento arrotondamento        *
      *                          *-------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-300.
      *                  *---------------------------------------------*
      *                  * Trattamento arrotondamento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se esiste un valore minimo per la spesa *
      *                      *-----------------------------------------*
           if        w-vpb-min-spb
                    (w-det-tot-spb-c02)   =    zero
                     go to det-tot-spb-302.
      *                      *-----------------------------------------*
      *                      * Se l'importo spesa sin qui determinato  *
      *                      * e' inferiore al minimo valore, si forza *
      *                      * il valore minimo                        *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    <    w-vpb-min-spb
                                              (w-det-tot-spb-c02)
                     move  w-vpb-min-spb
                          (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis      .
       det-tot-spb-302.
      *                      *-----------------------------------------*
      *                      * Se esiste un valore massimo per la spesa*
      *                      *-----------------------------------------*
           if        w-vpb-max-spb
                    (w-det-tot-spb-c02)   =    zero
                     go to det-tot-spb-304.
      *                      *-----------------------------------------*
      *                      * Se l'importo spesa sin qui determinato  *
      *                      * e' superiore al massimo valore, si forza*
      *                      * il valore massimo                       *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    >    w-vpb-max-spb
                                              (w-det-tot-spb-c02)
                     move  w-vpb-max-spb
                          (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis      .
       det-tot-spb-304.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo arroton-*
      *                      * damento                                 *
      *                      *-----------------------------------------*
           go to     det-tot-spb-310
                     det-tot-spb-320
                     det-tot-spb-330
                     det-tot-spb-340
                                depending on   w-vpb-tar-spb
                                              (w-det-tot-spb-c02)     .
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : nessuno           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scadenze       *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-310.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : al valore superio-*
      *                      *                       re                *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    >    zero
                     add      w-vpb-var-spb
                             (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis
                     subtract 1           from w-det-tot-spb-wis
           else      subtract w-vpb-var-spb
                             (w-det-tot-spb-c02)
                                          from w-det-tot-spb-wis
                     add      1           to   w-det-tot-spb-wis      .
           divide    w-vpb-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-vpb-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-320.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : al valore inferio-*
      *                      *                       re                *
      *                      *-----------------------------------------*
           divide    w-vpb-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-vpb-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-330.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : commerciale       *
      *                      *-----------------------------------------*
           divide    w-vpb-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis
                                                         rounded      .
           multiply  w-vpb-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-340.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : statale           *
      *                      *-----------------------------------------*
           divide    2                    into w-vpb-var-spb
                                              (w-det-tot-spb-c02)
                                        giving w-det-tot-spb-wpc
                                                          rounded     .
           if        w-det-tot-spb-wis    >    zero
                     add      w-det-tot-spb-wpc
                                          to   w-det-tot-spb-wis
                     subtract 1           from w-det-tot-spb-wis
           else      subtract w-det-tot-spb-wpc
                                          from w-det-tot-spb-wis
                     add      1           to   w-det-tot-spb-wis      .
           divide    w-vpb-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-vpb-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scadenze       *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-400.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale spese bollo            *
      *                  *---------------------------------------------*
           add       w-det-tot-spb-wis    to   w-tot-tot-spb          .
      *                  *---------------------------------------------*
      *                  * Riciclo su prossima scadenza                *
      *                  *---------------------------------------------*
           go to     det-tot-spb-100.
       det-tot-spb-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-vpb-civ-spb        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-spb        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto contropartite         *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-ctp-tip      .
           move      w-vpb-ccp-spb        to   w-agg-cst-ctp-cod      .
           move      w-tot-tot-spb        to   w-agg-cst-ctp-imp      .
           perform   agg-cst-ctp-000      thru agg-cst-ctp-999        .
       det-tot-spb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale spese                               *
      *    *-----------------------------------------------------------*
       det-tot-spe-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totale spese                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-spe          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-spe-ctr      .
       det-tot-spe-100.
           add       1                    to   w-det-tot-spe-ctr      .
           if        w-det-tot-spe-ctr    >    6
                     go to det-tot-spe-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        w-tot-spe-snx
                    (w-det-tot-spe-ctr)   =    0
                     go to det-tot-spe-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       w-tot-spe-imp
                    (w-det-tot-spe-ctr)   to   w-tot-tot-spe          .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-spe-100.
       det-tot-spe-200.
      *              *-------------------------------------------------*
      *              * Spese incasso calcolate                         *
      *              *-------------------------------------------------*
           add       w-tot-tot-sic        to   w-tot-tot-spe          .
      *              *-------------------------------------------------*
      *              * Spese incasso aggiuntive                        *
      *              *-------------------------------------------------*
           add       w-tot-tot-sia        to   w-tot-tot-spe          .
      *              *-------------------------------------------------*
      *              * Spese bollo                                     *
      *              *-------------------------------------------------*
           add       w-tot-tot-spb        to   w-tot-tot-spe          .
       det-tot-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale imponibile                          *
      *    *-----------------------------------------------------------*
       det-tot-ibl-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totalizzatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-ibl          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione castelletto iva              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-ibl-ctr      .
       det-tot-ibl-100.
           add       1                    to   w-det-tot-ibl-ctr      .
           if        w-det-tot-ibl-ctr    >    12
                     go to det-tot-ibl-999.
      *
           move      w-tot-iva-cod
                    (w-det-tot-ibl-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tot-ibl-100.
           add       w-tot-iva-ibl
                    (w-det-tot-ibl-ctr)
                                          to   w-tot-tot-ibl          .
           go to     det-tot-ibl-100.
       det-tot-ibl-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale imposta                             *
      *    *-----------------------------------------------------------*
       det-tot-imp-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totalizzatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-imp          .
           move      zero                 to   w-det-tot-imp-wpa      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione castelletto iva              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-imp-ctr      .
       det-tot-imp-100.
           add       1                    to   w-det-tot-imp-ctr      .
           if        w-det-tot-imp-ctr    >    6
                     go to det-tot-imp-999.
      *                  *---------------------------------------------*
      *                  * Test se imposta da calcolare                *
      *                  *---------------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    <    1 or
                     w-edt-iva-cod-003    >    8
                     go to det-tot-imp-120
           else      go to det-tot-imp-100.
       det-tot-imp-120.
      *                  *---------------------------------------------*
      *                  * Determinazione aliquota per il calcolo      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che si tratti di   *
      *                      * un aliquota di sola iva (omaggio) o no  *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tot-imp-122
           else      go to det-tot-imp-124.
       det-tot-imp-122.
      *                      *-----------------------------------------*
      *                      * Se aliquota di sola iva (omaggio)       *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-det-tot-imp-wci      .
           subtract  900                  from w-det-tot-imp-wci      .
           go to     det-tot-imp-140.
       det-tot-imp-124.
      *                      *-----------------------------------------*
      *                      * Se aliquota normale                     *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-det-tot-imp-wci      .
           go to     det-tot-imp-140.
       det-tot-imp-140.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "DI"                 to   d-imp-iva-tip-ope      .
           move      w-tot-iva-ibl
                    (w-det-tot-imp-ctr)   to   d-imp-iva-ibl-iva      .
           move      w-det-tot-imp-wci    to   d-imp-iva-cod-iva      .
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
      *                  *---------------------------------------------*
      *                  * Aggiornamento imposta relativa all'aliquota *
      *                  *---------------------------------------------*
           move      d-imp-iva-ims-iva    to   w-tot-iva-imp
                                              (w-det-tot-imp-ctr)     .
      *                  *---------------------------------------------*
      *                  * Aggiornamento accumulatore                  *
      *                  *---------------------------------------------*
           add       d-imp-iva-ims-iva    to   w-tot-tot-imp          .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-imp-100.
       det-tot-imp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale documento                           *
      *    *-----------------------------------------------------------*
       det-tot-doc-000.
           add       w-tot-tot-ibl
                     w-tot-tot-imp      giving w-tot-tot-doc          .
       det-tot-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione scadenze per il documento                  *
      *    *-----------------------------------------------------------*
       det-sca-doc-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri in input                 *
      *              *-------------------------------------------------*
           move      w-vpb-cod-fop        to   w-clc-tbl-scd-fdp      .
           move      rr-dat-emi           to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
       det-sca-doc-060.
      *              *-------------------------------------------------*
      *              * Regime di Scissione dei Pagamenti               *
      *              *-------------------------------------------------*
           if        w-vpb-tas-ivc        =    04
                     move  zero           to   w-clc-tbl-scd-iva      .
       det-sca-doc-080.
           move      w-vpb-pag-qaf        to   w-clc-tbl-scd-qaf      .
           move      w-tot-pag-act        to   w-clc-tbl-scd-acc      .
           move      w-vpb-pag-dsm        to   w-clc-tbl-scd-ddp      .
           move      zero                 to   w-det-sca-doc-ctr      .
       det-sca-doc-100.
           add       1                    to   w-det-sca-doc-ctr      .
           if        w-det-sca-doc-ctr    >    3
                     go to det-sca-doc-120.
           move      w-vpb-cod-pag
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-cdp
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-tip-amm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tam
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-per-toi
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-pti
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-dim-act
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dda
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-tip-pag
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tpg
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-num-sca
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-nsp
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-dec-prs
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dps
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-dap-mes
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dpm
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-dap-gio
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dpg
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-ggg-int
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-gdi
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-tip-scm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tsm
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-gio-scm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-gfs
                                              (w-det-sca-doc-ctr)     .
           move      w-vpb-cau-cge
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-ccp
                                              (w-det-sca-doc-ctr)     .
           go to     det-sca-doc-100.
       det-sca-doc-120.
           move      w-vpb-tip-esm        to   w-clc-tbl-scd-esm      .
           move      w-vpb-ggg-alt        to   w-clc-tbl-scd-alt      .
           move      w-vpb-mmm-e01        to   w-clc-tbl-scd-e01      .
           move      w-vpb-mmm-e02        to   w-clc-tbl-scd-e02      .
       det-sca-doc-200.
      *              *-------------------------------------------------*
      *              * Richiamo routine di calcolo                     *
      *              *-------------------------------------------------*
           perform   clc-tbl-scd-000      thru clc-tbl-scd-999        .
       det-sca-doc-300.
      *              *-------------------------------------------------*
      *              * Movimento da work locale a work totali fattura  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sca-doc-ctr      .
       det-sca-doc-310.
           add       1                    to   w-det-sca-doc-ctr      .
           if        w-det-sca-doc-ctr    >    w-clc-tbl-scd-nts
                     go to det-sca-doc-320.
           move      w-clc-tbl-scd-tds
                    (w-det-sca-doc-ctr)   to   w-tot-scd-tip
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-dds
                    (w-det-sca-doc-ctr)   to   w-tot-scd-dat
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-ccs
                    (w-det-sca-doc-ctr)   to   w-tot-scd-cau
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-ids
                    (w-det-sca-doc-ctr)   to   w-tot-scd-imp
                                              (w-det-sca-doc-ctr)     .
           go to     det-sca-doc-310.
       det-sca-doc-320.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero totale scadenze          *
      *              *-------------------------------------------------*
           move      w-clc-tbl-scd-nts    to   w-tot-scd-ele          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sca-doc-999.
       det-sca-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione ultimo documento emesso                    *
      *    *-----------------------------------------------------------*
       det-ult-doc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati in uscita                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-ult-doc-ctr      .
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-det-ult-doc-dte    =    zero
                     go to det-ult-doc-900.
       det-ult-doc-100.
      *              *-------------------------------------------------*
      *              * Start su file [fit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-det-ult-doc-dte    to   rf-fit-dat-doc         .
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
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ult-doc-900.
       det-ult-doc-200.
      *              *-------------------------------------------------*
      *              * Read-next                                       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se at end : ad uscita                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ult-doc-900.
       det-ult-doc-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
       det-ult-doc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-fit-dat-doc       =    w-det-ult-doc-dte
                     go to det-ult-doc-200.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-det-ult-doc-vld    =    02  and
                     rf-fit-cod-dpz       not  = w-det-ult-doc-dpe
                     go to det-ult-doc-200.
      *                  *---------------------------------------------*
      *                  * Test su giornale Iva                        *
      *                  *---------------------------------------------*
           if        rf-fit-num-giv       not  = w-det-ult-doc-giv
                     go to det-ult-doc-200.
      *                  *---------------------------------------------*
      *                  * Test su sigla numerazione                   *
      *                  *---------------------------------------------*
           if        rf-fit-sgl-num       not  = w-det-ult-doc-sgl
                     go to det-ult-doc-200.
       det-ult-doc-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-ult-doc-ctr      .
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ult-doc-200.
       det-ult-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ult-doc-999.
       det-ult-doc-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento castelletto iva                             *
      *    *-----------------------------------------------------------*
       agg-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Se importo a zero : uscita                      *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-imp    =    zero
                     go to agg-cst-iva-999.
      *              *-------------------------------------------------*
      *              * Inversione del segno in caso di aggiornamento   *
      *              * negativo                                        *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-tip    =    "-"
                     multiply -1          by   w-agg-cst-iva-imp      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che il campo abbia un'ali- *
      *              * quota propria oppure debba essere sventagliato  *
      *              * su tutto il castelletto iva                     *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-coi    not  = zero
                     go to agg-cst-iva-200
           else      go to agg-cst-iva-300.
       agg-cst-iva-200.
      *                  *---------------------------------------------*
      *                  * Caso in cui abbia un'aliquota propria       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento riga castelletto iva      *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-220.
           add       1                    to   w-agg-cst-iva-ctr      .
           if        w-agg-cst-iva-ctr    >    12
                     move  12             to   w-agg-cst-iva-ctr
                     go to agg-cst-iva-260.
           if        w-tot-iva-cod        
                    (w-agg-cst-iva-ctr)   =    zero
                     go to agg-cst-iva-240.
           if        w-agg-cst-iva-coi    =    w-tot-iva-cod
                                              (w-agg-cst-iva-ctr)
                     go to agg-cst-iva-260.
           go to     agg-cst-iva-220.
       agg-cst-iva-240.
           add       1                    to   w-tot-iva-ele          .
           move      w-agg-cst-iva-coi    to   w-tot-iva-cod
                                              (w-agg-cst-iva-ctr)     .
       agg-cst-iva-260.
           add       w-agg-cst-iva-imp    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)     .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-cst-iva-999.
       agg-cst-iva-300.
      *                  *---------------------------------------------*
      *                  * Caso in cui debba essere sventagliato sul   *
      *                  * castelletto iva gia' esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione del numero di elementi   *
      *                      * presenti in castelletto iva e del tota- *
      *                      * le imponibile                           *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-tot      .
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-320.
           add       1                    to   w-agg-cst-iva-ctr      .
      *                      *-----------------------------------------*
      *                      * Se codice iva a zero : fine scansione   *
      *                      *-----------------------------------------*
           if        w-tot-iva-cod        
                    (w-agg-cst-iva-ctr)   =    zero
                     go to agg-cst-iva-330.
      *                      *-----------------------------------------*
      *                      * Se codice sola iva (omaggio) : riciclo  *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-agg-cst-iva-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to agg-cst-iva-320.
           add       w-tot-iva-ibl
                    (w-agg-cst-iva-ctr)   to   w-agg-cst-iva-tot      .
           go to     agg-cst-iva-320.
       agg-cst-iva-330.
      *                      *-----------------------------------------*
      *                      * Memorizzazione numero elementi in ca-   *
      *                      * stelletto iva                           *
      *                      *-----------------------------------------*
           move      w-agg-cst-iva-ctr    to   w-agg-cst-iva-max      .
      *                      *-----------------------------------------*
      *                      * Ciclo di aggiornamento castelletto iva  *
      *                      * fino all'elemento  w-agg-cst-iva-max    *
      *                      * escluso                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-wpa      .
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-340.
           add       1                    to   w-agg-cst-iva-ctr      .
           if        w-agg-cst-iva-ctr    =    w-agg-cst-iva-max
                     go to agg-cst-iva-360.
      *                          *-------------------------------------*
      *                          * Se codice sola iva (omaggio) : ri-  *
      *                          * ciclo                               *
      *                          *-------------------------------------*
           move      w-tot-iva-cod
                    (w-agg-cst-iva-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to agg-cst-iva-340.
      *                          *-------------------------------------*
      *                          * Determinazione porzione dell'impor- *
      *                          * to relativa alla riga di castelletto*
      *                          * iva                                 *
      *                          *-------------------------------------*
           multiply  w-agg-cst-iva-imp    by   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)
                                        giving w-agg-cst-iva-s18      .
           divide    w-agg-cst-iva-tot    into w-agg-cst-iva-s18
                                                         rounded      .
           add       w-agg-cst-iva-s18    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)     .
           add       w-agg-cst-iva-s18    to   w-agg-cst-iva-wpa      .
           go to     agg-cst-iva-340.
       agg-cst-iva-360.
      *                      *-----------------------------------------*
      *                      * Trattamento ultimo elemento castelletto *
      *                      *-----------------------------------------*
           subtract  w-agg-cst-iva-wpa    from w-agg-cst-iva-imp
                                        giving w-agg-cst-iva-s11      .
           add       w-agg-cst-iva-s11    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-max)     .
       agg-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento castelletto contropartite                   *
      *    *-----------------------------------------------------------*
       agg-cst-ctp-000.
      *              *-------------------------------------------------*
      *              * Se importo a zero : uscita                      *
      *              *-------------------------------------------------*
           if        w-agg-cst-ctp-imp    =    zero
                     go to agg-cst-ctp-999.
      *              *-------------------------------------------------*
      *              * Inversione del segno in caso di aggiornamento   *
      *              * negativo                                        *
      *              *-------------------------------------------------*
           if        w-agg-cst-ctp-tip    =    "-"
                     multiply -1          by   w-agg-cst-ctp-imp      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che il campo abbia una con-*
      *              * tropartita propria oppure debba essere sventa-  *
      *              * gliato su tutto il castelletto contropartite    *
      *              *-------------------------------------------------*
           if        w-agg-cst-ctp-cod    not  = zero
                     go to agg-cst-ctp-200
           else      go to agg-cst-ctp-300.
       agg-cst-ctp-200.
      *                  *---------------------------------------------*
      *                  * Caso in cui abbia una contropartita propria *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento riga castelletto contro-  *
      *                      * partite                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-ctp-ctr      .
       agg-cst-ctp-220.
           add       1                    to   w-agg-cst-ctp-ctr      .
           if        w-agg-cst-ctp-ctr    >    20
                     move  20             to   w-agg-cst-ctp-ctr
                     go to agg-cst-ctp-260.
           if        w-tot-ctp-cod        
                    (w-agg-cst-ctp-ctr)   =    zero
                     go to agg-cst-ctp-240.
           if        w-agg-cst-ctp-cod    =    w-tot-ctp-cod
                                              (w-agg-cst-ctp-ctr)
                     go to agg-cst-ctp-260.
           go to     agg-cst-ctp-220.
       agg-cst-ctp-240.
           add       1                    to   w-tot-ctp-ele          .
           move      w-agg-cst-ctp-cod    to   w-tot-ctp-cod
                                              (w-agg-cst-ctp-ctr)     .
       agg-cst-ctp-260.
           add       w-agg-cst-ctp-imp    to   w-tot-ctp-imp
                                              (w-agg-cst-ctp-ctr)     .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-cst-ctp-999.
       agg-cst-ctp-300.
      *                  *---------------------------------------------*
      *                  * Caso in cui debba essere sventagliato sul   *
      *                  * castelletto contropartite gia' esistente    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione del numero di elementi   *
      *                      * presenti in castelletto contropartite e *
      *                      * del totale                              *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-ctp-tot      .
           move      zero                 to   w-agg-cst-ctp-ctr      .
       agg-cst-ctp-320.
           add       1                    to   w-agg-cst-ctp-ctr      .
           if        w-tot-ctp-cod        
                    (w-agg-cst-ctp-ctr)
                                          =    zero
                     go to agg-cst-ctp-330.
           add       w-tot-ctp-imp
                    (w-agg-cst-ctp-ctr)   to   w-agg-cst-ctp-tot      .
           go to     agg-cst-ctp-320.
       agg-cst-ctp-330.
      *                      *-----------------------------------------*
      *                      * Memorizzazione numero elementi in ca-   *
      *                      * stelletto contropartite                 *
      *                      *-----------------------------------------*
           move      w-agg-cst-ctp-ctr    to   w-agg-cst-ctp-max      .
      *                      *-----------------------------------------*
      *                      * Ciclo di aggiornamento castelletto con- *
      *                      * tropartite fino all'elemento            *
      *                      * w-agg-cst-ctp-max escluso               *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-ctp-wpa      .
           move      zero                 to   w-agg-cst-ctp-ctr      .
       agg-cst-ctp-340.
           add       1                    to   w-agg-cst-ctp-ctr      .
           if        w-agg-cst-ctp-ctr    =    w-agg-cst-ctp-max
                     go to agg-cst-ctp-360.
      *                          *-------------------------------------*
      *                          * Determinazione porzione sconto in   *
      *                          * chiusura relativa alla riga di ca-  *
      *                          * stelletto contropartite             *
      *                          *-------------------------------------*
           multiply  w-agg-cst-ctp-imp    by   w-tot-ctp-imp
                                              (w-agg-cst-ctp-ctr)
                                        giving w-agg-cst-ctp-s18      .
           divide    w-agg-cst-ctp-tot    into w-agg-cst-ctp-s18
                                       rounded                        .
           add       w-agg-cst-ctp-s18    to   w-tot-ctp-imp
                                              (w-agg-cst-ctp-ctr)     .
           add       w-agg-cst-ctp-s18    to   w-agg-cst-ctp-wpa      .
           go to     agg-cst-ctp-340.
       agg-cst-ctp-360.
      *                      *-----------------------------------------*
      *                      * Trattamento ultimo elemento castelletto *
      *                      *-----------------------------------------*
           subtract  w-agg-cst-ctp-wpa    from w-agg-cst-ctp-imp
                                        giving w-agg-cst-ctp-s11      .
           add       w-agg-cst-ctp-s11    to   w-tot-ctp-imp
                                              (w-agg-cst-ctp-max)     .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-cst-ctp-999.
       agg-cst-ctp-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *                                                           *
      *    * Input  : w-lvl-prz-prz = Prezzo da sottoporre a legame    *
      *    *                                                           *
      *    *          w-lvl-prz-vlt = Valuta per il legame             *
      *    *                                                           *
      *    *          w-lvl-prz-tdc = Tipo di cambio valuta            *
      *    *                                                           *
      *    *          w-lvl-prz-cdc = Coefficiente di cambio attuale   *
      *    *                                                           *
      *    *          w-lvl-prz-ccr = Coefficiente di cambio di rife-  *
      *    *                          rimento                          *
      *    *                                                           *
      *    *          w-lvl-prz-plm = Percentuale di limitazione       *
      *    *                                                           *
      *    *          w-lvl-prz-tlm = Tipo di limitazione              *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-lvl-prz-prz = Prezzo determinato               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

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
      *    * Calcolo tabella scadenze                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dtblscd0.dts"                   .

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione area di w-rig relativa al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      spaces               to    w-dec-tip-rig-tpr     .
           move      spaces               to    w-dec-tip-rig-tfu     .
           move      zero                 to    w-dec-tip-rig-cac     .
           move      spaces               to    w-dec-tip-rig-ast     .
           move      zero                 to    w-dec-tip-rig-tot     .
      *              *-------------------------------------------------*
      *              * Presenza o assenza dell'asterisco               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ispezione stringa                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-inx      .
           inspect   w-dec-tip-rig-str
                                      tallying w-dec-tip-rig-inx
                                          for  all "*"                .
      *                  *---------------------------------------------*
      *                  * Se nessun asterisco trovato : oltre         *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-inx    =    zero
                     go to dec-tip-rig-100.
      *                  *---------------------------------------------*
      *                  * Segnale di presenza asterisco               *
      *                  *---------------------------------------------*
           move      "#"                  to   w-dec-tip-rig-ast      .
      *                  *---------------------------------------------*
      *                  * Pulizia stringa da asterisco                *
      *                  *---------------------------------------------*
           inspect   w-dec-tip-rig-str
                                     replacing all "*" by   spaces    .
       dec-tip-rig-100.
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo riga                        *
      *              *-------------------------------------------------*
           move      w-dec-tip-rig-chr (1)
                                          to   w-dec-tip-rig-tpr      .
      *              *-------------------------------------------------*
      *              * Test se esiste un tipo funzionamento            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo per tipi riga P o L                    *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-tpr    not  = "P" and
                     w-dec-tip-rig-tpr    not  = "L"
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Se secondo carattere della stringa a spazi :*
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-chr (2)
                                          =    spaces
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Memorizzazione tipo funzionamento           *
      *                  *---------------------------------------------*
           move      w-dec-tip-rig-chr (2)
                                          to   w-dec-tip-rig-tfu      .
       dec-tip-rig-200.
      *              *-------------------------------------------------*
      *              * Ricerca di un codice addebito o commento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se tipo riga A o C                     *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-tpr    not  = "A" and
                     w-dec-tip-rig-tpr    not  = "C"
                     go to dec-tip-rig-300.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione stringa a ritroso        *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-cod      .
           move      6                    to   w-dec-tip-rig-c01      .
           move      3                    to   w-dec-tip-rig-c02      .
       dec-tip-rig-210.
           subtract  1                    from w-dec-tip-rig-c01      .
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    w-dec-tip-rig-tpr    
                     go to dec-tip-rig-220.
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    spaces or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   <    "0"    or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   >    "9"
                     go to dec-tip-rig-210.
           move      w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   to   w-dec-tip-rig-num
                                              (w-dec-tip-rig-c02)     .
           subtract  1                    from w-dec-tip-rig-c02      .
           go to     dec-tip-rig-210.
       dec-tip-rig-220.
      *                  *---------------------------------------------*
      *                  * Codice individuato                          *
      *                  *---------------------------------------------*
           move      w-dec-tip-rig-cod    to   w-dec-tip-rig-cac      .
      *                  *---------------------------------------------*
      *                  * Lettura per determinare il tipo totale :    *
      *                  * solo per addebiti con codice diverso da     *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-tpr    not  = "A"
                     go to dec-tip-rig-300.
           if        w-dec-tip-rig-cac    =    zero
                     go to dec-tip-rig-300.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zac-tip      .
           move      w-dec-tip-rig-cac    to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
           move      w-let-arc-zac-tot    to   w-dec-tip-rig-tot      .
       dec-tip-rig-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-tip-rig-999.
       dec-tip-rig-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      rf-fir-qta-ven       to   d-imp-ven-qta-ven      .
           move      rf-fir-snx-2qt       to   d-imp-ven-snx-2qt      .
           move      rf-fir-qta-a02       to   d-imp-ven-qta-a02      .
           move      rf-fir-snx-3qt       to   d-imp-ven-snx-3qt      .
           move      rf-fir-qta-a03       to   d-imp-ven-qta-a03      .
           move      rf-fir-prz-net       to   d-imp-ven-prz-uni      .
           move      rf-fir-prz-ven       to   d-imp-ven-prz-unl      .
           move      rf-fir-per-scr (1)   to   d-imp-ven-per-scr (1)  .
           move      rf-fir-per-scr (2)   to   d-imp-ven-per-scr (2)  .
           move      rf-fir-per-scr (3)   to   d-imp-ven-per-scr (3)  .
           move      rf-fir-per-scr (4)   to   d-imp-ven-per-scr (4)  .
           move      rf-fir-per-scr (5)   to   d-imp-ven-per-scr (5)  .
           move      rf-fir-dec-prz       to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to rf-fir-imp-rig           .
       det-imp-rig-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [fit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-fit-000.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      rr-dpz-emi           to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pfat3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-fit-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/fat/prg/obj/pfat3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-fit-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bit-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to  fnd-arc-bit-999.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bol/prg/obj/pbol3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       fnd-arc-bit-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Open                    *
      *    *-----------------------------------------------------------*
       fil-rlt-opn-000.
      *              *-------------------------------------------------*
      *              * Preparazione area ausiliaria per controlli i-o  *
      *              * su [rlt]                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * File name                                   *
      *                  *---------------------------------------------*
           move      "rlt "               to   f-rlt-nam              .
      *                  *---------------------------------------------*
      *                  * File pathname                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prelevamento di un pathname unico per   *
      *                      * files temporanei                        *
      *                      *-----------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Salvataggio pathname determinato        *
      *                      *-----------------------------------------*
           move      s-pat                to   f-rlt-pat              .
      *                  *---------------------------------------------*
      *                  * File status                                 *
      *                  *---------------------------------------------*
           move      "00"                 to   f-rlt-sts              .
       fil-rlt-opn-100.
      *              *-------------------------------------------------*
      *              * Preparazione seconda area ausiliaria per con-   *
      *              * trolli i-o su [rlt]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record number interessato dall'operazione   *
      *                  * di i-o                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-rlt-krn              .
      *                  *---------------------------------------------*
      *                  * Max record number memorizzato in assoluto   *
      *                  *---------------------------------------------*
           move      zero                 to   w-rlt-max              .
      *                  *---------------------------------------------*
      *                  * Contatore records per singolo documento     *
      *                  *---------------------------------------------*
           move      zero                 to   w-rlt-ctr              .
      *                  *---------------------------------------------*
      *                  * Indice per scansione records per singolo    *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-rlt-inx              .
       fil-rlt-opn-200.
      *              *-------------------------------------------------*
      *              * Open output [rlt]                               *
      *              *-------------------------------------------------*
           open      output rlt                                       .
      *              *-------------------------------------------------*
      *              * Close [rlt]                                     *
      *              *-------------------------------------------------*
           close     rlt                                              .
      *              *-------------------------------------------------*
      *              * Open i-o [rlt]                                  *
      *              *-------------------------------------------------*
           open      i-o    rlt                                       .
       fil-rlt-opn-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-rlt-opn-999.
       fil-rlt-opn-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Close                   *
      *    *-----------------------------------------------------------*
       fil-rlt-cls-000.
      *              *-------------------------------------------------*
      *              * Se file non aperto : ad uscita                  *
      *              *-------------------------------------------------*
           if        f-rlt-pat            =    spaces
                     go to fil-rlt-cls-900.
       fil-rlt-cls-100.
      *              *-------------------------------------------------*
      *              * Close [rlt]                                     *
      *              *-------------------------------------------------*
           close     rlt                                              .
       fil-rlt-cls-200.
      *              *-------------------------------------------------*
      *              * Delete file [rlt]                               *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-rlt-pat            to   s-pat                  .
           move      "R"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fil-rlt-cls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-rlt-cls-999.
       fil-rlt-cls-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Put                     *
      *    *-----------------------------------------------------------*
       fil-rlt-put-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del record num- *
      *              * ber                                             *
      *              *-------------------------------------------------*
           if        w-rlt-krn            not   > w-rlt-max
                     go to fil-rlt-put-300.
       fil-rlt-put-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento del massimo record number finora  *
      *              * scritto                                         *
      *              *-------------------------------------------------*
           move      w-rlt-krn            to   w-rlt-max              .
       fil-rlt-put-200.
      *              *-------------------------------------------------*
      *              * Se il record number da scrivere e' maggiore del *
      *              * massimo finora scritto                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record, se non valida : a ri-     *
      *                  * scrittura                                   *
      *                  *---------------------------------------------*
           write     rlt-rec invalid key
                             go to   fil-rlt-put-300.
           go to     fil-rlt-put-900.
       fil-rlt-put-300.
      *              *-------------------------------------------------*
      *              * Se il record number da scrivere non e' maggiore *
      *              * del massimo finora scritto                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riscrittura record, se non valida : a       *
      *                  * scrittura                                   *
      *                  *---------------------------------------------*
           rewrite   rlt-rec invalid key
                             go to   fil-rlt-put-200.
           go to     fil-rlt-put-900.
       fil-rlt-put-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fil-rlt-put-999.
       fil-rlt-put-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Get                     *
      *    *-----------------------------------------------------------*
       fil-rlt-get-000.
           read      rlt   with no lock
                           invalid key
                           go to   fil-rlt-get-999.
       fil-rlt-get-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : New                     *
      *    *-----------------------------------------------------------*
       fil-rlt-new-000.
      *              *-------------------------------------------------*
      *              * Contatore records per singolo documento a zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-ctr              .
       fil-rlt-new-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Write                   *
      *    *-----------------------------------------------------------*
       fil-rlt-wrt-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore records per singolo docu-  *
      *              * mento                                           *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-ctr              .
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-rlt-ctr            to   w-rlt-krn              .
      *              *-------------------------------------------------*
      *              * Scrittura effettiva                             *
      *              *-------------------------------------------------*
           perform   fil-rlt-put-000      thru fil-rlt-put-999        .
       fil-rlt-wrt-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Update                  *
      *    *-----------------------------------------------------------*
       fil-rlt-upd-000.
      *              *-------------------------------------------------*
      *              * Riscrittura effettiva                           *
      *              *-------------------------------------------------*
           perform   fil-rlt-put-000      thru fil-rlt-put-999        .
       fil-rlt-upd-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Start                   *
      *    *-----------------------------------------------------------*
       fil-rlt-str-000.
      *              *-------------------------------------------------*
      *              * Indice per scansione records per singolo docu-  *
      *              * mento : a zero                                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-inx              .
      *              *-------------------------------------------------*
      *              * Preparazione status di uscita                   *
      *              *-------------------------------------------------*
           if        w-rlt-inx            <    w-rlt-ctr
                     move  e-not-err      to   w-rlt-sts
           else      move  e-end-fil      to   w-rlt-sts              .
       fil-rlt-str-999.
           exit.

      *    *===========================================================*
      *    * File relative di appoggio [rlt] : Read next               *
      *    *-----------------------------------------------------------*
       fil-rlt-nxt-000.
      *              *-------------------------------------------------*
      *              * Se fine file : uscita con errore                *
      *              *-------------------------------------------------*
           if        w-rlt-inx            not < w-rlt-ctr
                     move  e-end-fil      to   w-rlt-sts
                     go to fil-rlt-nxt-999.
      *              *-------------------------------------------------*
      *              * Incremento indice per scansione                 *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-inx              .
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-rlt-inx            to   w-rlt-krn              .
      *              *-------------------------------------------------*
      *              * Lettura effettiva                               *
      *              *-------------------------------------------------*
           perform   fil-rlt-get-000      thru fil-rlt-get-999        .
      *              *-------------------------------------------------*
      *              * Status di uscita ad Ok                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   w-rlt-sts              .
       fil-rlt-nxt-999.
           exit.

      *    *===========================================================*
      *    * Editing di una quantita' da incolonnare                   *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per modulo aggiornamento contabilita' genera- *
      *    * le, clienti, fornitori, iva                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice dipendenza dell'a-    *
      *    * zienda                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/acoddpz0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per la fat- *
      *    * turazione                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/acdezfi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione imposta Iva in base ad un  *
      *    * imponibile                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione provvigioni                *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

