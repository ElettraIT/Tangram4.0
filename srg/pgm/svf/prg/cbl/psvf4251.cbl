       Identification Division.
       Program-Id.                                 psvf4251           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:    cgc                 *
      *                                   Fase:    svf425              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/02/94    *
      *                       Ultima revisione:    NdK del 26/01/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione per il programma psvf4251:       *
      *                                                                *
      *                    Stampa statistica di vendita sul fatturato  *
      *                    per classe geografica cliente - cliente -   *
      *                    prodotto.                                   *
      *                                                                *
      *                    Emissione sequenziale (chiamato da svf900)  *
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
      *    * File Control [sqz]                                        *
      *    *-----------------------------------------------------------*
           select  sqz       assign to input-output         f-sqz-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-sqz-sts .

      *    *===========================================================*
      *    * File Control [kk0]                                        *
      *    *-----------------------------------------------------------*
           select  optional  kk0   assign to disk           f-kk0-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is kk0-k01
                             file status  is                f-kk0-sts .

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [sqz]                                    *
      *    *-----------------------------------------------------------*
       fd  sqz.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  sqz-rec.
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  sqz-chr occurs       2048  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [kk0]                                    *
      *    *-----------------------------------------------------------*
       fd  kk0.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  kk0-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  kk0-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01                                  *
      *            *---------------------------------------------------*
               10  kk0-k01.
      *                *-----------------------------------------------*
      *                * Tipo record                                   *
      *                *   01 : Totale per classe geografica clienti   *
      *                *   02 : Totale per cliente                     *
      *                *   03 : Totale per prodotto                    *
      *                *-----------------------------------------------*
                   15  kk0-tip-rec        pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice classe geografica clienti              *
      *                *-----------------------------------------------*
                   15  kk0-cod-cgc        pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente                                *
      *                *-----------------------------------------------*
                   15  kk0-cod-cli        pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza per il cliente              *
      *                *-----------------------------------------------*
                   15  kk0-dpz-cli        pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico per il prodotto               *
      *                *-----------------------------------------------*
                   15  kk0-num-pro        pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  kk0-dat.
      *            *---------------------------------------------------*
      *            * Quantita' fatturata 1. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-qta-1oP            pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore fatturato    1. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-fat-1op            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Quantita' fatturata 2. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-qta-2op            pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore fatturato    2. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-fat-2op            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Quantita' fatturata 3. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-qta-3op            pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore fatturato    3. periodo                    *
      *            *---------------------------------------------------*
               10  kk0-fat-3op            pic s9(13)                  .

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
      *            * Subchiave 1 : Per classe geografica cliente       *
      *            *---------------------------------------------------*
               10  srt-k01.
      *                *-----------------------------------------------*
      *                * Valore ABC                                    *
      *                *                                               *
      *                * Solo per tipo ordinamento classe geografica : *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                *                                               *
      *                * In caso di ABC decrescente questo valore sa-  *
      *                * ra' complementato                             *
      *                *-----------------------------------------------*
                   15  srt-k01-val-abc    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Flag di codice diverso da spaces ma non tro-  *
      *                * vato in archivio codici geografici            *
      *                *                                               *
      *                *   - 1 : Esistente                             *
      *                *   - 5 : Non esistente                         *
      *                *   - 9 : Codice a spaces                       *
      *                *                                               *
      *                * Solo per tipo ordinamento classe geografica : *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                * - Per descrizione                             *
      *                *-----------------------------------------------*
                   15  srt-k01-flg-eon    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione per classe geografica             *
      *                *                                               *
      *                * Solo per tipo ordinamento classe geografica : *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                * - Per descrizione                             *
      *                *-----------------------------------------------*
                   15  srt-k01-des-cgc    pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * Flag di codice a spaces o diverso da spaces   *
      *                *                                               *
      *                *   - 1 : Codice diverso da spaces              *
      *                *   - 9 : Codice a spaces                       *
      *                *                                               *
      *                * Solo per tipo ordinamento classe geografica : *
      *                *                                               *
      *                * - Per codice                                  *
      *                *-----------------------------------------------*
                   15  srt-k01-flg-sod    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Codice classe geografica                      *
      *                *                                               *
      *                * Per tutti i tipi ordinamento per la classe    *
      *                * geografica                                    *
      *                *-----------------------------------------------*
                   15  srt-k01-cod-cgc    pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Subchiave 2 : Per cliente                         *
      *            *---------------------------------------------------*
               10  srt-k02.
      *                *-----------------------------------------------*
      *                * Valore ABC                                    *
      *                *                                               *
      *                * Solo per tipo ordinamento cliente :           *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                *                                               *
      *                * In caso di ABC decrescente questo valore sa-  *
      *                * ra' complementato                             *
      *                *-----------------------------------------------*
                   15  srt-k02-val-abc    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Flag di codice diverso da zero ma non trovato *
      *                * in archivio clienti                           *
      *                *                                               *
      *                *   - 1 : Esistente                             *
      *                *   - 5 : Non esistente                         *
      *                *   - 9 : Codice a zero                         *
      *                *                                               *
      *                * Solo per tipo ordinamento cliente :           *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                * - Per ragione sociale cliente                 *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k02-flg-eon    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale cliente                       *
      *                *                                               *
      *                * Solo per tipo ordinamento cliente :           *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                * - Per ragione sociale cliente                 *
      *                *-----------------------------------------------*
                   15  srt-k02-rag-cli    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Mnemonico per cliente                         *
      *                *                                               *
      *                * Solo per tipo ordinamento cliente :           *
      *                *                                               *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k02-mne-cli    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Flag di codice a zero o diverso da zero       *
      *                *                                               *
      *                *   - 1 : Codice diverso da zero                *
      *                *   - 9 : Codice a zero                         *
      *                *                                               *
      *                * Solo per tipo ordinamento cliente :           *
      *                *                                               *
      *                * - Per codice                                  *
      *                *-----------------------------------------------*
                   15  srt-k02-flg-zod    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente                                *
      *                *                                               *
      *                * Per tutti i tipi ordinamento cliente          *
      *                *-----------------------------------------------*
                   15  srt-k02-cod-cli    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza del cliente                 *
      *                *                                               *
      *                * Per tutti i tipi ordinamento cliente          *
      *                *                                               *
      *                * Solamente se le personalizzazioni indicano    *
      *                * una stampa differenziata per dipendenza del   *
      *                * cliente                                       *
      *                *-----------------------------------------------*
                   15  srt-k02-dpz-cli    pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Subchiave 3 : Per prodotto                        *
      *            *---------------------------------------------------*
               10  srt-k03.
      *                *-----------------------------------------------*
      *                * Valore ABC                                    *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                *                                               *
      *                * In caso di ABC decrescente questo valore sa-  *
      *                * ra' complementato                             *
      *                *-----------------------------------------------*
                   15  srt-k03-val-abc    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Flag di codice numerico diverso da zero ma    *
      *                * non trovato in archivio prodotti              *
      *                *                                               *
      *                *   - 1 : Esistente                             *
      *                *   - 5 : Non esistente                         *
      *                *   - 9 : Codice a zero                         *
      *                *                                               *
      *                * Per tutti i tipi ordinamento prodotto         *
      *                *-----------------------------------------------*
                   15  srt-k03-flg-eon    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Numero di sequenza della classe del prodotto  *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-sqz-cla    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico della classe del prodotto     *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-cod-cla    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Numero di sequenza del gruppo del prodotto    *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-sqz-gru    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico del gruppo del prodotto       *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-cod-gru    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Numero di sequenza del sottogruppo del pro-   *
      *                * dotto                                         *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-sqz-sgr    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico del sottogruppo del prodotto  *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per classe e codice prodotto                *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-cod-sgr    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Descrizione per il prodotto                   *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - ABC decrescente sul fatturato               *
      *                * - ABC decrescente sulla quantita'             *
      *                * - ABC crescente sul fatturato                 *
      *                * - ABC crescente sulla quantita'               *
      *                * - Per descrizione prodotto                    *
      *                * - Per classe e descrizione prodotto           *
      *                *-----------------------------------------------*
                   15  srt-k03-des-pro    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Flag di codice alfanumerico a spaces o diver- *
      *                * so da spaces                                  *
      *                *                                               *
      *                *   - 1 : Codice diverso da spaces              *
      *                *   - 9 : Codice a spaces                       *
      *                *                                               *
      *                * Solo per tipo ordinamento prodotto :          *
      *                *                                               *
      *                * - Per codice prodotto                         *
      *                * - Per classe e codice prodotto                *
      *                *-----------------------------------------------*
                   15  srt-k03-flg-sod    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Codice alfanumerico prodotto                  *
      *                *                                               *
      *                * Per tutti i tipi ordinamento prodotto         *
      *                *-----------------------------------------------*
                   15  srt-k03-alf-pro    pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Codice numerico prodotto                      *
      *                *                                               *
      *                * Per tutti i tipi ordinamento prodotto         *
      *                *-----------------------------------------------*
                   15  srt-k03-num-pro    pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Subchiave 4 : Per documento                       *
      *            *                                                   *
      *            * Per tutti i tipi di dettaglio stampa              *
      *            *---------------------------------------------------*
               10  srt-k04.
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  srt-k04-dat-doc    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Tipo documento                                *
      *                *-----------------------------------------------*
                   15  srt-k04-tip-doc    pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Numero documento, nel formato s.aa.xx.nnnnnn  *
      *                *-----------------------------------------------*
                   15  srt-k04-num-doc    pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Codice classe geografica cliente                  *
      *            *---------------------------------------------------*
               10  srt-cod-cgc.
      *                *-----------------------------------------------*
      *                * Codice classe geografica cliente, codice      *
      *                *-----------------------------------------------*
                   15  srt-cod-cgc-cod    pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Codice classe geografica cliente, descrizione *
      *                *-----------------------------------------------*
                   15  srt-cod-cgc-des    pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * Codice classe geografica cliente, flag di e-  *
      *                * sistenza                                      *
      *                *-----------------------------------------------*
                   15  srt-cod-cgc-flg    pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  srt-cod-cli.
      *                *-----------------------------------------------*
      *                * Codice cliente, codice                        *
      *                *-----------------------------------------------*
                   15  srt-cod-cli-cod    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, codice sua dipendenza         *
      *                *-----------------------------------------------*
                   15  srt-cod-cli-dpz    pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, mnemonico                     *
      *                *-----------------------------------------------*
                   15  srt-cod-cli-mne    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, ragione sociale               *
      *                *-----------------------------------------------*
                   15  srt-cod-cli-rag    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice classe merceologica                        *
      *            *---------------------------------------------------*
               10  srt-cod-cla.
      *                *-----------------------------------------------*
      *                * Codice classe merceologica, codice            *
      *                *-----------------------------------------------*
                   15  srt-cod-cla-cod    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice classe merceologica, mnemonico         *
      *                *-----------------------------------------------*
                   15  srt-cod-cla-mne    pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Codice classe merceologica, numero di sequen- *
      *                * za                                            *
      *                *-----------------------------------------------*
                   15  srt-cod-cla-sqz    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice classe merceologica, si/no ulteriore   *
      *                * suddivisione                                  *
      *                *-----------------------------------------------*
                   15  srt-cod-cla-sud    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice classe merceologica, descrizione       *
      *                *-----------------------------------------------*
                   15  srt-cod-cla-des    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice gruppo merceologico                        *
      *            *---------------------------------------------------*
               10  srt-cod-gru.
      *                *-----------------------------------------------*
      *                * Codice gruppo merceologico, codice            *
      *                *-----------------------------------------------*
                   15  srt-cod-gru-cod    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice gruppo merceologico, mnemonico         *
      *                *-----------------------------------------------*
                   15  srt-cod-gru-mne    pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Codice gruppo merceologico, numero di sequen- *
      *                * za                                            *
      *                *-----------------------------------------------*
                   15  srt-cod-gru-sqz    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice gruppo merceologico, si/no ulteriore   *
      *                * suddivisione                                  *
      *                *-----------------------------------------------*
                   15  srt-cod-gru-sud    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice gruppo merceologico, descrizione       *
      *                *-----------------------------------------------*
                   15  srt-cod-gru-des    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice sottogruppo merceologico                   *
      *            *---------------------------------------------------*
               10  srt-cod-sgr.
      *                *-----------------------------------------------*
      *                * Codice sottogruppo merceologico, codice       *
      *                *-----------------------------------------------*
                   15  srt-cod-sgr-cod    pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice sottogruppo merceologico, mnemonico    *
      *                *-----------------------------------------------*
                   15  srt-cod-sgr-mne    pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Codice sottogruppo merceologico, numero di    *
      *                * sequenza                                      *
      *                *-----------------------------------------------*
                   15  srt-cod-sgr-sqz    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice sottogruppo merceologico, descrizione  *
      *                *-----------------------------------------------*
                   15  srt-cod-sgr-des    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice prodotto                                   *
      *            *---------------------------------------------------*
               10  srt-cod-pro.
      *                *-----------------------------------------------*
      *                * Codice prodotto, codice numerico              *
      *                *-----------------------------------------------*
                   15  srt-cod-pro-cod    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice prodotto, codice alfanumerico          *
      *                *-----------------------------------------------*
                   15  srt-cod-pro-alf    pic  x(14)                  .
      *                *-----------------------------------------------*
      *                * Codice prodotto, descrizione                  *
      *                *-----------------------------------------------*
                   15  srt-cod-pro-des    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice prodotto, unita' di misura di vendita  *
      *                *-----------------------------------------------*
                   15  srt-cod-pro-umi    pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Codice prodotto, numero decimali quantita'    *
      *                *-----------------------------------------------*
                   15  srt-cod-pro-dec    pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Estremi del documento                             *
      *            *---------------------------------------------------*
               10  srt-est-doc.
      *                *-----------------------------------------------*
      *                * Estremi del documento, tipo documento         *
      *                *-----------------------------------------------*
                   15  srt-est-doc-tdo    pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Estremi del documento, data documento         *
      *                *-----------------------------------------------*
                   15  srt-est-doc-ddo    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Estremi del documento, numero documento, nel  *
      *                *                        formato s.aa.xx.nnnnnn *
      *                *-----------------------------------------------*
                   15  srt-est-doc-ndo    pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Campo da esporre come 'Note' in riga di det-  *
      *                * taglio documento                              *
      *                *-----------------------------------------------*
                   15  srt-est-doc-not    pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Valori statistici                                 *
      *            *---------------------------------------------------*
               10  srt-val-sta.
      *                *-----------------------------------------------*
      *                * Quantita' fatturata                           *
      *                *-----------------------------------------------*
                   15  srt-val-sta-qta    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Importo fatturato                             *
      *                *-----------------------------------------------*
                   15  srt-val-sta-val    pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Incidenza sulla statistica                    *
      *                *   - P : In positivo                           *
      *                *   - N : In negativo                           *
      *                *-----------------------------------------------*
                   15  srt-val-sta-pon    pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Incidenza sul 1. periodo di riferimento       *
      *                *   - S : Si'                                   *
      *                *   - N : No                                    *
      *                *-----------------------------------------------*
                   15  srt-val-sta-sn1    pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Incidenza sul 2. periodo di riferimento       *
      *                *   - S : Si'                                   *
      *                *   - N : No                                    *
      *                *-----------------------------------------------*
                   15  srt-val-sta-sn2    pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Incidenza sul 3. periodo di riferimento       *
      *                *   - S : Si'                                   *
      *                *   - N : No                                    *
      *                *-----------------------------------------------*
                   15  srt-val-sta-sn3    pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valori ABC                                        *
      *            *---------------------------------------------------*
               10  srt-val-abc.
      *                *-----------------------------------------------*
      *                * Valore ABC per la classe geografica           *
      *                *-----------------------------------------------*
                   15  srt-val-abc-cgc    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Valore ABC per il cliente                     *
      *                *-----------------------------------------------*
                   15  srt-val-abc-cli    pic s9(13)v9(03)            .
      *                *-----------------------------------------------*
      *                * Valore ABC per il prodotto                    *
      *                *-----------------------------------------------*
                   15  srt-val-abc-pro    pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valori per selezione record                       *
      *            *---------------------------------------------------*
               10  srt-sel-rec.
      *                *-----------------------------------------------*
      *                * Flag per record da non selezionare            *
      *                *-----------------------------------------------*
                   15  srt-sel-rec-non    pic  x(01)                  .

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
                     "svf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "cgc"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "svf425"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "psvf4251"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "FATT. CLASSE GEOGRAFICA-CLIENTE-PRODOTTO"       .

      *    *===========================================================*
      *    * Area ausiliaria per controlli i-o su [sqz]                *
      *    *-----------------------------------------------------------*
       01  f-sqz.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-sqz-nam                  pic  x(04) value "sqz "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-sqz-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-sqz-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area ausiliaria per controlli i-o su [kk0]                *
      *    *-----------------------------------------------------------*
       01  f-kk0.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-kk0-nam                  pic  x(04) value "kk0 "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-kk0-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-kk0-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mxport" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/x"                                  .

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
      *            * Per routine let-rec-ric-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-rec-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No record richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No stampa                                      *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l07  pic  x(64)                  .
                   15  w-cnt-prn-sav-l06  pic  x(64)                  .
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 448  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records logici                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .
      *        *-------------------------------------------------------*
      *        * [gxr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxr"                          .
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .
      *        *-------------------------------------------------------*
      *        * [gxc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxc"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [zp1]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp1"                          .
      *        *-------------------------------------------------------*
      *        * [zp2]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp2"                          .
      *        *-------------------------------------------------------*
      *        * [zp3]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzp3"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Denominazione dipendenza in uso                       *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu-den             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Numero globale dipendenze                             *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-dpz             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero dipendenze selezionate                         *
      *        *-------------------------------------------------------*
           05  rr-dpz-ctr-sel             pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dipendenze selezionate                        *
      *        *-------------------------------------------------------*
           05  rr-dpz-tbl-dpz.
      *            *---------------------------------------------------*
      *            * Elementi della tabella dipendenze selezionate     *
      *            *---------------------------------------------------*
               10  rr-dpz-ele-dpz occurs 99.
      *                *-----------------------------------------------*
      *                * Codice dipendenza selezionato                 *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-cod     pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Denominazione dipendenza                      *
      *                *-----------------------------------------------*
                   15  rr-dpz-ele-den     pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Tipo classe geografica cliente selezionata            *
      *        *  - 01 : Nazione                                       *
      *        *  - 02 : Regione                                       *
      *        *  - 03 : Provincia                                     *
      *        *-------------------------------------------------------*
           05  rr-tcg-cli                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice classe geografica cliente selezionata          *
      *        *  - Se codice '   ' : Tutti i codici                   *
      *        *-------------------------------------------------------*
           05  rr-ccg-cli                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice classe geografica cliente selezionata, descri- *
      *        * zione                                                 *
      *        *  - Se codice '   ' : 'Tutti i codici      '           *
      *        *-------------------------------------------------------*
           05  rr-ccg-cli-des             pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per la classe geografica cliente  *
      *        *  - 01 : Ordinamento ABC decrescente sul fatturato     *
      *        *  - 02 : Ordinamento ABC decrescente sulla quantita'   *
      *        *  - 03 : Ordinamento ABC crescente sul fatturato       *
      *        *  - 04 : Ordinamento ABC crescente sulla quantita'     *
      *        *  - 05 : Ordinamento per descrizione classe            *
      *        *  - 06 : Ordinamento per codice classe                 *
      *        *                                                       *
      *        * Non significativo se codice classe geografica cliente *
      *        * diversa da spaces, ovvero un solo codice              *
      *        *-------------------------------------------------------*
           05  rr-tor-cgc                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per clienti                       *
      *        *  - 01 : Ordinamento ABC decrescente sul fatturato     *
      *        *  - 02 : Ordinamento ABC decrescente sulla quantita'   *
      *        *  - 03 : Ordinamento ABC crescente sul fatturato       *
      *        *  - 04 : Ordinamento ABC crescente sulla quantita'     *
      *        *  - 05 : Ordinamento per ragione sociale cliente       *
      *        *  - 06 : Ordinamento per codice cliente                *
      *        *  - 07 : Ordinamento per mnemonico cliente             *
      *        *-------------------------------------------------------*
           05  rr-tor-cli                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice filtro di selezione anagrafica prodotti, uti-  *
      *        * lizzato solo come selezione, non come ordinamento     *
      *        *-------------------------------------------------------*
           05  rr-fso-dcp                 pic  9(08)                  .
           05  rr-fso-dcp-alf redefines
               rr-fso-dcp                 pic  x(08)                  .
           05  rr-fso-dcp-des             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per prodotti                      *
      *        *  - 01 : Ordinamento ABC decrescente sul fatturato     *
      *        *  - 02 : Ordinamento ABC decrescente sulla quantita'   *
      *        *  - 03 : Ordinamento ABC crescente sul fatturato       *
      *        *  - 04 : Ordinamento ABC crescente sulla quantita'     *
      *        *  - 05 : Ordinamento per codice prodotto               *
      *        *  - 06 : Ordinamento per descrizione prodotto          *
      *        *  - 07 : Ordinamento per classe e codice prodotto      *
      *        *  - 08 : Ordinamento per classe e descrizione prodotto *
      *        *  - 09 : Ordinamento per classe (senza dettaglio pro-  *
      *        *         dotto)                                        *
      *        *-------------------------------------------------------*
           05  rr-tor-pro                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero periodi di riferimento                         *
      *        *                                                       *
      *        *  - 01..03 : Numero effettivo di periodi di riferimen- *
      *        *             to                                        *
      *        *                                                       *
      *        *  - 11     : Due periodi di riferimento, con la stampa *
      *        *             del solo 2. periodo di cio' che non e'    *
      *        *             stato movimentato nel 1. periodo          *
      *        *                                                       *
      *        *  - 12     : Due periodi di riferimento, con la stampa *
      *        *             del solo 1. periodo di cio' che non e'    *
      *        *             stato movimentato nel 2. periodo          *
      *        *                                                       *
      *        * Forzato a 01 se tipo dettaglio stampa 02, ovvero con  *
      *        * la lista documenti emessi per ogni prodotto           *
      *        *-------------------------------------------------------*
           05  rr-num-pdr                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo di riferimento, data minima                *
      *        *-------------------------------------------------------*
           05  rr-p1d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 1. periodo di riferimento, data massima               *
      *        *-------------------------------------------------------*
           05  rr-p1d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo di riferimento, data minima                *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 11 op- *
      *        * pure 12                                               *
      *        *-------------------------------------------------------*
           05  rr-p2d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 2. periodo di riferimento, data massima               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 11 op- *
      *        * pure 12                                               *
      *        *-------------------------------------------------------*
           05  rr-p2d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo di riferimento, data minima                *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 03, e nemmeno se pari a 11 oppure 12     *
      *        *-------------------------------------------------------*
           05  rr-p3d-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * 3. periodo di riferimento, data massima               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 03, e nemmeno se pari a 11 oppure 12     *
      *        *-------------------------------------------------------*
           05  rr-p3d-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo calcolo % di variazione                          *
      *        *  - 01 : Sul fatturato                                 *
      *        *  - 02 : Sulla quantita'                               *
      *        *                                                       *
      *        * Non significativo se il numero periodi di riferimento *
      *        * e' minore di 02, pero' significativo se pari a 12     *
      *        *-------------------------------------------------------*
           05  rr-tpc-pdv                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Si/no generazione file                                *
      *        *-------------------------------------------------------*
           05  rr-gen-fil                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxn]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxn.
               10  w-let-arc-gxn-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxn-cod      pic  x(03)     value spaces .
               10  w-let-arc-gxn-des      pic  x(20)     value spaces .
               10  w-let-arc-gxn-exc      pic  x(03)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxr]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxr.
               10  w-let-arc-gxr-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxr-cod      pic  x(03)     value spaces .
               10  w-let-arc-gxr-des      pic  x(25)     value spaces .
               10  w-let-arc-gxr-exc      pic  x(03)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxp.
               10  w-let-arc-gxp-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxp-cod      pic  x(03)     value spaces .
               10  w-let-arc-gxp-des      pic  x(25)     value spaces .
               10  w-let-arc-gxp-rgn      pic  x(03)     value spaces .
               10  w-let-arc-gxp-exc      pic  x(03)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxc.
               10  w-let-arc-gxc-flg      pic  x(01)     value spaces .
               10  w-let-arc-gxc-cod      pic  9(05)     value zero   .
               10  w-let-arc-gxc-des      pic  x(30)     value spaces .
               10  w-let-arc-gxc-prv      pic  x(03)     value spaces .
               10  w-let-arc-gxc-exc      pic  9(05)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli] e [dcc]                *
      *        *-------------------------------------------------------*
           05  w-let-cli-dcc.
               10  w-let-cli-dcc-cli      pic  9(07)     value zero   .
               10  w-let-cli-dcc-dpz      pic  x(04)     value spaces .
               10  w-let-cli-dcc-rag      pic  x(40)     value spaces .
               10  w-let-cli-dcc-mne      pic  x(10)     value spaces .
               10  w-let-cli-dcc-naz      pic  x(03)     value spaces .
               10  w-let-cli-dcc-cmn      pic  9(05)     value zero   .
               10  w-let-cli-dcc-rgn      pic  x(03)     value spaces .
               10  w-let-cli-dcc-prv      pic  x(03)     value spaces .
               10  w-let-cli-dcc-exc      pic  9(07)     value zero   .
               10  w-let-cli-dcc-exd      pic  x(04)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-cod      pic  9(07)     value zero   .
               10  w-let-arc-dcp-flg      pic  x(01)     value spaces .
               10  w-let-arc-dcp-alf      pic  x(14)     value spaces .
               10  w-let-arc-dcp-des      pic  x(40)     value spaces .
               10  w-let-arc-dcp-umi      pic  x(03)     value spaces .
               10  w-let-arc-dcp-dec      pic  9(01)     value zero   .
               10  w-let-arc-dcp-cla      pic  9(05)     value zero   .
               10  w-let-arc-dcp-gru      pic  9(05)     value zero   .
               10  w-let-arc-dcp-sgr      pic  9(05)     value zero   .
               10  w-let-arc-dcp-exc      pic  9(07)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp1]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp1.
               10  w-let-arc-zp1-flg      pic  x(01)     value spaces .
               10  w-let-arc-zp1-cla      pic  9(05)     value zero   .
               10  w-let-arc-zp1-des      pic  x(40)     value spaces .
               10  w-let-arc-zp1-mne      pic  x(05)     value spaces .
               10  w-let-arc-zp1-sud      pic  9(02)     value zero   .
               10  w-let-arc-zp1-sqz      pic  9(07)     value zero   .
               10  w-let-arc-zp1-exc      pic  9(05)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp2]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp2.
               10  w-let-arc-zp2-flg      pic  x(01)     value spaces .
               10  w-let-arc-zp2-cla      pic  9(05)     value zero   .
               10  w-let-arc-zp2-gru      pic  9(05)     value zero   .
               10  w-let-arc-zp2-des      pic  x(40)     value spaces .
               10  w-let-arc-zp2-mne      pic  x(05)     value spaces .
               10  w-let-arc-zp2-sud      pic  9(02)     value zero   .
               10  w-let-arc-zp2-sqz      pic  9(07)     value zero   .
               10  w-let-arc-zp2-exc      pic  9(05)     value zero   .
               10  w-let-arc-zp2-exg      pic  9(05)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zp3]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zp3.
               10  w-let-arc-zp3-flg      pic  x(01)     value spaces .
               10  w-let-arc-zp3-cla      pic  9(05)     value zero   .
               10  w-let-arc-zp3-gru      pic  9(05)     value zero   .
               10  w-let-arc-zp3-sgr      pic  9(05)     value zero   .
               10  w-let-arc-zp3-des      pic  x(40)     value spaces .
               10  w-let-arc-zp3-mne      pic  x(05)     value spaces .
               10  w-let-arc-zp3-sud      pic  9(02)     value zero   .
               10  w-let-arc-zp3-sqz      pic  9(07)     value zero   .
               10  w-let-arc-zp3-exc      pic  9(05)     value zero   .
               10  w-let-arc-zp3-exg      pic  9(05)     value zero   .
               10  w-let-arc-zp3-exs      pic  9(05)     value zero   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det pathname file sequenziale in output      *
      *        *-------------------------------------------------------*
           05  w-det-pth-fso.
               10  w-det-pth-fso-pat      pic  x(80)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 7. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l07.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 6. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l06.
      *            *---------------------------------------------------*
      *            * Codice classe geografica cliente                  *
      *            *---------------------------------------------------*
               10  w-rot-l06-cod-cgc      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-rot-l05-cod-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza per il cliente                  *
      *            *---------------------------------------------------*
               10  w-rot-l05-dpz-cli      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
      *            *---------------------------------------------------*
      *            * Codice numerico della classe                      *
      *            *---------------------------------------------------*
               10  w-rot-l04-cod-cla      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
      *            *---------------------------------------------------*
      *            * Codice numerico del gruppo                        *
      *            *---------------------------------------------------*
               10  w-rot-l03-cod-gru      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
      *            *---------------------------------------------------*
      *            * Codice numerico del sottogruppo                   *
      *            *---------------------------------------------------*
               10  w-rot-l02-cod-sgr      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
      *            *---------------------------------------------------*
      *            * Codice numerico del prodotto                      *
      *            *---------------------------------------------------*
               10  w-rot-l01-cod-pro      pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per livelli di rottura                          *
      *    *-----------------------------------------------------------*
       01  w-liv.
      *        *-------------------------------------------------------*
      *        * Codice prodotto                                       *
      *        *-------------------------------------------------------*
           05  w-liv-pro.
      *            *---------------------------------------------------*
      *            * Codice numerico del prodotto                      *
      *            *---------------------------------------------------*
               10  w-liv-pro-cod-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico del prodotto                  *
      *            *---------------------------------------------------*
               10  w-liv-pro-cod-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per il prodotto                       *
      *            *---------------------------------------------------*
               10  w-liv-pro-des-pro      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-pro-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-pro-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-pro-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-pro-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-pro-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per il prodotto          *
      *            *---------------------------------------------------*
               10  w-liv-pro-num-ele      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice sottogruppo merceologico                       *
      *        *-------------------------------------------------------*
           05  w-liv-sgr.
      *            *---------------------------------------------------*
      *            * Codice numerico del sottogruppo                   *
      *            *---------------------------------------------------*
               10  w-liv-sgr-cod-num      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice mnemonico del sottogruppo                  *
      *            *---------------------------------------------------*
               10  w-liv-sgr-cod-mne      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di sequenza del sottogruppo                *
      *            *---------------------------------------------------*
               10  w-liv-sgr-num-sqz      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per il sottogruppo                    *
      *            *---------------------------------------------------*
               10  w-liv-sgr-des-sgr      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-sgr-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-sgr-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-sgr-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-sgr-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-sgr-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per il sottogruppo       *
      *            *---------------------------------------------------*
               10  w-liv-sgr-num-ele      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice gruppo merceologico                            *
      *        *-------------------------------------------------------*
           05  w-liv-gru.
      *            *---------------------------------------------------*
      *            * Codice numerico del gruppo                        *
      *            *---------------------------------------------------*
               10  w-liv-gru-cod-num      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice mnemonico del gruppo                       *
      *            *---------------------------------------------------*
               10  w-liv-gru-cod-mne      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di sequenza del gruppo                     *
      *            *---------------------------------------------------*
               10  w-liv-gru-num-sqz      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Segnale di gruppo ulteriormente suddiviso         *
      *            *---------------------------------------------------*
               10  w-liv-gru-ult-sud      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per il gruppo                         *
      *            *---------------------------------------------------*
               10  w-liv-gru-des-gru      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-gru-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-gru-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-gru-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-gru-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gru-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per il gruppo            *
      *            *---------------------------------------------------*
               10  w-liv-gru-num-ele      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Codice classe merceologica                            *
      *        *-------------------------------------------------------*
           05  w-liv-cla.
      *            *---------------------------------------------------*
      *            * Codice numerico della classe                      *
      *            *---------------------------------------------------*
               10  w-liv-cla-cod-num      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice mnemonico della classe                     *
      *            *---------------------------------------------------*
               10  w-liv-cla-cod-mne      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero di sequenza della classe                   *
      *            *---------------------------------------------------*
               10  w-liv-cla-num-sqz      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Segnale di classe ulteriormente suddivisa         *
      *            *---------------------------------------------------*
               10  w-liv-cla-ult-sud      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per la classe                         *
      *            *---------------------------------------------------*
               10  w-liv-cla-des-cla      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-cla-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-cla-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-cla-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-cla-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cla-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per la classe            *
      *            *---------------------------------------------------*
               10  w-liv-cla-num-ele      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Cliente                                               *
      *        *-------------------------------------------------------*
           05  w-liv-cli.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-liv-cli-cod-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del cliente                     *
      *            *---------------------------------------------------*
               10  w-liv-cli-dpz-cli      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale del cliente                       *
      *            *---------------------------------------------------*
               10  w-liv-cli-rag-cli      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Mnemonico del cliente                             *
      *            *---------------------------------------------------*
               10  w-liv-cli-mne-cli      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-cli-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-cli-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-cli-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-cli-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cli-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per il cliente           *
      *            *---------------------------------------------------*
               10  w-liv-cli-num-ele      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Classe geografica cliente                             *
      *        *-------------------------------------------------------*
           05  w-liv-cgc.
      *            *---------------------------------------------------*
      *            * Codice classe geografica                          *
      *            *---------------------------------------------------*
               10  w-liv-cgc-cod-cgc      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per la classe geografica              *
      *            *---------------------------------------------------*
               10  w-liv-cgc-des-cgc      pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Flag di esistenza per la classe geografica        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-flg-cgc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-cgc-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-cgc-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-cgc-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-cgc-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-cgc-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero clienti trattati per la classe geografica  *
      *            *---------------------------------------------------*
               10  w-liv-cgc-num-cli      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Totale generale                                       *
      *        *-------------------------------------------------------*
           05  w-liv-gen.
      *            *---------------------------------------------------*
      *            * Flag di unita' di misura memorizzata              *
      *            *---------------------------------------------------*
               10  w-liv-gen-flg-umi      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore dell' unita' di misura memorizzata         *
      *            *---------------------------------------------------*
               10  w-liv-gen-val-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Flag di numero decimali memorizzati               *
      *            *---------------------------------------------------*
               10  w-liv-gen-flg-dec      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore del numero decimali memorizzati            *
      *            *---------------------------------------------------*
               10  w-liv-gen-val-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-qta-p1r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 1. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-tot-p1r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-qta-p2r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 2. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-tot-p2r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Totale quantita' 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-qta-p3r      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Totale fatturato 3. periodo di riferimento        *
      *            *---------------------------------------------------*
               10  w-liv-gen-tot-p3r      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Numero elementi trattati per il totale generale   *
      *            *---------------------------------------------------*
               10  w-liv-gen-num-ele      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Flag di fine ciclo in esecuzione                  *
      *            *   - Spaces : No                                   *
      *            *   - #      : Si                                   *
      *            *---------------------------------------------------*
               10  w-liv-gen-flg-end      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore linee generate                          *
      *            *---------------------------------------------------*
               10  w-liv-gen-ctr-lin      pic  9(11)                  .

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'mfatfir0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mfatfir0.mdl"                   .

      *    *===========================================================*
      *    * Link-area per modulo dell'area 'svf'           'mnotsvf0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mnotsvf0.mdl"                   .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf/[cod-cli]'            *
      *    *-----------------------------------------------------------*
       01  w-prs-svf-cli.
      *        *-------------------------------------------------------*
      *        * Tipo codice cliente da esporre                        *
      *        *   - C : Il codice numerico                            *
      *        *   - M : Il codice mnemonico                           *
      *        *-------------------------------------------------------*
           05  w-prs-svf-cli-tco          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No raggruppamento di tutte le eventuali dipendenze *
      *        * per lo stesso cliente, ove possibile                  *
      *        *   - S : Si, raggruppare ove possibile                 *
      *        *   - N : No, tenere sempre distinte le dipendenze del  *
      *        *             cliente                                   *
      *        *-------------------------------------------------------*
           05  w-prs-svf-cli-rgd          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf/[slp-rdu]'            *
      *    *-----------------------------------------------------------*
       01  w-prs-svf-spr.
      *        *-------------------------------------------------------*
      *        * Salto pagina a rotture fondamentali                   *
      *        *   - S : Si                                            *
      *        *   - N : No                                            *
      *        *-------------------------------------------------------*
           05  w-prs-svf-spr-slp          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Rinumerazione pagina da 1 a rotture fondamentali      *
      *        *   - S : Si                                            *
      *        *   - N : No                                            *
      *        *-------------------------------------------------------*
           05  w-prs-svf-spr-rdu          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/svf/[snx-tqn]'            *
      *    *-----------------------------------------------------------*
       01  w-prs-svf-tqn.
      *        *-------------------------------------------------------*
      *        * Si/No stampa totali quantita' non omogenee            *
      *        *   - S : Si'                                           *
      *        *   - N : No                                            *
      *        *-------------------------------------------------------*
           05  w-prs-svf-tqn-snx          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per selezione su record [fit] in esame               *
      *    *-----------------------------------------------------------*
       01  w-sel-fit.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *   - Spaces : Selezione superata                       *
      *        *   - #      : Selezione non superata                   *
      *        *-------------------------------------------------------*
           05  w-sel-fit-flg-exi          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo su dipendenze selezionate         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-ctr-dpz          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cod-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza per il cliente                      *
      *        *-------------------------------------------------------*
           05  w-sel-fit-dpz-cli          pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico per il cliente                              *
      *        *-------------------------------------------------------*
           05  w-sel-fit-mne-cli          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Ragione sociale per il cliente                        *
      *        *-------------------------------------------------------*
           05  w-sel-fit-rag-cli          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice nazione per il cliente                         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-naz-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice comune per il cliente                          *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cmn-cli          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice regione per il cliente                         *
      *        *-------------------------------------------------------*
           05  w-sel-fit-rgn-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice provincia per il cliente                       *
      *        *-------------------------------------------------------*
           05  w-sel-fit-prv-cli          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Codice tipo classe geografica cliente                 *
      *        *-------------------------------------------------------*
           05  w-sel-fit-cod-cgc          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione tipo classe geografica cliente            *
      *        *-------------------------------------------------------*
           05  w-sel-fit-des-cgc          pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Flag di tipo classe geografica cliente esistente      *
      *        *-------------------------------------------------------*
           05  w-sel-fit-flg-cgc          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per selezione su record [fir] in esame               *
      *    *-----------------------------------------------------------*
       01  w-sel-fir.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *   - Spaces : Selezione superata                       *
      *        *   - #      : Selezione non superata                   *
      *        *-------------------------------------------------------*
           05  w-sel-fir-flg-exi          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice prodotto numerico                              *
      *        *-------------------------------------------------------*
           05  w-sel-fir-num-pro          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice prodotto alfanumerico                          *
      *        *-------------------------------------------------------*
           05  w-sel-fir-alf-pro          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione prodotto                                  *
      *        *-------------------------------------------------------*
           05  w-sel-fir-des-pro          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Unita' di misura di vendita per il prodotto           *
      *        *-------------------------------------------------------*
           05  w-sel-fir-umi-pro          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero decimali quantita' per il prodotto             *
      *        *-------------------------------------------------------*
           05  w-sel-fir-dec-pro          pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice classe merceologica per il prodotto            *
      *        *-------------------------------------------------------*
           05  w-sel-fir-cod-cla          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza per la classe merceologica per il  *
      *        * prodotto                                              *
      *        *-------------------------------------------------------*
           05  w-sel-fir-sqz-cla          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no ulteriore suddivisione per la classe merceolo-  *
      *        * gica per il prodotto                                  *
      *        *-------------------------------------------------------*
           05  w-sel-fir-sud-cla          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico per la classe merceologica per il prodotto  *
      *        *-------------------------------------------------------*
           05  w-sel-fir-mne-cla          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per la classe merceologica per il pro-    *
      *        * dotto                                                 *
      *        *-------------------------------------------------------*
           05  w-sel-fir-des-cla          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice gruppo merceologico per il prodotto            *
      *        *-------------------------------------------------------*
           05  w-sel-fir-cod-gru          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza per il gruppo merceologico per il  *
      *        * prodotto                                              *
      *        *-------------------------------------------------------*
           05  w-sel-fir-sqz-gru          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no ulteriore suddivisione per il gruppo merceolo-  *
      *        * gico per il prodotto                                  *
      *        *-------------------------------------------------------*
           05  w-sel-fir-sud-gru          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico per il gruppo merceologica per il prodotto  *
      *        *-------------------------------------------------------*
           05  w-sel-fir-mne-gru          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per il gruppo merceologico per il pro-    *
      *        * dotto                                                 *
      *        *-------------------------------------------------------*
           05  w-sel-fir-des-gru          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice sottogruppo merceologico per il prodotto       *
      *        *-------------------------------------------------------*
           05  w-sel-fir-cod-sgr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di sequenza per il sottogruppo merceologico    *
      *        * per il prodotto                                       *
      *        *-------------------------------------------------------*
           05  w-sel-fir-sqz-sgr          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Mnemonico per il sottogruppo merceologica per il pro- *
      *        * dotto                                                 *
      *        *-------------------------------------------------------*
           05  w-sel-fir-mne-sgr          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per il sottogruppo merceologico per il    *
      *        * prodotto                                              *
      *        *-------------------------------------------------------*
           05  w-sel-fir-des-sgr          pic  x(40)                  .

      *    *===========================================================*
      *    * Work area per esecuzione stampa                           *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Sub-work per intestazione pagina                      *
      *        *-------------------------------------------------------*
           05  w-stp-int.
      *            *---------------------------------------------------*
      *            * Numero totale di intestazioni eseguite            *
      *            *---------------------------------------------------*
               10  w-stp-int-num-int      pic  9(05)       value zero .
      *            *---------------------------------------------------*
      *            * Titolo per lo stampato, linea 1 e linea 2,  alli- *
      *            * neati a sinistra                                  *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stp      pic  x(80)                  .
               10  w-stp-int-ti2-stp      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina da stampare                         *
      *            *---------------------------------------------------*
               10  w-stp-int-num-pag      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Data di stampa                                    *
      *            *---------------------------------------------------*
               10  w-stp-int-dat-stp      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale azienda allineata a sinistra      *
      *            *---------------------------------------------------*
               10  w-stp-int-rag-azi      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza della ragione sociale azienda, allinea- *
      *            * ta a sinistra, senza considerare gli spazi in co- *
      *            * da                                                *
      *            *---------------------------------------------------*
               10  w-stp-int-rag-azl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza del titolo per lo stampato, allineato a *
      *            * sinistra, senza considerare gli spazi in coda     *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Colonna di stampa per il titolo dello stampato    *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Necessarieta' di una o due linee di stampa per il *
      *            * titolo dello stampato : 'U' o 'D'                 *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-uod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero caratteri di slittamento a destra per data *
      *            * e pagina                                          *
      *            *---------------------------------------------------*
               10  w-stp-int-ncs-dep      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-int-wct-c01      pic  9(03)                  .
               10  w-stp-int-wct-c02      pic  9(03)                  .
               10  w-stp-int-wct-c03      pic  9(03)                  .
               10  w-stp-int-wct-c04      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodi per editing                                *
      *            *---------------------------------------------------*
               10  w-stp-int-wed-e01      pic  x(40)                  .
               10  w-stp-int-wed-dt0      pic  x(08)                  .
               10  w-stp-int-wed-dt9      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per sub-intestazione pagina                  *
      *        *-------------------------------------------------------*
           05  w-stp-sub.
      *            *---------------------------------------------------*
      *            * Area da stampare, centrata sulla pagina, allinea- *
      *            * ta a sinistra                                     *
      *            *---------------------------------------------------*
               10  w-stp-sub-ads-asx      pic  x(72)                  .
      *            *---------------------------------------------------*
      *            * Area da stampare, centrata sulla pagina, allinea- *
      *            * ta al centro                                      *
      *            *---------------------------------------------------*
               10  w-stp-sub-ads-sub      pic  x(72)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza area da stampare                        *
      *            *---------------------------------------------------*
               10  w-stp-sub-ads-lng      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di stampa iniziale, centrata sulla pa-  *
      *            * gina                                              *
      *            *---------------------------------------------------*
               10  w-stp-sub-pos-dsi      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Literal per sub-intestazione                      *
      *            *---------------------------------------------------*
               10  w-stp-sub-lit-sub      pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Tipo di codice per sub-intestazione               *
      *            *   - N : Numerico                                  *
      *            *   - A : Alfanumerico                              *
      *            *---------------------------------------------------*
               10  w-stp-sub-tip-cod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore codice numerico                            *
      *            *---------------------------------------------------*
               10  w-stp-sub-cod-num      pic  9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore codice alfanumerico                        *
      *            *---------------------------------------------------*
               10  w-stp-sub-cod-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Valore appendice al codice                        *
      *            *---------------------------------------------------*
               10  w-stp-sub-app-cod      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Valore descrizione                                *
      *            *---------------------------------------------------*
               10  w-stp-sub-cod-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Valore codice numerico editato                    *
      *            *---------------------------------------------------*
               10  w-stp-sub-cod-ned      pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-sub-wct-c01      pic  9(03)                  .
               10  w-stp-sub-wct-c02      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per fincatura verticale                      *
      *        *-------------------------------------------------------*
           05  w-stp-fnc.
      *            *---------------------------------------------------*
      *            * Numero totale di fincature stampate               *
      *            *---------------------------------------------------*
               10  w-stp-fnc-num-fnc      pic  9(05)       value zero .
      *            *---------------------------------------------------*
      *            * Titolo di sinistra per la fincatura, allineato a  *
      *            * sinistra                                          *
      *            *---------------------------------------------------*
               10  w-stp-fnc-tit-asx      pic  x(56)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza del titolo di sinistra per la fincatura *
      *            * allineato a sinistra, senza considerare gli spazi *
      *            * in coda                                           *
      *            *---------------------------------------------------*
               10  w-stp-fnc-tit-asl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Colonna di stampa per il titolo di sinistra per   *
      *            * la fincatura                                      *
      *            *---------------------------------------------------*
               10  w-stp-fnc-tit-asc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-fnc-wct-c01      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa del codice prodotto, della sua    *
      *        * descrizione                                           *
      *        *-------------------------------------------------------*
           05  w-stp-ced-pro.
      *            *---------------------------------------------------*
      *            * Codice numerico del prodotto                      *
      *            *---------------------------------------------------*
               10  w-stp-ced-pro-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice alfanumerico del prodotto                  *
      *            *---------------------------------------------------*
               10  w-stp-ced-pro-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Descrizione del prodotto                          *
      *            *---------------------------------------------------*
               10  w-stp-ced-pro-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Area editata di comodo                            *
      *            *---------------------------------------------------*
               10  w-stp-ced-pro-e40      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa voci Unita' di misura, Quantita', *
      *        * Fatturato, e %                                        *
      *        *-------------------------------------------------------*
           05  w-stp-sta-qfp.
      *            *---------------------------------------------------*
      *            * Si/No stampa quantita'                            *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-snq      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali per le quantita'                  *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura per le quantita'                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' 1                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-q01      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 1                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-f01      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Quantita' 2                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-q02      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 2                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-f02      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * % di variazione 1                                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-p01      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Quantita' 3                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-q03      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 3                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-f03      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * % di variazione 2                                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-p02      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Valore del campo 'Note'                           *
      *            *---------------------------------------------------*
               10  w-stp-sta-qfp-not      pic  x(80)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa totali                            *
      *        *-------------------------------------------------------*
           05  w-stp-sta-tot.
      *            *---------------------------------------------------*
      *            * Literal per il totale, allineato a sinistra       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-lit.
                   15  w-stp-sta-tot-lch occurs 56
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Leading character                                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-ldc      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No stampa quantita'                            *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-snq      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero decimali per la quantita'                  *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Unita' di misura                                  *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-umi      pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Quantita' 1                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-q01      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 1                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-f01      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Quantita' 2                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-q02      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 2                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-f02      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * % di variazione 1                                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-p01      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Quantita' 3                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-q03      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Fatturato 3                                       *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-f03      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * % di variazione 2                                 *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-p02      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Valore del campo note                             *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-not      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-sta-tot-c01      pic  9(03)                  .
               10  w-stp-sta-tot-c02      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per editing voce 'Quantita''                 *
      *        *-------------------------------------------------------*
           05  w-stp-edt-qta.
      *            *---------------------------------------------------*
      *            * Numero decimali per la quantita'                  *
      *            *---------------------------------------------------*
               10  w-stp-edt-qta-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Valore quantita' da editare                       *
      *            *---------------------------------------------------*
               10  w-stp-edt-qta-qta      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore quantita' editata                          *
      *            *---------------------------------------------------*
               10  w-stp-edt-qta-edt      pic  x(15)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per editing voce 'Fatturato'                 *
      *        *-------------------------------------------------------*
           05  w-stp-edt-fat.
      *            *---------------------------------------------------*
      *            * Valore fatturato da editare                       *
      *            *---------------------------------------------------*
               10  w-stp-edt-fat-fat      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore fatturato editata                          *
      *            *---------------------------------------------------*
               10  w-stp-edt-fat-edt      pic  x(16)                  .
      *            *---------------------------------------------------*
      *            * Literal fatturato                                 *
      *            *---------------------------------------------------*
               10  w-stp-edt-fat-lit      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per editing voce '% di variazione'           *
      *        *-------------------------------------------------------*
           05  w-stp-edt-pdv.
      *            *---------------------------------------------------*
      *            * Valore del primo dei due parametri                *
      *            *---------------------------------------------------*
               10  w-stp-edt-pdv-1ov      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore del secondo dei due parametri              *
      *            *---------------------------------------------------*
               10  w-stp-edt-pdv-2ov      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Valore della % di variazione da editare           *
      *            *---------------------------------------------------*
               10  w-stp-edt-pdv-pdv      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Valore % di variazione editata                    *
      *            *---------------------------------------------------*
               10  w-stp-edt-pdv-edt      pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per calcolo percentuali di variazione        *
      *        *-------------------------------------------------------*
           05  w-stp-clc-pdv.
      *            *---------------------------------------------------*
      *            * Parametro 1                                       *
      *            *---------------------------------------------------*
               10  w-stp-clc-pdv-v01      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * Parametro 2                                       *
      *            *---------------------------------------------------*
               10  w-stp-clc-pdv-v02      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * % di variazione 1                                 *
      *            *---------------------------------------------------*
               10  w-stp-clc-pdv-p01      pic s9(13)v9(01)            .
      *            *---------------------------------------------------*
      *            * Parametro 3                                       *
      *            *---------------------------------------------------*
               10  w-stp-clc-pdv-v03      pic s9(13)v9(03)            .
      *            *---------------------------------------------------*
      *            * % di variazione 2                                 *
      *            *---------------------------------------------------*
               10  w-stp-clc-pdv-p02      pic s9(13)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Sub-work per calcolo caratteristiche campo 'Note'     *
      *        *-------------------------------------------------------*
           05  w-stp-clc-not.
      *            *---------------------------------------------------*
      *            * Lunghezza del campo Note                          *
      *            *---------------------------------------------------*
               10  w-stp-clc-not-lun      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di stampa per il campo Note             *
      *            *---------------------------------------------------*
               10  w-stp-clc-not-pos      pic  9(03)                  .

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
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Lettura record richieste                        *
      *              *-------------------------------------------------*
           perform   let-rec-ric-000      thru let-rec-ric-999        .
           if        w-cnt-let-rec-ric    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Lettura parametri di selezione stampa           *
      *              *-------------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to main-900.
       main-300.
      *              *-------------------------------------------------*
      *              * Open files per routine di stampa                *
      *              *-------------------------------------------------*
           perform   prn-opn-fls-000      thru prn-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione eventuale sort preliminare           *
      *              *-------------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se sort eseguito oppure no *
      *              *-------------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to main-400
           else      go to main-500.
       main-400.
      *              *-------------------------------------------------*
      *              * Se sort non eseguito                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di report-program                     *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-500.
      *              *-------------------------------------------------*
      *              * Se sort eseguito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-600.
      *              *-------------------------------------------------*
      *              * Close files per routine di stampa               *
      *              *-------------------------------------------------*
           perform   prn-cls-fls-000      thru prn-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo stampa                     *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
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
           move      "N"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm
           else      move  spaces         to   w-cnt-dic-ini-pgm      .
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
      *    * Lettura record richieste                                  *
      *    *-----------------------------------------------------------*
       let-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-rec-ric      .
      *              *-------------------------------------------------*
      *              * Test se programma senza richieste               *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to let-rec-ric-999.
      *              *-------------------------------------------------*
      *              * Richiesta tipo funzionamento a segreteria       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizio lettura record richieste                 *
      *              *-------------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      s-fun                to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric
                     go to let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Estrazione segmenti da 255  bytes da record ri- *
      *              * chieste                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stu-pnt-stu      .
       let-rec-ric-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           if        b-rsc                not  = spaces
                     go to let-rec-ric-200.
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    b-chr
                     delimited by size    into rr
                                  with pointer w-cnt-stu-pnt-stu      .
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-rec-ric-100.
       let-rec-ric-200.
      *              *-------------------------------------------------*
      *              * Fine lettura record richieste                   *
      *              *-------------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric      .
       let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo trattamento richieste             *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
       let-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l07            to   w-cnt-prn-sav-l07      .
           move      w-rot-l06            to   w-cnt-prn-sav-l06      .
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-770      thru prn-rou-pri-771        .
           perform   prn-rou-pri-760      thru prn-rou-pri-761        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 7. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l07            =    w-cnt-prn-sav-l07
                     go to prn-rou-pri-303.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l07    to   w-rot-l07              .
           move      w-cnt-prn-sav-l06    to   w-rot-l06              .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-860      thru prn-rou-pri-861        .
           perform   prn-rou-pri-870      thru prn-rou-pri-871        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-770      thru prn-rou-pri-771        .
           perform   prn-rou-pri-760      thru prn-rou-pri-761        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-303.
      *              *-------------------------------------------------*
      *              * Se rottura del 6. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l06            =    w-cnt-prn-sav-l06
                     go to prn-rou-pri-306.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l06    to   w-rot-l06              .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-860      thru prn-rou-pri-861        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-760      thru prn-rou-pri-761        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-306.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-860      thru prn-rou-pri-861        .
           perform   prn-rou-pri-870      thru prn-rou-pri-871        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-760.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 6. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-761.
           perform   prn-ini-lr6-000      thru prn-ini-lr6-999        .
       prn-rou-pri-761.
           exit.
       prn-rou-pri-770.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 7. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-771.
           perform   prn-ini-lr7-000      thru prn-ini-lr7-999        .
       prn-rou-pri-771.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-860.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 6. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-861.
           perform   prn-fin-lr6-000      thru prn-fin-lr6-999        .
       prn-rou-pri-861.
           exit.
       prn-rou-pri-870.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 7. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-871.
           perform   prn-fin-lr7-000      thru prn-fin-lr7-999        .
       prn-rou-pri-871.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
       int-pag-std-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
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
      *              * Lettura della personalizzazione per il tratta-  *
      *              * mento del codice cliente nelle statistiche sul  *
      *              * fatturato                                       *
      *              *-------------------------------------------------*
           perform   prs-svf-cli-000      thru prs-svf-cli-999        .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per il tratta-  *
      *              * mento del salto pagina e della rinumerazione    *
      *              * pagine da 1 alle rotture fondamentali           *
      *              *-------------------------------------------------*
           perform   prs-svf-spr-000      thru prs-svf-spr-999        .
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione per la stampa   *
      *              * dei totali per le quantita' non omogenee nel-   *
      *              * le statistiche sul fatturato                    *
      *              *-------------------------------------------------*
           perform   prs-svf-tqn-000      thru prs-svf-tqn-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione su statistiche sul fatturato    *
      *    * per trattamento del codice cliente                        *
      *    *-----------------------------------------------------------*
       prs-svf-cli-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/svf[cod-cli]"
                                delimited by   size
                     i-ide-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-cli-150.
       prs-svf-cli-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-svf-cli-500.
       prs-svf-cli-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-cli-700.
       prs-svf-cli-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf[cod-cli]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-cli-650.
       prs-svf-cli-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-svf-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-cli-700.
       prs-svf-cli-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-cli-700.
       prs-svf-cli-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo codice cliente da esporre          'C' *
      *                  *---------------------------------------------*
           if        w-prs-svf-cli-tco    not  = "M"
                     move  "C"            to   w-prs-svf-cli-tco      .
      *                  *---------------------------------------------*
      *                  * Raggruppamento dipendenze               'S' *
      *                  *---------------------------------------------*
           if        w-prs-svf-cli-rgd    not  = "N"
                     move  "S"            to   w-prs-svf-cli-rgd      .
       prs-svf-cli-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-svf-cli-999.
       prs-svf-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione su statistiche sul fatturato    *
      *    * per salto pagina e rinumerazione pagina da 1 a rotture    *
      *    * fondamentali                                              *
      *    *-----------------------------------------------------------*
       prs-svf-spr-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/svf[slp-rdu]"
                                delimited by   size
                     i-ide-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-spr-150.
       prs-svf-spr-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-svf-spr-500.
       prs-svf-spr-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-spr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-spr-700.
       prs-svf-spr-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf[slp-rdu]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-spr-650.
       prs-svf-spr-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-svf-spr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-spr-700.
       prs-svf-spr-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-spr          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-spr-700.
       prs-svf-spr-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No salto pagina                      'S' *
      *                  *---------------------------------------------*
           if        w-prs-svf-spr-slp    not  = "N"
                     move  "S"            to   w-prs-svf-spr-slp      .
      *                  *---------------------------------------------*
      *                  * Rinumerazione pagine da 1               'S' *
      *                  *---------------------------------------------*
           if        w-prs-svf-spr-rdu    not  = "N"
                     move  "S"            to   w-prs-svf-spr-rdu      .
           if        w-prs-svf-spr-slp    not  = "S"
                     move  "N"            to   w-prs-svf-spr-rdu      .
       prs-svf-spr-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-svf-spr-999.
       prs-svf-spr-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per la stampa dei totali per le *
      *    * quantita' non omogenee nelle statistiche sul fatturato    *
      *    *-----------------------------------------------------------*
       prs-svf-tqn-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/svf[snx-tqn]"
                                delimited by   size
                     i-ide-fas
                                delimited by   size
                                          into s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-tqn-150.
       prs-svf-tqn-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-svf-tqn-500.
       prs-svf-tqn-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-tqn          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-tqn-700.
       prs-svf-tqn-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/svf[snx-tqn]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-svf-tqn-650.
       prs-svf-tqn-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-svf-tqn          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-tqn-700.
       prs-svf-tqn-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-svf-tqn          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-svf-tqn-700.
       prs-svf-tqn-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No stampa totali quantita'           'S' *
      *                  *---------------------------------------------*
           if        w-prs-svf-tqn-snx    not  = "N"
                     move  "S"            to   w-prs-svf-tqn-snx      .
       prs-svf-tqn-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-svf-tqn-999.
       prs-svf-tqn-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No record richieste                          *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No stampa                                    *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Open files                         *
      *    *-----------------------------------------------------------*
       prn-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Open modulo per la determinazione della voce    *
      *              * 'Fatturato' sulle statistiche di vendita sul    *
      *              * fatturato da Righe Documenti                    *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   w-mod-fat-fir-fas      .
           perform   mod-fat-fir-opn-000  thru mod-fat-fir-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo per la determinazione del valore    *
      *              * della voce 'Note' da esporre sulle statistiche  *
      *              * di vendita sul fatturato da Testate Documenti   *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   w-mod-not-svf-fas      .
           move      rr                   to   w-mod-not-svf-war      .
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     move  01             to   w-mod-not-svf-npr
           else      move  rr-num-pdr     to   w-mod-not-svf-npr      .
           perform   mod-not-svf-opn-000  thru mod-not-svf-opn-999    .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
      *              * Filtro per selezione ed ordinamento [dcp]       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * Lettura tipo ordinamento da filtro di selezione *
      *              * ed ordinamento per file [dcp].                  *
      *              *                                                 *
      *              * Nota : Il richiamo della funzione 'TO' viene e- *
      *              *        seguito solamente perche' la funzione di *
      *              *        sola selezione 'SE', in seguito richia-  *
      *              *        mata, pretende la preventiva esecuzione  *
      *              *        della richiesta di tipo ordinamento.     *
      *              *-------------------------------------------------*
           move      "TO"                 to   f-ope                  .
           move      rr-fso-dcp-alf       to   f-key                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo per la determinazione della voce   *
      *              * 'Fatturato' sulle statistiche di vendita sul    *
      *              * fatturato da Righe Documenti                    *
      *              *-------------------------------------------------*
           perform   mod-fat-fir-cls-000  thru mod-fat-fir-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo per la determinazione del valore   *
      *              * della voce 'Note' da esporre sulle statistiche  *
      *              * di vendita sul fatturato da Testate Documenti   *
      *              *-------------------------------------------------*
           perform   mod-not-svf-cls-000  thru mod-not-svf-cls-999    .
      *              *-------------------------------------------------*
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
      *              *-------------------------------------------------*
      *              * [gxr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
      *              * [zp1]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
      *              *-------------------------------------------------*
      *              * [zp2]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
      *              *-------------------------------------------------*
      *              * [zp3]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
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
      *              * Filtro per selezione ed ordinamento [dcp]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Cancel                                      *
      *                  *---------------------------------------------*
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       prn-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : Si'                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-exe-rou-srt      .
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   stp-srt-inp-000
                                          thru stp-srt-inp-999
                     output procedure     is   prn-rou-pri-000
                                          thru prn-rou-pri-999        .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se i tipi di ordinamento   *
      *              * includono almeno una opzione in ABC oppure no.  *
      *              * Si intende necessaria una opzione in ABC anche  *
      *              * se la statistica e' sul nuovo fatturato o sul   *
      *              * non fatturato.                                  *
      *              *-------------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to stp-srt-inp-400.
           if        rr-tor-cgc           =    01 or
                     rr-tor-cgc           =    02 or
                     rr-tor-cgc           =    03 or
                     rr-tor-cgc           =    04 or
                     rr-tor-cli           =    01 or
                     rr-tor-cli           =    02 or
                     rr-tor-cli           =    03 or
                     rr-tor-cli           =    04 or
                     rr-tor-pro           =    01 or
                     rr-tor-pro           =    02 or
                     rr-tor-pro           =    03 or
                     rr-tor-pro           =    04
                     go to stp-srt-inp-400.
       stp-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Se i tipi di ordinamento non includono alcuna   *
      *              * opzione in ABC                                  *
      *              *-------------------------------------------------*
       stp-srt-inp-150.
      *                  *---------------------------------------------*
      *                  * Inizializzazione lettura sequenziale archi- *
      *                  * vio [fir]                                   *
      *                  *---------------------------------------------*
           perform   fil-fir-ing-000      thru fil-fir-ing-999        .
       stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale archivio [fir]          *
      *                  *---------------------------------------------*
           perform   fil-fir-get-000      thru fil-fir-get-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-flg    not  = spaces
                     go to stp-srt-inp-250.
      *                  *---------------------------------------------*
      *                  * Se quantita' e fatturato sono entrambi a    *
      *                  * zero : si ignora il record                  *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-qta    =    zero and
                     w-mod-fat-fir-val    =    zero
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Se i valori non incidono sulla statistica   *
      *                  * ne' in positivo ne' in negativo : si igno-  *
      *                  * ra il record                                *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-pon    not  = "P" and
                     w-mod-fat-fir-pon    not  = "N"
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Se i valori non incidono su nessuno dei tre *
      *                  * periodi della statistica : si ignora il re- *
      *                  * cord                                        *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-sn1    not  = "S" and
                     w-mod-fat-fir-sn2    not  = "S" and
                     w-mod-fat-fir-sn3    not  = "S"
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Selezione sul record [fit] in esame         *
      *                  *---------------------------------------------*
           perform   sel-fit-rec-000      thru sel-fit-rec-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si  ignora il   *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-sel-fit-flg-exi    not  = spaces
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Selezione sul record [fir] in esame         *
      *                  *---------------------------------------------*
           perform   sel-fir-rec-000      thru sel-fir-rec-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si  ignora il   *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-sel-fir-flg-exi    not  = spaces
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Composizione record [srt] dalle aree :      *
      *                  * - [fit]                                     *
      *                  * - [fir]                                     *
      *                  * - w-sel-fit                                 *
      *                  * - w-sel-fir                                 *
      *                  * - w-mod-fat-fir                             *
      *                  * - w-not-fat-fir                             *
      *                  * - rr                                        *
      *                  *---------------------------------------------*
           perform   cmp-srt-rec-000      thru cmp-srt-rec-999        .
      *                  *---------------------------------------------*
      *                  * Rilascio del record al Sort                 *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura sequenziale archivio   *
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-200.
       stp-srt-inp-250.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Se i tipi di ordinamento includono almeno una   *
      *              * opzione in ABC.                                 *
      *              * Si intende necessaria una opzione in ABC anche  *
      *              * se la statistica e' sul nuovo fatturato o sul   *
      *              * non fatturato.                                  *
      *              *-------------------------------------------------*
       stp-srt-inp-450.
      *                  *---------------------------------------------*
      *                  * Open file di appoggio sequenziale [sqz]     *
      *                  *---------------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Open file di appoggio ad indici [kk0]       *
      *                  *---------------------------------------------*
           perform   opn-fil-kk0-000      thru opn-fil-kk0-999        .
       stp-srt-inp-500.
      *                  *---------------------------------------------*
      *                  * Inizializzazione lettura sequenziale archi- *
      *                  * vio [fir]                                   *
      *                  *---------------------------------------------*
           perform   fil-fir-ing-000      thru fil-fir-ing-999        .
       stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale archivio [fir]          *
      *                  *---------------------------------------------*
           perform   fil-fir-get-000      thru fil-fir-get-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : a chiusura/riapertura        *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-flg    not  = spaces
                     go to stp-srt-inp-600.
      *                  *---------------------------------------------*
      *                  * Se quantita' e fatturato sono entrambi a    *
      *                  * zero : si ignora il record                  *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-qta    =    zero and
                     w-mod-fat-fir-val    =    zero
                     go to stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Se i valori non incidono sulla statistica   *
      *                  * ne' in positivo ne' in negativo : si igno-  *
      *                  * ra il record                                *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-pon    not  = "P" and
                     w-mod-fat-fir-pon    not  = "N"
                     go to stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Se i valori non incidono su nessuno dei tre *
      *                  * periodi della statistica : si ignora il re- *
      *                  * cord                                        *
      *                  *---------------------------------------------*
           if        w-mod-fat-fir-sn1    not  = "S" and
                     w-mod-fat-fir-sn2    not  = "S" and
                     w-mod-fat-fir-sn3    not  = "S"
                     go to stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Selezione sul record [fit] in esame         *
      *                  *---------------------------------------------*
           perform   sel-fit-rec-000      thru sel-fit-rec-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si  ignora il   *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-sel-fit-flg-exi    not  = spaces
                     go to stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Selezione sul record [fir] in esame         *
      *                  *---------------------------------------------*
           perform   sel-fir-rec-000      thru sel-fir-rec-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si  ignora il   *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-sel-fir-flg-exi    not  = spaces
                     go to stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Composizione record [srt] dalle aree :      *
      *                  * - [fit]                                     *
      *                  * - [fir]                                     *
      *                  * - w-sel-fit                                 *
      *                  * - w-sel-fir                                 *
      *                  * - w-mod-fat-fir                             *
      *                  * - w-not-fat-fir                             *
      *                  * - rr                                        *
      *                  *---------------------------------------------*
           perform   cmp-srt-rec-000      thru cmp-srt-rec-999        .
      *                  *---------------------------------------------*
      *                  * Da area record [srt] ad area record file    *
      *                  * sequenziale [sqz]                           *
      *                  *---------------------------------------------*
           move      srt-rec              to   sqz-rec                .
      *                  *---------------------------------------------*
      *                  * Scrittura record file sequenziale [sqz]     *
      *                  *---------------------------------------------*
           perform   wrt-fil-sqz-000      thru wrt-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento valori ABC su file di appog-  *
      *                  * gio ad indici                               *
      *                  *---------------------------------------------*
           perform   agg-abc-kk0-000      thru agg-abc-kk0-999        .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura sequenziale archivio   *
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-550.
       stp-srt-inp-600.
      *                  *---------------------------------------------*
      *                  * Close file di appoggio sequenziale [sqz]    *
      *                  *---------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Open file di appoggio sequenziale [sqz]     *
      *                  *---------------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
       stp-srt-inp-650.
      *                  *---------------------------------------------*
      *                  * Read file di appoggio sequenziale [sqz]     *
      *                  *---------------------------------------------*
           perform   rea-fil-sqz-000      thru rea-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sqz-sts            not  = e-not-err
                     go to stp-srt-inp-700.
      *                  *---------------------------------------------*
      *                  * Da area record file sequenziale [kk0] ad a- *
      *                  * rea record [sqz]                            *
      *                  *---------------------------------------------*
           move      sqz-rec              to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Aggiornamento valori ABC su file di appog-  *
      *                  * gio sequenziale                             *
      *                  *---------------------------------------------*
           perform   agg-abc-sqz-000      thru agg-abc-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Rilascio del record al Sort                 *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura sequenziale archivio   *
      *                  * [sqz]                                       *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-650.
       stp-srt-inp-700.
      *                  *---------------------------------------------*
      *                  * Close file di appoggio sequenziale [sqz]    *
      *                  *---------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Close file di appoggio ad indici [kk0]      *
      *                  *---------------------------------------------*
           perform   cls-fil-kk0-000      thru cls-fil-kk0-999        .
      *                  *---------------------------------------------*
      *                  * Delete file di appoggio sequenziale [sqz]   *
      *                  *---------------------------------------------*
           perform   del-fil-sqz-000      thru del-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Delete file di appoggio ad indici [kk0]     *
      *                  *---------------------------------------------*
           perform   del-fil-kk0-000      thru del-fil-kk0-999        .
       stp-srt-inp-750.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione lettura sequenziale archivio [fir]       *
      *    *-----------------------------------------------------------*
       fil-fir-ing-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione 'GS' del modulo 'mfatfir0'    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero periodi di riferimento           *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     move  02             to   w-mod-fat-fir-npr
           else if   rr-num-pdr           =    21 or
                     rr-num-pdr           =    22
                     move  01             to   w-mod-fat-fir-npr
           else      move  rr-num-pdr     to   w-mod-fat-fir-npr      .
      *                      *-----------------------------------------*
      *                      * Tipo calcolo voce 'Costo del Fatturato' *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    21
                     move  "P"            to   w-mod-fat-fir-ccf
           else if   rr-num-pdr           =    22
                     move  "U"            to   w-mod-fat-fir-ccf
           else      move  spaces         to   w-mod-fat-fir-ccf      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data min                    *
      *                      *-----------------------------------------*
           move      rr-p1d-min           to   w-mod-fat-fir-p1i      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data max                    *
      *                      *-----------------------------------------*
           move      rr-p1d-max           to   w-mod-fat-fir-p1f      .
      *                      *-----------------------------------------*
      *                      * 2. periodo, data min                    *
      *                      *-----------------------------------------*
           move      rr-p2d-min           to   w-mod-fat-fir-p2i      .
      *                      *-----------------------------------------*
      *                      * 2. periodo, data max                    *
      *                      *-----------------------------------------*
           move      rr-p2d-max           to   w-mod-fat-fir-p2f      .
      *                      *-----------------------------------------*
      *                      * 3. periodo, data min                    *
      *                      *-----------------------------------------*
           move      rr-p3d-min           to   w-mod-fat-fir-p3i      .
      *                      *-----------------------------------------*
      *                      * 3. periodo, data max                    *
      *                      *-----------------------------------------*
           move      rr-p3d-max           to   w-mod-fat-fir-p3f      .
      *                  *---------------------------------------------*
      *                  * Chiamata modulo                             *
      *                  *---------------------------------------------*
           perform   mod-fat-fir-gts-000  thru mod-fat-fir-gts-999    .
       fil-fir-ing-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale archivio [fir]                        *
      *    *-----------------------------------------------------------*
       fil-fir-get-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione 'GT' del modulo 'mfatfir0'    *
      *              *-------------------------------------------------*
           perform   mod-fat-fir-get-000  thru mod-fat-fir-get-999    .
       fil-fir-get-999.
           exit.

      *    *===========================================================*
      *    * Selezioni su record [fit] in esame                        *
      *    *-----------------------------------------------------------*
       sel-fit-rec-000.
      *              *-------------------------------------------------*
      *              * Selezione su codice dipendenza                  *
      *              *-------------------------------------------------*
       sel-fit-rec-005.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del numero di dipen-   *
      *                  * denze selezionate                           *
      *                  *---------------------------------------------*
           if        rr-dpz-inu           not  = zero
                     go to sel-fit-rec-010
           else if   rr-dpz-ctr-dpz       =    rr-dpz-ctr-sel
                     go to sel-fit-rec-015
           else      go to sel-fit-rec-020.
       sel-fit-rec-010.
      *                  *---------------------------------------------*
      *                  * Se una sola dipendenza selezionata          *
      *                  *---------------------------------------------*
           if        rf-fit-cod-dpz       =    rr-dpz-inu
                     go to sel-fit-rec-100
           else      go to sel-fit-rec-900.
       sel-fit-rec-015.
      *                  *---------------------------------------------*
      *                  * Se tutte le dipendenze selezionate          *
      *                  *---------------------------------------------*
           go to     sel-fit-rec-100.
       sel-fit-rec-020.
      *                  *---------------------------------------------*
      *                  * Se solo alcune dipendenze selezionate       *
      *                  *---------------------------------------------*
           move      zero                 to   w-sel-fit-ctr-dpz      .
       sel-fit-rec-025.
           add       1                    to   w-sel-fit-ctr-dpz      .
           if        w-sel-fit-ctr-dpz    >    rr-dpz-ctr-sel
                     go to sel-fit-rec-900.
           if        rf-fit-cod-dpz       =    rr-dpz-ele-cod
                                              (w-sel-fit-ctr-dpz)
                     go to sel-fit-rec-100
           else      go to sel-fit-rec-025.
       sel-fit-rec-100.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica [cli] e [dcc]                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-fit-cod-cli       to   w-let-cli-dcc-cli      .
           move      rf-fit-dpz-cli       to   w-let-cli-dcc-dpz      .
           perform   let-cli-dcc-000      thru let-cli-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-cli    to   w-sel-fit-cod-cli      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza per il cliente        *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-dpz    to   w-sel-fit-dpz-cli      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale per il cliente          *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rag    to   w-sel-fit-rag-cli      .
      *                      *-----------------------------------------*
      *                      * Mnemonico per il cliente                *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-mne    to   w-sel-fit-mne-cli      .
      *                      *-----------------------------------------*
      *                      * Codice nazione per il cliente           *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-naz    to   w-sel-fit-naz-cli      .
      *                      *-----------------------------------------*
      *                      * Codice comune per il cliente            *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-cmn    to   w-sel-fit-cmn-cli      .
      *                      *-----------------------------------------*
      *                      * Codice regione per il cliente           *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rgn    to   w-sel-fit-rgn-cli      .
      *                      *-----------------------------------------*
      *                      * Codice provincia per il cliente         *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-prv    to   w-sel-fit-prv-cli      .
       sel-fit-rec-200.
      *              *-------------------------------------------------*
      *              * Selezione su codice classe geografica cliente   *
      *              *-------------------------------------------------*
       sel-fit-rec-225.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di classe     *
      *                  * geografica cliente selezionata              *
      *                  *---------------------------------------------*
           if        rr-tcg-cli           =    01
                     go to sel-fit-rec-250
           else if   rr-tcg-cli           =    02
                     go to sel-fit-rec-275
           else if   rr-tcg-cli           =    03
                     go to sel-fit-rec-300.
       sel-fit-rec-250.
      *                  *---------------------------------------------*
      *                  * Se tipo di classe geografica cliente : 01,  *
      *                  * nazione                                     *
      *                  *---------------------------------------------*
       sel-fit-rec-255.
      *                      *-----------------------------------------*
      *                      * Se un solo codice da selezionare, e il  *
      *                      * codice associato al cliente non corri-  *
      *                      * sponde a quello da selezionare : usci-  *
      *                      * ta per selezione non superata           *
      *                      *-----------------------------------------*
           if        rr-ccg-cli           not  = spaces        and
                     rr-ccg-cli           not  = w-sel-fit-naz-cli
                     go to sel-fit-rec-900.
       sel-fit-rec-260.
      *                      *-----------------------------------------*
      *                      * Lettura [gxn] per la nazione            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-sel-fit-naz-cli    to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                          *-------------------------------------*
      *                          * Dati letti in area di comodo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice nazione                  *
      *                              *---------------------------------*
           move      w-let-arc-gxn-cod    to   w-sel-fit-cod-cgc      .
      *                              *---------------------------------*
      *                              * Descrizione nazione             *
      *                              *---------------------------------*
           move      w-let-arc-gxn-des    to   w-sel-fit-des-cgc      .
      *                              *---------------------------------*
      *                              * Flag di nazione esistente       *
      *                              *---------------------------------*
           move      w-let-arc-gxn-flg    to   w-sel-fit-flg-cgc      .
       sel-fit-rec-265.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sel-fit-rec-400.
       sel-fit-rec-275.
      *                  *---------------------------------------------*
      *                  * Se tipo di classe geografica cliente : 02,  *
      *                  * regione                                     *
      *                  *---------------------------------------------*
       sel-fit-rec-280.
      *                      *-----------------------------------------*
      *                      * Se un solo codice da selezionare, e il  *
      *                      * codice associato al cliente non corri-  *
      *                      * sponde a quello da selezionare : usci-  *
      *                      * ta per selezione non superata           *
      *                      *-----------------------------------------*
           if        rr-ccg-cli           not  = spaces        and
                     rr-ccg-cli           not  = w-sel-fit-rgn-cli
                     go to sel-fit-rec-900.
       sel-fit-rec-285.
      *                      *-----------------------------------------*
      *                      * Lettura [gxr] per la regione            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-sel-fit-rgn-cli    to   w-let-arc-gxr-cod      .
           perform   let-arc-gxr-000      thru let-arc-gxr-999        .
      *                          *-------------------------------------*
      *                          * Dati letti in area di comodo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice regione                  *
      *                              *---------------------------------*
           move      w-let-arc-gxr-cod    to   w-sel-fit-cod-cgc      .
      *                              *---------------------------------*
      *                              * Descrizione regione             *
      *                              *---------------------------------*
           move      w-let-arc-gxr-des    to   w-sel-fit-des-cgc      .
      *                              *---------------------------------*
      *                              * Flag di regione esistente       *
      *                              *---------------------------------*
           move      w-let-arc-gxr-flg    to   w-sel-fit-flg-cgc      .
       sel-fit-rec-290.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sel-fit-rec-400.
       sel-fit-rec-300.
      *                  *---------------------------------------------*
      *                  * Se tipo di classe geografica cliente : 03,  *
      *                  * provincia                                   *
      *                  *---------------------------------------------*
       sel-fit-rec-305.
      *                      *-----------------------------------------*
      *                      * Se un solo codice da selezionare, e il  *
      *                      * codice associato al cliente non corri-  *
      *                      * sponde a quello da selezionare : usci-  *
      *                      * ta per selezione non superata           *
      *                      *-----------------------------------------*
           if        rr-ccg-cli           not  = spaces        and
                     rr-ccg-cli           not  = w-sel-fit-prv-cli
                     go to sel-fit-rec-900.
       sel-fit-rec-310.
      *                      *-----------------------------------------*
      *                      * Lettura [gxp] per la provincia          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura                             *
      *                          *-------------------------------------*
           move      w-sel-fit-prv-cli    to   w-let-arc-gxp-cod      .
           perform   let-arc-gxp-000      thru let-arc-gxp-999        .
      *                          *-------------------------------------*
      *                          * Dati letti in area di comodo        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Codice provincia                *
      *                              *---------------------------------*
           move      w-let-arc-gxp-cod    to   w-sel-fit-cod-cgc      .
      *                              *---------------------------------*
      *                              * Descrizione provincia   *
      *                              *---------------------------------*
           move      w-let-arc-gxp-des    to   w-sel-fit-des-cgc      .
      *                              *---------------------------------*
      *                              * Flag di codice provincia esist. *
      *                              *---------------------------------*
           move      w-let-arc-gxp-flg    to   w-sel-fit-flg-cgc      .
       sel-fit-rec-315.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     sel-fit-rec-400.
       sel-fit-rec-400.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione superata       *
      *                  *---------------------------------------------*
           move      spaces               to   w-sel-fit-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fit-rec-999.
       sel-fit-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione non superata               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione non superata   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sel-fit-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fit-rec-999.
       sel-fit-rec-999.
           exit.

      *    *===========================================================*
      *    * Selezioni su record [fir] in esame                        *
      *    *-----------------------------------------------------------*
       sel-fir-rec-000.
      *              *-------------------------------------------------*
      *              * Se il filtro di selezione sull'anagrafica pro-  *
      *              * dotti [dcp] indica la selezione di un solo co-  *
      *              * dice prodotto, ed il codice prodotto della ri-  *
      *              * ga in esame non corrisponde a quello da sele-   *
      *              * zionare : uscita per selezione non superata     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice filtro di selezione          *
      *                  *---------------------------------------------*
           if        rr-fso-dcp           <    10000000 or
                     rr-fso-dcp           >    19999999
                     go to sel-fir-rec-100.
      *                  *---------------------------------------------*
      *                  * Test su codice prodotto                     *
      *                  *---------------------------------------------*
           subtract  10000000             from rr-fso-dcp
                                        giving w-sel-fir-num-pro      .
           if        rf-fir-num-pro       =    w-sel-fir-num-pro
                     go to sel-fir-rec-100
           else      go to sel-fir-rec-900.
       sel-fir-rec-100.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica [dcp]                        *
      *              *-------------------------------------------------*
       sel-fir-rec-110.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      rf-fir-num-pro       to   w-let-arc-dcp-cod      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
       sel-fir-rec-120.
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice prodotto numerico                *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-cod    to   w-sel-fir-num-pro      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto alfanumerico            *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-alf    to   w-sel-fir-alf-pro      .
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto                    *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-des    to   w-sel-fir-des-pro      .
      *                      *-----------------------------------------*
      *                      * Unita' di misura di vendita per il pro- *
      *                      * dotto                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-umi    to   w-sel-fir-umi-pro      .
      *                      *-----------------------------------------*
      *                      * Numero decimali quantita' per il pro-   *
      *                      * dotto                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-dec    to   w-sel-fir-dec-pro      .
      *                      *-----------------------------------------*
      *                      * Codice classe merceologica per il pro-  *
      *                      * dotto                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-cla    to   w-sel-fir-cod-cla      .
      *                      *-----------------------------------------*
      *                      * Codice gruppo merceologico per il pro-  *
      *                      * dotto                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-gru    to   w-sel-fir-cod-gru      .
      *                      *-----------------------------------------*
      *                      * Codice sottogruppo merceologico per il  *
      *                      * prodotto                                *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-sgr    to   w-sel-fir-cod-sgr      .
       sel-fir-rec-200.
      *              *-------------------------------------------------*
      *              * Lettura classe merceologica [zp1]               *
      *              *-------------------------------------------------*
       sel-fir-rec-210.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      w-sel-fir-cod-cla    to   w-let-arc-zp1-cla      .
           perform   let-arc-zp1-000      thru let-arc-zp1-999        .
       sel-fir-rec-220.
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-let-arc-zp1-sqz    to   w-sel-fir-sqz-cla      .
      *                      *-----------------------------------------*
      *                      * Si/no ulteriore suddivisione            *
      *                      *-----------------------------------------*
           move      w-let-arc-zp1-sud    to   w-sel-fir-sud-cla      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-zp1-mne    to   w-sel-fir-mne-cla      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-let-arc-zp1-des    to   w-sel-fir-des-cla      .
       sel-fir-rec-230.
      *                  *---------------------------------------------*
      *                  * Se codice a zero, oppure se elemento non    *
      *                  * ulteriormente suddiviso si normalizzano a   *
      *                  * zero il codice gruppo e sottogruppo         *
      *                  *---------------------------------------------*
           if        w-sel-fir-cod-cla    =    zero or
                     w-let-arc-zp1-sud    not  = 02
                     move  zero           to   w-sel-fir-cod-gru
                     move  zero           to   w-sel-fir-cod-sgr      .
       sel-fir-rec-300.
      *              *-------------------------------------------------*
      *              * Lettura gruppo merceologico [zp2]               *
      *              *-------------------------------------------------*
       sel-fir-rec-310.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      w-sel-fir-cod-cla    to   w-let-arc-zp2-cla      .
           move      w-sel-fir-cod-gru    to   w-let-arc-zp2-gru      .
           perform   let-arc-zp2-000      thru let-arc-zp2-999        .
       sel-fir-rec-320.
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-let-arc-zp2-sqz    to   w-sel-fir-sqz-gru      .
      *                      *-----------------------------------------*
      *                      * Si/no ulteriore suddivisione            *
      *                      *-----------------------------------------*
           move      w-let-arc-zp2-sud    to   w-sel-fir-sud-gru      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-zp2-mne    to   w-sel-fir-mne-gru      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-let-arc-zp2-des    to   w-sel-fir-des-gru      .
       sel-fir-rec-330.
      *                  *---------------------------------------------*
      *                  * Se codice a zero, oppure se elemento non    *
      *                  * ulteriormente suddiviso si normalizza a     *
      *                  * zero il codice sottogruppo                  *
      *                  *---------------------------------------------*
           if        w-sel-fir-cod-gru    =    zero or
                     w-let-arc-zp2-sud    not  = 02
                     move  zero           to   w-sel-fir-cod-sgr      .
       sel-fir-rec-400.
      *              *-------------------------------------------------*
      *              * Lettura sottogruppo merceologico [zp3]          *
      *              *-------------------------------------------------*
       sel-fir-rec-410.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      w-sel-fir-cod-cla    to   w-let-arc-zp3-cla      .
           move      w-sel-fir-cod-gru    to   w-let-arc-zp3-gru      .
           move      w-sel-fir-cod-sgr    to   w-let-arc-zp3-sgr      .
           perform   let-arc-zp3-000      thru let-arc-zp3-999        .
       sel-fir-rec-420.
      *                  *---------------------------------------------*
      *                  * Dati letti in area di comodo                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-let-arc-zp3-sqz    to   w-sel-fir-sqz-sgr      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-zp3-mne    to   w-sel-fir-des-sgr      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-let-arc-zp3-des    to   w-sel-fir-des-sgr      .
       sel-fir-rec-500.
      *              *-------------------------------------------------*
      *              * Se il filtro di selezione sull'anagrafica pro-  *
      *              * dotti [dcp] indica la selezione di un solo co-  *
      *              * dice prodotto non si eseguono ulteriori sele-   *
      *              * zioni                                           *
      *              *-------------------------------------------------*
           if        rr-fso-dcp           not  < 10000000 and
                     rr-fso-dcp           not  > 19999999
                     go to sel-fir-rec-800.
       sel-fir-rec-600.
      *              *-------------------------------------------------*
      *              * Se record [dcp] non esistente : selezione non   *
      *              * superata                                        *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to sel-fir-rec-900.
       sel-fir-rec-700.
      *              *-------------------------------------------------*
      *              * Selezione mediante filtro di ordinamento e se-  *
      *              * lezione su anagrafica prodotti [dcp], e devia-  *
      *              * zione secondo l'esito                           *
      *              *-------------------------------------------------*
           move      "SE"                 to   f-ope                  .
           move      "pgm/dcp/prg/obj/bzosdcp0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                =    e-not-err
                     go to sel-fir-rec-800
           else      go to sel-fir-rec-900.
       sel-fir-rec-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione superata                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione superata       *
      *                  *---------------------------------------------*
           move      spaces               to   w-sel-fir-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fir-rec-999.
       sel-fir-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione non superata               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : selezione non superata   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sel-fir-flg-exi      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     sel-fir-rec-999.
       sel-fir-rec-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [srt] prelevando dalle seguenti aree  *
      *    *                                                           *
      *    *   - [fit]                                                 *
      *    *   - [fir]                                                 *
      *    *   - w-sel-fit                                             *
      *    *   - w-sel-fir                                             *
      *    *   - w-mod-fat-fir                                         *
      *    *   - w-not-fat-fir                                         *
      *    *   - rr                                                    *
      *    *-----------------------------------------------------------*
       cmp-srt-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare totale del record   *
      *              *-------------------------------------------------*
           move      spaces               to   srt-rec                .
       cmp-srt-rec-100.
      *              *-------------------------------------------------*
      *              * Area chiave di ordinamento                      *
      *              *-------------------------------------------------*
       cmp-srt-rec-200.
      *                  *---------------------------------------------*
      *                  * Subchiave 1 : Per classe geografica cliente *
      *                  *---------------------------------------------*
       cmp-srt-rec-210.
      *                      *-----------------------------------------*
      *                      * Valore ABC                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzato a zero                      *
      *                          *-------------------------------------*
           move      zero                 to   srt-k01-val-abc        .
       cmp-srt-rec-215.
      *                      *-----------------------------------------*
      *                      * Flag di codice diverso da spaces ma non *
      *                      * trovato in archivio codici geografici   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento classe :   *
      *                          *  - ABC decrescente sul fatturato    *
      *                          *  - ABC decrescente sulla quantita'  *
      *                          *  - ABC crescente sul fatturato      *
      *                          *  - ABC crescente sulla quantita'    *
      *                          *  - Per descrizione                  *
      *                          * altrimenti forzato a spaces         *
      *                          *-------------------------------------*
           if        rr-tor-cgc           not  = 01 and
                     rr-tor-cgc           not  = 02 and
                     rr-tor-cgc           not  = 03 and
                     rr-tor-cgc           not  = 04 and
                     rr-tor-cgc           not  = 05
                     move  spaces        to   srt-k01-flg-eon
                     go to cmp-srt-rec-220.
      *                          *-------------------------------------*
      *                          * Se codice a spaces : si forza '9'   *
      *                          *-------------------------------------*
           if        w-sel-fit-cod-cgc    =    spaces
                     move  9              to   srt-k01-flg-eon
                     go to cmp-srt-rec-220.
      *                          *-------------------------------------*
      *                          * Se codice diverso da spaces : a se- *
      *                          * conda del flag di esistenza         *
      *                          *-------------------------------------*
           if        w-sel-fit-flg-cgc    =    spaces
                     move  1              to   srt-k01-flg-eon
           else      move  5              to   srt-k01-flg-eon        .
       cmp-srt-rec-220.
      *                      *-----------------------------------------*
      *                      * Descrizione per classe geografica       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento classe :   *
      *                          *  - ABC decrescente sul fatturato    *
      *                          *  - ABC decrescente sulla quantita'  *
      *                          *  - ABC crescente sul fatturato      *
      *                          *  - ABC crescente sulla quantita'    *
      *                          *  - Per descrizione                  *
      *                          * altrimenti forzato a spaces         *
      *                          *-------------------------------------*
           if        rr-tor-cgc           not  = 01 and
                     rr-tor-cgc           not  = 02 and
                     rr-tor-cgc           not  = 03 and
                     rr-tor-cgc           not  = 04 and
                     rr-tor-cgc           not  = 05
                     move  spaces         to   srt-k01-des-cgc
                     go to cmp-srt-rec-225.
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fit-des-cgc    to   srt-k01-des-cgc        .
       cmp-srt-rec-225.
      *                      *-----------------------------------------*
      *                      * Flag di codice a spaces o diverso da    *
      *                      * spaces                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento classe :   *
      *                          *  - Per codice                       *
      *                          * altrimenti forzato a zero           *
      *                          *-------------------------------------*
           if        rr-tor-cgc           not  = 06
                     move  zero           to   srt-k01-flg-sod
                     go to cmp-srt-rec-230.
      *                          *-------------------------------------*
      *                          * Memorizzazione con test             *
      *                          *-------------------------------------*
           if        w-sel-fit-cod-cgc    =    spaces
                     move  9              to   srt-k01-flg-sod
           else      move  1              to   srt-k01-flg-sod        .
       cmp-srt-rec-230.
      *                      *-----------------------------------------*
      *                      * Codice classe geografica                *
      *                      *-----------------------------------------*
           move      w-sel-fit-cod-cgc    to   srt-k01-cod-cgc        .
       cmp-srt-rec-300.
      *                  *---------------------------------------------*
      *                  * Subchiave 2 : Per cliente                   *
      *                  *---------------------------------------------*
       cmp-srt-rec-310.
      *                      *-----------------------------------------*
      *                      * Valore ABC                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzato a zero                      *
      *                          *-------------------------------------*
           move      zero                 to   srt-k02-val-abc        .
       cmp-srt-rec-315.
      *                      *-----------------------------------------*
      *                      * Flag di codice diverso da zero ma non   *
      *                      * trovato in archivio codici clienti      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento clienti :  *
      *                          *  - ABC decrescente sul fatturato    *
      *                          *  - ABC decrescente sulla quantita'  *
      *                          *  - ABC crescente sul fatturato      *
      *                          *  - ABC crescente sulla quantita'    *
      *                          *  - Per ragione sociale cliente      *
      *                          *  - Per mnemonico                    *
      *                          * altrimenti forzato a zero           *
      *                          *-------------------------------------*
           if        rr-tor-cli           not  = 01 and
                     rr-tor-cli           not  = 02 and
                     rr-tor-cli           not  = 03 and
                     rr-tor-cli           not  = 04 and
                     rr-tor-cli           not  = 05 and
                     rr-tor-cli           not  = 07
                     move  zero           to   srt-k02-flg-eon
                     go to cmp-srt-rec-320.
      *                          *-------------------------------------*
      *                          * Se codice a zero : si forza '9'     *
      *                          *-------------------------------------*
            if       w-sel-fit-cod-cli    =    zero
                     move  9              to   srt-k02-flg-eon
                     go to cmp-srt-rec-320.
      *                          *-------------------------------------*
      *                          * Se codice diverso da zero : si pone *
      *                          * a seconda del valore della ragione  *
      *                          * sociale                             *
      *                          *-------------------------------------*
           if        w-sel-fit-rag-cli    not  = spaces
                     move  1              to   srt-k02-flg-eon
           else      move  5              to   srt-k02-flg-eon        .
       cmp-srt-rec-320.
      *                      *-----------------------------------------*
      *                      * Ragione sociale cliente                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento clienti :  *
      *                          *  - ABC decrescente sul fatturato    *
      *                          *  - ABC decrescente sulla quantita'  *
      *                          *  - ABC crescente sul fatturato      *
      *                          *  - ABC crescente sulla quantita'    *
      *                          *  - Per ragione sociale cliente      *
      *                          * altrimenti forzato a spaces         *
      *                          *-------------------------------------*
           if        rr-tor-cli           not  = 01 and
                     rr-tor-cli           not  = 02 and
                     rr-tor-cli           not  = 03 and
                     rr-tor-cli           not  = 04 and
                     rr-tor-cli           not  = 05
                     move  spaces         to   srt-k02-rag-cli
                     go to cmp-srt-rec-325.
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fit-rag-cli    to   srt-k02-rag-cli        .
       cmp-srt-rec-325.
      *                      *-----------------------------------------*
      *                      * Mnemonico cliente                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento clienti :  *
      *                          *  - Per mnemonico                    *
      *                          * altrimenti forzato a spaces         *
      *                          *-------------------------------------*
           if        rr-tor-cli           not  = 07
                     move  spaces         to   srt-k02-mne-cli
                     go to cmp-srt-rec-330.
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fit-mne-cli    to   srt-k02-mne-cli        .
       cmp-srt-rec-330.
      *                      *-----------------------------------------*
      *                      * Flag di codice a zero o diverso da zero *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento cliente :  *
      *                          *  - Per codice                       *
      *                          * altrimenti forzato a zero           *
      *                          *-------------------------------------*
           if        rr-tor-cli           not  = 06
                     move  zero           to   srt-k02-flg-zod
                     go to cmp-srt-rec-335.
      *                          *-------------------------------------*
      *                          * Memorizzazione con test             *
      *                          *-------------------------------------*
           if        w-sel-fit-cod-cli    =    zero
                     move  9              to   srt-k02-flg-zod
           else      move  1              to   srt-k02-flg-zod        .
       cmp-srt-rec-335.
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-sel-fit-cod-cli    to   srt-k02-cod-cli        .
       cmp-srt-rec-340.
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si memorizza il codice dipendenza   *
      *                          * cliente solo se :                   *
      *                          *  - Le personalizzazioni indicano u- *
      *                          *    na differenziazione, ove possi-  *
      *                          *    bile, per dipendenza             *
      *                          * Altrimenti si forza a spaces        *
      *                          *-------------------------------------*
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   srt-k02-dpz-cli
           else      move  w-sel-fit-dpz-cli
                                          to   srt-k02-dpz-cli        .
       cmp-srt-rec-400.
      *                  *---------------------------------------------*
      *                  * Subchiave 3 : Per prodotto                  *
      *                  *---------------------------------------------*
       cmp-srt-rec-410.
      *                      *-----------------------------------------*
      *                      * Valore ABC                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzato a zero                      *
      *                          *-------------------------------------*
           move      zero                 to   srt-k03-val-abc        .
       cmp-srt-rec-415.
      *                      *-----------------------------------------*
      *                      * Flag di codice numerico diverso da zero *
      *                      * ma non trovato in archivio prodotti     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se codice a zero : si forza '9'     *
      *                          *-------------------------------------*
            if       w-sel-fir-num-pro    =    zero
                     move  9              to   srt-k03-flg-eon
                     go to cmp-srt-rec-420.
      *                          *-------------------------------------*
      *                          * Se codice diverso da zero : si pone *
      *                          * a seconda del valore della descri-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-sel-fir-des-pro    not  = spaces
                     move  1              to   srt-k03-flg-eon
           else      move  5              to   srt-k03-flg-eon        .
       cmp-srt-rec-420.
      *                      *-----------------------------------------*
      *                      * Numero di sequenza e codice per :       *
      *                      *  - Classe merceologica prodotto         *
      *                      *  - Gruppo merceologico prodotto         *
      *                      *  - Sottogruppo merceologico prodotto    *
      *                      *-----------------------------------------*
       cmp-srt-rec-425.
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento prodotto : *
      *                          * - Per classe e codice prodotto      *
      *                          * - Per classe e descrizione prodotto *
      *                          * altrimenti forzati a zero           *
      *                          *-------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08 and
                     rr-tor-pro           not  = 09
                     move  zero           to   srt-k03-sqz-cla
                     move  zero           to   srt-k03-cod-cla
                     move  zero           to   srt-k03-sqz-gru
                     move  zero           to   srt-k03-cod-gru
                     move  zero           to   srt-k03-sqz-sgr
                     move  zero           to   srt-k03-cod-sgr
                     go to cmp-srt-rec-445.
       cmp-srt-rec-430.
      *                          *-------------------------------------*
      *                          * Per la classe merceologica          *
      *                          *-------------------------------------*
           move      w-sel-fir-sqz-cla    to   srt-k03-sqz-cla        .
           move      w-sel-fir-cod-cla    to   srt-k03-cod-cla        .
       cmp-srt-rec-435.
      *                          *-------------------------------------*
      *                          * Per il gruppo merceologico          *
      *                          *-------------------------------------*
           move      w-sel-fir-sqz-gru    to   srt-k03-sqz-gru        .
           move      w-sel-fir-cod-gru    to   srt-k03-cod-gru        .
       cmp-srt-rec-440.
      *                          *-------------------------------------*
      *                          * Per il sottogruppo merceologico     *
      *                          *-------------------------------------*
           move      w-sel-fir-sqz-sgr    to   srt-k03-sqz-sgr        .
           move      w-sel-fir-cod-sgr    to   srt-k03-cod-sgr        .
       cmp-srt-rec-445.
      *                      *-----------------------------------------*
      *                      * Descrizione per il prodotto             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento prodotto : *
      *                          *  - ABC decrescente sul fatturato    *
      *                          *  - ABC decrescente sulla quantita'  *
      *                          *  - ABC crescente sul fatturato      *
      *                          *  - ABC crescente sulla quantita'    *
      *                          *  - Per descrizione prodotto         *
      *                          *  - Per classe e descrizione prod.   *
      *                          * altrimenti forzato a spaces         *
      *                          *-------------------------------------*
           if        rr-tor-pro           not  = 01 and
                     rr-tor-pro           not  = 02 and
                     rr-tor-pro           not  = 03 and
                     rr-tor-pro           not  = 04 and
                     rr-tor-pro           not  = 06 and
                     rr-tor-pro           not  = 08
                     move  spaces         to   srt-k03-des-pro
                     go to cmp-srt-rec-450.
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fir-des-pro    to   srt-k03-des-pro        .
       cmp-srt-rec-450.
      *                      *-----------------------------------------*
      *                      * Flag di codice alfanumerico a spaces o  *
      *                      * diverso da spaces                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Solo se tipo ordinamento prodotto : *
      *                          *  - Per codice prodotto              *
      *                          *  - Per classe e codice prodotto     *
      *                          * altrimenti forzato a zero           *
      *                          *-------------------------------------*
           if        rr-tor-pro           not  = 05 and
                     rr-tor-pro           not  = 07
                     move  zero           to   srt-k03-flg-sod
                     go to cmp-srt-rec-455.
      *                          *-------------------------------------*
      *                          * Memorizzazione con test             *
      *                          *-------------------------------------*
           if        w-sel-fir-alf-pro    =    spaces
                     move  9              to   srt-k03-flg-sod
           else      move  1              to   srt-k03-flg-sod        .
       cmp-srt-rec-455.
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico prodotto            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fir-alf-pro    to   srt-k03-alf-pro        .
       cmp-srt-rec-460.
      *                      *-----------------------------------------*
      *                      * Codice numerico prodotto                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Memorizzazione                      *
      *                          *-------------------------------------*
           move      w-sel-fir-num-pro    to   srt-k03-num-pro        .
       cmp-srt-rec-500.
      *                  *---------------------------------------------*
      *                  * Subchiave 4 : Per documento                 *
      *                  *---------------------------------------------*
       cmp-srt-rec-510.
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-fit-dat-doc       to   srt-k04-dat-doc        .
       cmp-srt-rec-510.
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      rf-fit-cod-tmo       to   srt-k04-tip-doc        .
       cmp-srt-rec-515.
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-fit-num-doc       to   srt-k04-num-doc        .
       cmp-srt-rec-800.
      *              *-------------------------------------------------*
      *              * Area dati                                       *
      *              *-------------------------------------------------*
       cmp-srt-rec-810.
      *                  *---------------------------------------------*
      *                  * Codice classe geografica cliente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      w-sel-fit-cod-cgc    to   srt-cod-cgc-cod        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-sel-fit-des-cgc    to   srt-cod-cgc-des        .
      *                      *-----------------------------------------*
      *                      * Flag di esistenza                       *
      *                      *-----------------------------------------*
           move      w-sel-fit-flg-cgc    to   srt-cod-cgc-flg        .
       cmp-srt-rec-820.
      *                  *---------------------------------------------*
      *                  * Cliente                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-sel-fit-cod-cli    to   srt-cod-cli-cod        .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente           *
      *                      *-----------------------------------------*
           move      w-sel-fit-dpz-cli    to   srt-cod-cli-dpz        .
      *                      *-----------------------------------------*
      *                      * Mnemonico del cliente                   *
      *                      *-----------------------------------------*
           move      w-sel-fit-mne-cli    to   srt-cod-cli-mne        .
      *                      *-----------------------------------------*
      *                      * Ragione sociale del cliente             *
      *                      *-----------------------------------------*
           move      w-sel-fit-rag-cli    to   srt-cod-cli-rag        .
       cmp-srt-rec-830.
      *                  *---------------------------------------------*
      *                  * Classe merceologica                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      w-sel-fir-cod-cla    to   srt-cod-cla-cod        .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-sel-fir-mne-cla    to   srt-cod-cla-mne        .
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-sel-fir-sqz-cla    to   srt-cod-cla-sqz        .
      *                      *-----------------------------------------*
      *                      * Si/no ulteriore suddivisione            *
      *                      *-----------------------------------------*
           move      w-sel-fir-sud-cla    to   srt-cod-cla-sud        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-sel-fir-des-cla    to   srt-cod-cla-des        .
       cmp-srt-rec-840.
      *                  *---------------------------------------------*
      *                  * Gruppo merceologico                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      w-sel-fir-cod-gru    to   srt-cod-gru-cod        .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-sel-fir-mne-gru    to   srt-cod-gru-mne        .
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-sel-fir-sqz-gru    to   srt-cod-gru-sqz        .
      *                      *-----------------------------------------*
      *                      * Si/no ulteriore suddivisione            *
      *                      *-----------------------------------------*
           move      w-sel-fir-sud-gru    to   srt-cod-gru-sud        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-sel-fir-des-gru    to   srt-cod-gru-des        .
       cmp-srt-rec-850.
      *                  *---------------------------------------------*
      *                  * Sottogruppo merceologico                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      w-sel-fir-cod-sgr    to   srt-cod-sgr-cod        .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-sel-fir-mne-sgr    to   srt-cod-sgr-mne        .
      *                      *-----------------------------------------*
      *                      * Numero di sequenza                      *
      *                      *-----------------------------------------*
           move      w-sel-fir-sqz-sgr    to   srt-cod-sgr-sqz        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      w-sel-fir-des-sgr    to   srt-cod-sgr-des        .
       cmp-srt-rec-860.
      *                  *---------------------------------------------*
      *                  * Prodotto                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice numerico del prodotto            *
      *                      *-----------------------------------------*
           move      w-sel-fir-num-pro    to   srt-cod-pro-cod        .
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico del prodotto        *
      *                      *-----------------------------------------*
           move      w-sel-fir-alf-pro    to   srt-cod-pro-alf        .
      *                      *-----------------------------------------*
      *                      * Descrizione del prodotto                *
      *                      *-----------------------------------------*
           move      w-sel-fir-des-pro    to   srt-cod-pro-des        .
      *                      *-----------------------------------------*
      *                      * Unita' di misura del prodotto           *
      *                      *-----------------------------------------*
           move      w-sel-fir-umi-pro    to   srt-cod-pro-umi        .
      *                      *-----------------------------------------*
      *                      * Numero decimali del prodotto            *
      *                      *-----------------------------------------*
           move      w-sel-fir-dec-pro    to   srt-cod-pro-dec        .
       cmp-srt-rec-870.
      *                  *---------------------------------------------*
      *                  * Estremi del documento                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      rf-fit-cod-tmo       to   srt-est-doc-tdo        .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-fit-dat-doc       to   srt-est-doc-ddo        .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-fit-num-doc       to   srt-est-doc-ndo        .
      *                      *-----------------------------------------*
      *                      * Campo da esporre come 'Note' in riga di *
      *                      * dettaglio documento                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione richiamo del modulo    *
      *                          * 'mnotsvf0' per la funzione di de-   *
      *                          * terminazione del valore del campo   *
      *                          * 'Note'                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Tipo di determinazione : per il *
      *                              * dettaglio                       *
      *                              *---------------------------------*
           move      00                   to   w-mod-not-svf-tdd      .
      *                              *---------------------------------*
      *                              * Numero di caratteri disponibili *
      *                              *---------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                              *---------------------------------*
      *                              * 1. quantita'                    *
      *                              *---------------------------------*
           move      w-mod-fat-fir-qta    to   w-mod-not-svf-q01      .
      *                              *---------------------------------*
      *                              * 1. fatturato                    *
      *                              *---------------------------------*
           move      w-mod-fat-fir-val    to   w-mod-not-svf-f01      .
      *                              *---------------------------------*
      *                              * 2. quantita'                    *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-q02      .
      *                              *---------------------------------*
      *                              * 2. fatturato                    *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-f02      .
      *                              *---------------------------------*
      *                              * 1. % di variazione              *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-p01      .
      *                              *---------------------------------*
      *                              * 3. quantita'                    *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-q03      .
      *                              *---------------------------------*
      *                              * 3. fatturato                    *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-f03      .
      *                              *---------------------------------*
      *                              * 2. % di variazione              *
      *                              *---------------------------------*
           move      zero                 to   w-mod-not-svf-p02      .
      *                              *---------------------------------*
      *                              * Tipo documento                  *
      *                              *---------------------------------*
           move      rf-fit-tip-doc       to   w-mod-not-svf-tdo      .
      *                              *---------------------------------*
      *                              * Data documento                  *
      *                              *---------------------------------*
           move      rf-fit-dat-doc       to   w-mod-not-svf-ddo      .
      *                              *---------------------------------*
      *                              * Numero documento                *
      *                              *---------------------------------*
           move      rf-fit-num-doc       to   w-mod-not-svf-ndo      .
      *                          *-------------------------------------*
      *                          * Richiamo effettivo del modulo       *
      *                          *-------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
      *                          *-------------------------------------*
      *                          * Memorizzazione risultato            *
      *                          *-------------------------------------*
           move      w-mod-not-svf-not    to   srt-est-doc-not        .
       cmp-srt-rec-880.
      *                  *---------------------------------------------*
      *                  * Valori statistici                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' fatturata                     *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-qta    to   srt-val-sta-qta        .
      *                      *-----------------------------------------*
      *                      * Importo fatturato                       *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-val    to   srt-val-sta-val        .
      *                      *-----------------------------------------*
      *                      * Incidenza sulla statistica              *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-pon    to   srt-val-sta-pon        .
      *                      *-----------------------------------------*
      *                      * Incidenza sul 1. periodo di riferimento *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-sn1    to   srt-val-sta-sn1        .
      *                      *-----------------------------------------*
      *                      * Incidenza sul 2. periodo di riferimento *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-sn2    to   srt-val-sta-sn2        .
      *                      *-----------------------------------------*
      *                      * Incidenza sul 3. periodo di riferimento *
      *                      *-----------------------------------------*
           move      w-mod-fat-fir-sn3    to   srt-val-sta-sn3        .
       cmp-srt-rec-890.
      *                  *---------------------------------------------*
      *                  * Valori ABC                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore ABC per la classe geografica     *
      *                      *-----------------------------------------*
           move      zero                 to   srt-val-abc-cgc        .
      *                      *-----------------------------------------*
      *                      * Valore ABC per il cliente               *
      *                      *-----------------------------------------*
           move      zero                 to   srt-val-abc-cli        .
      *                      *-----------------------------------------*
      *                      * Valore ABC per il prodotto nell'ambito  *
      *                      * del cliente                             *
      *                      *-----------------------------------------*
           move      zero                 to   srt-val-abc-pro        .
       cmp-srt-rec-950.
      *                  *---------------------------------------------*
      *                  * Valori per selezione record                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag per record da non selezionare      *
      *                      *-----------------------------------------*
           move      spaces               to   srt-sel-rec-non        .
       cmp-srt-rec-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento valori ABC su file di appoggio ad indici,   *
      *    * basandosi sui contenuti di :                              *
      *    *                                                           *
      *    *   - area sort record [srt]                                *
      *    *   - area rr                                               *
      *    *-----------------------------------------------------------*
       agg-abc-kk0-000.
      *              *-------------------------------------------------*
      *              * Valore ABC per la classe geografica             *
      *              *-------------------------------------------------*
       agg-abc-kk0-025.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : aggiornamento ABC       *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-kk0-050.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento classe :           *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti nessuna azione                   *
      *                  *---------------------------------------------*
           if        rr-tor-cgc           not  = 01 and
                     rr-tor-cgc           not  = 02 and
                     rr-tor-cgc           not  = 03 and
                     rr-tor-cgc           not  = 04
                     go to agg-abc-kk0-200.
       agg-abc-kk0-050.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      01                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      zero                 to   kk0-num-pro            .
           read      kk0    invalid key
                            go to   agg-abc-kk0-075.
           go to     agg-abc-kk0-100.
       agg-abc-kk0-075.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           write     kk0-rec
                            invalid key
                            go to   agg-abc-kk0-050.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura record                *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-050.
       agg-abc-kk0-100.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento 1. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-1op
                           add      srt-val-sta-val
                                          to   kk0-fat-1op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-1op
                           subtract srt-val-sta-val
                                          from kk0-fat-1op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 2. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-2op
                           add      srt-val-sta-val
                                          to   kk0-fat-2op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-2op
                           subtract srt-val-sta-val
                                          from kk0-fat-2op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 3. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-3op
                           add      srt-val-sta-val
                                          to   kk0-fat-3op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-3op
                           subtract srt-val-sta-val
                                          from kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Riscrittura record file ad indici, se   *
      *                      * errori : a rilettura                    *
      *                      *-----------------------------------------*
           rewrite   kk0-rec
                            invalid key
                            go to   agg-abc-kk0-050.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-200.
       agg-abc-kk0-200.
      *              *-------------------------------------------------*
      *              * Valore ABC per il cliente e la sua dipendenza   *
      *              *-------------------------------------------------*
       agg-abc-kk0-225.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : aggiornamento ABC       *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-kk0-250.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento per clienti :      *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti nessuna azione                   *
      *                  *---------------------------------------------*
           if        rr-tor-cli           not  = 01 and
                     rr-tor-cli           not  = 02 and
                     rr-tor-cli           not  = 03 and
                     rr-tor-cli           not  = 04
                     go to agg-abc-kk0-400.
       agg-abc-kk0-250.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      02                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      srt-cod-cli-cod      to   kk0-cod-cli            .
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   kk0-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   kk0-dpz-cli            .
           move      zero                 to   kk0-num-pro            .
           read      kk0    invalid key
                            go to   agg-abc-kk0-275.
           go to     agg-abc-kk0-300.
       agg-abc-kk0-275.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           write     kk0-rec
                            invalid key
                            go to   agg-abc-kk0-250.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura record                *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-250.
       agg-abc-kk0-300.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento 1. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-1op
                           add      srt-val-sta-val
                                          to   kk0-fat-1op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-1op
                           subtract srt-val-sta-val
                                          from kk0-fat-1op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 2. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-2op
                           add      srt-val-sta-val
                                          to   kk0-fat-2op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-2op
                           subtract srt-val-sta-val
                                          from kk0-fat-2op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 3. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-3op
                           add      srt-val-sta-val
                                          to   kk0-fat-3op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-3op
                           subtract srt-val-sta-val
                                          from kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Riscrittura record file ad indici, se   *
      *                      * errori : a rilettura                    *
      *                      *-----------------------------------------*
           rewrite   kk0-rec
                            invalid key
                            go to   agg-abc-kk0-250.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-400.
       agg-abc-kk0-400.
      *              *-------------------------------------------------*
      *              * Valore ABC per codice prodotto                  *
      *              *-------------------------------------------------*
       agg-abc-kk0-425.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : aggiornamento ABC       *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-kk0-450.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento per prodotti :     *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti nessuna azione                   *
      *                  *---------------------------------------------*
           if        rr-tor-pro           not  = 01 and
                     rr-tor-pro           not  = 02 and
                     rr-tor-pro           not  = 03 and
                     rr-tor-pro           not  = 04
                     go to agg-abc-kk0-999.
       agg-abc-kk0-450.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      03                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      srt-cod-cli-cod      to   kk0-cod-cli            .
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   kk0-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   kk0-dpz-cli            .
           move      srt-cod-pro-cod      to   kk0-num-pro            .
           read      kk0    invalid key
                            go to   agg-abc-kk0-475.
           go to     agg-abc-kk0-500.
       agg-abc-kk0-475.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Scrittura record normalizzato           *
      *                      *-----------------------------------------*
           write     kk0-rec
                            invalid key
                            go to   agg-abc-kk0-450.
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura record                *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-450.
       agg-abc-kk0-500.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento 1. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-1op
                           add      srt-val-sta-val
                                          to   kk0-fat-1op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-1op
                           subtract srt-val-sta-val
                                          from kk0-fat-1op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 2. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-2op
                           add      srt-val-sta-val
                                          to   kk0-fat-2op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-2op
                           subtract srt-val-sta-val
                                          from kk0-fat-2op            .
      *                      *-----------------------------------------*
      *                      * Aggiornamento 3. periodo in positivo o  *
      *                      * in negativo                             *
      *                      *-----------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   kk0-qta-3op
                           add      srt-val-sta-val
                                          to   kk0-fat-3op
                     else  subtract srt-val-sta-qta
                                          from kk0-qta-3op
                           subtract srt-val-sta-val
                                          from kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Riscrittura record file ad indici, se   *
      *                      * errori : a rilettura                    *
      *                      *-----------------------------------------*
           rewrite   kk0-rec
                            invalid key
                            go to   agg-abc-kk0-450.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-abc-kk0-999.
       agg-abc-kk0-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento valori ABC su file di appoggio sequenziale  *
      *    * basandosi sui contenuti di :                              *
      *    *                                                           *
      *    *   - area record [kk0]                                     *
      *    *   - area rr                                               *
      *    *                                                           *
      *    * Aggiornamento del flag per record da non selezionare in   *
      *    * caso di statistica sul nuovo fatturato o sul non fattu-   *
      *    * rato                                                      *
      *    *-----------------------------------------------------------*
       agg-abc-sqz-000.
      *              *-------------------------------------------------*
      *              * Valore ABC per la classe geografica             *
      *              *-------------------------------------------------*
       agg-abc-sqz-025.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : a lettura ABC           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-sqz-050.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento per classe :       *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti forzatura valore a zero          *
      *                  *---------------------------------------------*
           if        rr-tor-cgc           not  = 01 and
                     rr-tor-cgc           not  = 02 and
                     rr-tor-cgc           not  = 03 and
                     rr-tor-cgc           not  = 04
                     go to agg-abc-sqz-200.
       agg-abc-sqz-050.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      01                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      zero                 to   kk0-num-pro            .
           read      kk0    with no lock
                            invalid key
                            go to   agg-abc-sqz-075.
           go to     agg-abc-sqz-100.
       agg-abc-sqz-075.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Continuazione come se esistente         *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-100.
       agg-abc-sqz-100.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
       agg-abc-sqz-125.
      *                      *-----------------------------------------*
      *                      * Aggiornamento valore ABC in funzione    *
      *                      * del numero di periodi di riferimento    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non ABC : nessuna azione         *
      *                          *-------------------------------------*
           if        rr-tor-cgc           not  = 01 and
                     rr-tor-cgc           not  = 02 and
                     rr-tor-cgc           not  = 03 and
                     rr-tor-cgc           not  = 04
                     go to agg-abc-sqz-150.
      *                          *-------------------------------------*
      *                          * Preparazione valore ABC             *
      *                          *-------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    11
                     if    rr-tor-cgc     =    02 or
                           rr-tor-cgc     =    04
                           move   kk0-qta-2op
                                          to   srt-k01-val-abc
                     else  move   kk0-fat-2op
                                          to   srt-k01-val-abc
           else if   rr-num-pdr           =    03
                     if    rr-tor-cgc     =    02 or
                           rr-tor-cgc     =    04
                           move   kk0-qta-3op
                                          to   srt-k01-val-abc
                     else  move   kk0-fat-3op
                                          to   srt-k01-val-abc
           else      if    rr-tor-cgc     =    02 or
                           rr-tor-cgc     =    04
                           move   kk0-qta-1op
                                          to   srt-k01-val-abc
                     else  move   kk0-fat-1op
                                          to   srt-k01-val-abc        .
      *                          *-------------------------------------*
      *                          * Se ABC decrescente si complementa   *
      *                          *-------------------------------------*
           if        rr-tor-cgc           =    01 or
                     rr-tor-cgc           =    02
                     multiply -1          by   srt-k01-val-abc
                     add       9999999999999,999
                                          to   srt-k01-val-abc        .
      *                          *-------------------------------------*
      *                          * Memorizzazione in area specifica    *
      *                          *-------------------------------------*
           move      srt-k01-val-abc      to   srt-val-abc-cgc        .
       agg-abc-sqz-150.
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento del flag per    *
      *                      * record da non selezionare               *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    11     and
                     kk0-qta-1oP          not  = zero and
                     kk0-fat-1op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
           if        rr-num-pdr           =    12     and
                     kk0-qta-2op          not  = zero and
                     kk0-fat-2op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
       agg-abc-sqz-175.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-200.
       agg-abc-sqz-200.
      *              *-------------------------------------------------*
      *              * Valore ABC per il cliente e la sua dipendenza   *
      *              *-------------------------------------------------*
       agg-abc-sqz-225.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : a lettura ABC           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-sqz-250.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento per clienti :      *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti forzatura valore a zero          *
      *                  *---------------------------------------------*
           if        rr-tor-cli           not  = 01 and
                     rr-tor-cli           not  = 02 and
                     rr-tor-cli           not  = 03 and
                     rr-tor-cli           not  = 04
                     go to agg-abc-sqz-400.
       agg-abc-sqz-250.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      02                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      srt-cod-cli-cod      to   kk0-cod-cli            .
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   kk0-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   kk0-dpz-cli            .
           move      zero                 to   kk0-num-pro            .
           read      kk0    with no lock
                            invalid key
                            go to   agg-abc-sqz-275.
           go to     agg-abc-sqz-300.
       agg-abc-sqz-275.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Continuazione come se esistente         *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-300.
       agg-abc-sqz-300.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
       agg-abc-sqz-325.
      *                      *-----------------------------------------*
      *                      * Aggiornamento valore ABC in funzione    *
      *                      * del numero di periodi di riferimento    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non ABC : nessuna azione         *
      *                          *-------------------------------------*
           if        rr-tor-cli           not  = 01 and
                     rr-tor-cli           not  = 02 and
                     rr-tor-cli           not  = 03 and
                     rr-tor-cli           not  = 04
                     go to agg-abc-sqz-350.
      *                          *-------------------------------------*
      *                          * Preparazione valore ABC             *
      *                          *-------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    11
                     if    rr-tor-cli     =    02 or
                           rr-tor-cli     =    04
                           move   kk0-qta-2op
                                          to   srt-k02-val-abc
                     else  move   kk0-fat-2op
                                          to   srt-k02-val-abc
           else if   rr-num-pdr           =    03
                     if    rr-tor-cli     =    02 or
                           rr-tor-cli     =    04
                           move   kk0-qta-3op
                                          to   srt-k02-val-abc
                     else  move   kk0-fat-3op
                                          to   srt-k02-val-abc
           else      if    rr-tor-cli     =    02 or
                           rr-tor-cli     =    04
                           move   kk0-qta-1op
                                          to   srt-k02-val-abc
                     else  move   kk0-fat-1op
                                          to   srt-k02-val-abc        .
      *                          *-------------------------------------*
      *                          * Se ABC decrescente si complementa   *
      *                          *-------------------------------------*
           if        rr-tor-cli           =    01 or
                     rr-tor-cli           =    02
                     multiply -1          by   srt-k02-val-abc
                     add       9999999999999,999
                                          to   srt-k02-val-abc        .
      *                          *-------------------------------------*
      *                          * Memorizzazione in area specifica    *
      *                          *-------------------------------------*
           move      srt-k01-val-abc      to   srt-val-abc-cli        .
       agg-abc-sqz-350.
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento del flag per    *
      *                      * record da non selezionare               *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    11     and
                     kk0-qta-1oP          not  = zero and
                     kk0-fat-1op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
           if        rr-num-pdr           =    12     and
                     kk0-qta-2op          not  = zero and
                     kk0-fat-2op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
       agg-abc-sqz-375.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-400.
       agg-abc-sqz-400.
      *              *-------------------------------------------------*
      *              * Valore ABC per il prodotto                      *
      *              *-------------------------------------------------*
       agg-abc-sqz-425.
      *                  *---------------------------------------------*
      *                  * Se la statistica e' sul nuovo fatturato o   *
      *                  * sul non fatturato : a lettura ABC           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to agg-abc-sqz-450.
      *                  *---------------------------------------------*
      *                  * Solo se tipo ordinamento per prodotti :     *
      *                  *   - 01 : ABC decrescente sul fatturato      *
      *                  *   - 02 : ABC decrescente sulla quantita'    *
      *                  *   - 03 : ABC crescente sul fatturato        *
      *                  *   - 04 : ABC crescente sulla quantita'      *
      *                  * Altrimenti nessuna azione                   *
      *                  *---------------------------------------------*
           if        rr-tor-pro           not  = 01 and
                     rr-tor-pro           not  = 02 and
                     rr-tor-pro           not  = 03 and
                     rr-tor-pro           not  = 04
                     go to agg-abc-sqz-999.
       agg-abc-sqz-450.
      *                  *---------------------------------------------*
      *                  * Lettura record file ad indici               *
      *                  *---------------------------------------------*
           move      03                   to   kk0-tip-rec            .
           move      srt-cod-cgc-cod      to   kk0-cod-cgc            .
           move      srt-cod-cli-cod      to   kk0-cod-cli            .
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   kk0-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   kk0-dpz-cli            .
           move      srt-cod-pro-cod      to   kk0-num-pro            .
           read      kk0    with no lock
                            invalid key
                            go to   agg-abc-sqz-475.
           go to     agg-abc-sqz-500.
       agg-abc-sqz-475.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici non esistente      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record area dati        *
      *                      *-----------------------------------------*
           move      zero                 to   kk0-qta-1op            .
           move      zero                 to   kk0-fat-1op            .
           move      zero                 to   kk0-qta-2op            .
           move      zero                 to   kk0-fat-2op            .
           move      zero                 to   kk0-qta-3op            .
           move      zero                 to   kk0-fat-3op            .
      *                      *-----------------------------------------*
      *                      * Continuazione come se esistente         *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-500.
       agg-abc-sqz-500.
      *                  *---------------------------------------------*
      *                  * Se record file ad indici esistente          *
      *                  *---------------------------------------------*
       agg-abc-sqz-525.
      *                      *-----------------------------------------*
      *                      * Aggiornamento valore ABC in funzione    *
      *                      * del numero di periodi di riferimento    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non ABC : nessuna azione         *
      *                          *-------------------------------------*
           if        rr-tor-pro           not  = 01 and
                     rr-tor-pro           not  = 02 and
                     rr-tor-pro           not  = 03 and
                     rr-tor-pro           not  = 04
                     go to agg-abc-sqz-550.
      *                          *-------------------------------------*
      *                          * Preparazione valore ABC             *
      *                          *-------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    11
                     if    rr-tor-pro     =    02 or
                           rr-tor-pro     =    04
                           move   kk0-qta-2op
                                          to   srt-k03-val-abc
                     else  move   kk0-fat-2op
                                          to   srt-k03-val-abc
           else if   rr-num-pdr           =    03
                     if    rr-tor-pro     =    02 or
                           rr-tor-pro     =    04
                           move   kk0-qta-3op
                                          to   srt-k03-val-abc
                     else  move   kk0-fat-3op
                                          to   srt-k03-val-abc
           else      if    rr-tor-pro     =    02 or
                           rr-tor-pro     =    04
                           move   kk0-qta-1op
                                          to   srt-k03-val-abc
                     else  move   kk0-fat-1op
                                          to   srt-k03-val-abc        .
      *                          *-------------------------------------*
      *                          * Se ABC decrescente si complementa   *
      *                          *-------------------------------------*
           if        rr-tor-pro           =    01 or
                     rr-tor-pro           =    02
                     multiply -1          by   srt-k03-val-abc
                     add       9999999999999,999
                                          to   srt-k03-val-abc        .
      *                          *-------------------------------------*
      *                          * Memorizzazione in area specifica    *
      *                          *-------------------------------------*
           move      srt-k02-val-abc      to   srt-val-abc-pro        .
       agg-abc-sqz-550.
      *                      *-----------------------------------------*
      *                      * Eventuale aggiornamento del flag per    *
      *                      * record da non selezionare               *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    11     and
                     kk0-qta-1oP          not  = zero and
                     kk0-fat-1op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
           if        rr-num-pdr           =    12     and
                     kk0-qta-2op          not  = zero and
                     kk0-fat-2op          not  = zero
                     move  "N"            to   srt-sel-rec-non        .
       agg-abc-sqz-575.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-abc-sqz-999.
       agg-abc-sqz-999.
           exit.

      *    *===========================================================*
      *    * Open file di appoggio sequenziale [sqz]                   *
      *    *-----------------------------------------------------------*
       opn-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se il pathname per il file *
      *              * [sqz] e' gia' stato determinato oppure no       *
      *              *-------------------------------------------------*
           if        f-sqz-pat            =    spaces
                     go to opn-fil-sqz-300
           else      go to opn-fil-sqz-600.
       opn-fil-sqz-300.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [sqz] non e' ancora  *
      *              * stato determinato                               *
      *              *-------------------------------------------------*
       opn-fil-sqz-325.
      *                  *---------------------------------------------*
      *                  * Determinazione pathname per file [sqz]      *
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
           move      s-pat                to   f-sqz-pat              .
       opn-fil-sqz-350.
      *                  *---------------------------------------------*
      *                  * Open file [sqz] in output                   *
      *                  *---------------------------------------------*
           open      output sqz                                       .
       opn-fil-sqz-375.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-sqz-999.
       opn-fil-sqz-600.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [sqz] e' gia' stato  *
      *              * determinato                                     *
      *              *-------------------------------------------------*
       opn-fil-sqz-625.
      *                  *---------------------------------------------*
      *                  * Open file [sqz] in input                    *
      *                  *---------------------------------------------*
           open      input  sqz                                       .
       opn-fil-sqz-650.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-sqz-999.
       opn-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Close file di appoggio sequenziale [sqz]                  *
      *    *-----------------------------------------------------------*
       cls-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     sqz                                              .
       cls-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Write file di appoggio sequenziale [sqz]                  *
      *    *-----------------------------------------------------------*
       wrt-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           write     sqz-rec                                          .
       wrt-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Read file di appoggio sequenziale [sqz]                   *
      *    *-----------------------------------------------------------*
       rea-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Read                                            *
      *              *-------------------------------------------------*
           read      sqz    at end
                            move  e-end-fil
                                          to   f-sqz-sts
                            go to rea-fil-sqz-999.
           move      e-not-err            to   f-sqz-sts              .
       rea-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Delete file di appoggio sequenziale [sqz]                 *
      *    *-----------------------------------------------------------*
       del-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione da segreteria                 *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-sqz-pat            to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Open file di appoggio ad indici [kk0]                     *
      *    *-----------------------------------------------------------*
       opn-fil-kk0-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se il pathname per il file *
      *              * [kk0] e' gia' stato determinato oppure no       *
      *              *-------------------------------------------------*
           if        f-kk0-pat            =    spaces
                     go to opn-fil-kk0-300
           else      go to opn-fil-kk0-600.
       opn-fil-kk0-300.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [kk0] non e' ancora  *
      *              * stato determinato                               *
      *              *-------------------------------------------------*
       opn-fil-kk0-325.
      *                  *---------------------------------------------*
      *                  * Determinazione pathname per file [kk0]      *
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
           move      s-pat                to   f-kk0-pat              .
       opn-fil-kk0-350.
      *                  *---------------------------------------------*
      *                  * Open file [kk0] in i-o                      *
      *                  *---------------------------------------------*
           open      i-o    kk0                                       .
       opn-fil-kk0-375.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-kk0-999.
       opn-fil-kk0-600.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [kk0] e' gia' stato  *
      *              * determinato                                     *
      *              *-------------------------------------------------*
       opn-fil-kk0-625.
      *                  *---------------------------------------------*
      *                  * Open file [kk0] in i-o                      *
      *                  *---------------------------------------------*
           open      i-o    kk0                                       .
       opn-fil-kk0-650.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-kk0-999.
       opn-fil-kk0-999.
           exit.

      *    *===========================================================*
      *    * Close file di appoggio ad indici [kk0]                    *
      *    *-----------------------------------------------------------*
       cls-fil-kk0-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     kk0                                              .
       cls-fil-kk0-999.
           exit.

      *    *===========================================================*
      *    * Delete file di appoggio ad indici [kk0]                   *
      *    *-----------------------------------------------------------*
       del-fil-kk0-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione da segreteria                 *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-kk0-pat            to   s-pat                  .
           move      "K"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-fil-kk0-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun documento entro i limiti assegnati !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            move  "#"     to   w-cnt-prn-flg-sub
                            go to prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Se il flag per record da non selezionare e' in  *
      *              * status On : uscita per selezione mancata        *
      *              *-------------------------------------------------*
           if        srt-sel-rec-non      not  = spaces
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-sel-rec-999.
       prn-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Se la statistica e' sul fatturato nuovo, poi-   *
      *              * che' deve essere stampato un solo periodo, il   *
      *              * 2. periodo, si inverte l'incidenza sulla sta-   *
      *              * tistica del 1. con quella del 2. periodo        *
      *              *-------------------------------------------------*
           if        rr-num-pdr           =    11
                     move  srt-val-sta-sn1
                                          to   srt-val-sta-sn3
                     move  srt-val-sta-sn2
                                          to   srt-val-sta-sn1
                     move  srt-val-sta-sn3
                                          to   srt-val-sta-sn2
                     move  "N"            to   srt-val-sta-sn3        .
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
      *              *-------------------------------------------------*
      *              * 1. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico prodotto                    *
      *                  *---------------------------------------------*
           move      srt-cod-pro-cod      to   w-rot-l01-cod-pro      .
       prn-cmp-rot-100.
      *              *-------------------------------------------------*
      *              * 2. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico sottogruppo merceologico    *
      *                  *---------------------------------------------*
           if        rr-tor-pro           =    07 or
                     rr-tor-pro           =    08
                     move  srt-cod-sgr-cod
                                          to   w-rot-l02-cod-sgr
           else      move  zero           to   w-rot-l02-cod-sgr      .
       prn-cmp-rot-200.
      *              *-------------------------------------------------*
      *              * 3. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico gruppo merceologico         *
      *                  *---------------------------------------------*
           if        rr-tor-pro           =    07 or
                     rr-tor-pro           =    08
                     move  srt-cod-gru-cod
                                          to   w-rot-l03-cod-gru
           else      move  zero           to   w-rot-l03-cod-gru      .
       prn-cmp-rot-300.
      *              *-------------------------------------------------*
      *              * 4. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico classe merceologica         *
      *                  *---------------------------------------------*
           if        rr-tor-pro           =    07 or
                     rr-tor-pro           =    08 or
                     rr-tor-pro           =    09
                     move  srt-cod-cla-cod
                                          to   w-rot-l04-cod-cla
           else      move  zero           to   w-rot-l04-cod-cla      .
       prn-cmp-rot-400.
      *              *-------------------------------------------------*
      *              * 5. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      srt-cod-cli-cod      to   w-rot-l05-cod-cli      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   w-rot-l05-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   w-rot-l05-dpz-cli      .
       prn-cmp-rot-500.
      *              *-------------------------------------------------*
      *              * 6. livello di rottura                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice classe geografica cliente            *
      *                  *---------------------------------------------*
           move      srt-cod-cgc-cod      to   w-rot-l06-cod-cgc      .
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-cic-030.
      *              *-------------------------------------------------*
      *              * Test se emissione archivio sequenziale          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rr-gen-fil           not  = "S"
                     go to prn-ini-cic-050.
      *                  *---------------------------------------------*
      *                  * Personalizzazione per salto pagina          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-prs-svf-spr-slp      .
      *                  *---------------------------------------------*
      *                  * Subroutine di inizializzazione              *
      *                  *---------------------------------------------*
           perform   prn-ini-cic-gen-000  thru prn-ini-cic-gen-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     prn-ini-cic-999.
       prn-ini-cic-050.
      *              *-------------------------------------------------*
      *              * Determinazione lunghezza e posizione di stampa  *
      *              * per il campo 'Note'                             *
      *              *-------------------------------------------------*
           perform   stp-clc-not-000      thru stp-clc-not-999        .
       prn-ini-cic-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura generale                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gen-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gen-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gen-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per il totale ge-  *
      *                  * nerale                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-num-ele      .
      *                  *---------------------------------------------*
      *                  * Flag di fine ciclo in esecuzione : No       *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gen-flg-end      .
       prn-ini-cic-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se le personalizzazioni    *
      *              * indicano un salto pagina oppure no ad inizio    *
      *              * rotture fondamentali                            *
      *              *-------------------------------------------------*
           if        w-prs-svf-spr-slp    =    "S"
                     go to prn-ini-cic-400.
       prn-ini-cic-300.
      *              *-------------------------------------------------*
      *              * Se 'No' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-cic-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-ini-cic-999.
       prn-ini-cic-400.
      *              *-------------------------------------------------*
      *              * Se 'Si' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-ini-cic-999.
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *                                                           *
      *    * Subroutine per inizio emissione file sequenziale          *
      *    *-----------------------------------------------------------*
       prn-ini-cic-gen-000.
      *              *-------------------------------------------------*
      *              * Open file di export                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   x-ope                  .
           move      spaces               to   x-pat                  .
           move      spaces               to   x-nam                  .
           move      02                   to   x-tex                  .
           move      ";"                  to   x-sep                  .
           call      "swd/mod/prg/obj/mxport"
                                        using  x                      .
           move      x-pat                to   w-det-pth-fso-pat      .
       prn-ini-cic-gen-200.
      *              *-------------------------------------------------*
      *              * Intestazioni colonne                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione per la classe geografica        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      17                   to   x-car                  .
           move      "CLASSE GEOGRAFICA"  to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale cliente                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      15                   to   x-car                  .
           move      "RAGIONE SOCIALE"    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Classe prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      06                   to   x-car                  .
           move      "CLASSE"             to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Gruppo prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      06                   to   x-car                  .
           move      "GRUPPO"             to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Sottogruppo prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      11                   to   x-car                  .
           move      "SOTTOGRUPPO"        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      08                   to   x-car                  .
           move      "PRODOTTO"           to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione per il prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      11                   to   x-car                  .
           move      "DESCRIZIONE PRODOTTO"
                                          to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura per il prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      03                   to   x-car                  .
           move      "UDM"                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-ini-cic-gen-200.
      *              *-------------------------------------------------*
      *              * Se con elenco documenti                         *
      *              *-------------------------------------------------*
       prn-ini-cic-gen-400.
      *              *-------------------------------------------------*
      *              * Fatturati                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' 1. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      09                   to   x-car                  .
           move      "QUANTITA'"          to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Fatturato 1. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      13                   to   x-car                  .
           move      "FATTURATO -1P"      to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Test su numero periodi                      *
      *                  *---------------------------------------------*
           if        rr-num-pdr           <    2
                     go to prn-ini-cic-gen-800.
      *                  *---------------------------------------------*
      *                  * Quantita' 2. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      09                   to   x-car                  .
           move      "QUANTITA'"          to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Fatturato 2. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      13                   to   x-car                  .
           move      "FATTURATO -2P"      to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Test su numero periodi                      *
      *                  *---------------------------------------------*
           if        rr-num-pdr           <    3
                     go to prn-ini-cic-gen-800.
      *                  *---------------------------------------------*
      *                  * Quantita' 3. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      09                   to   x-car                  .
           move      "QUANTITA'"          to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Fatturato 3. periodo                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      13                   to   x-car                  .
           move      "FATTURATO -3P"      to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-ini-cic-gen-800.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
           move      "PR"                 to   x-ope                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-ini-cic-gen-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-ini-cic-gen-999.
       prn-ini-cic-gen-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-025.
      *              *-------------------------------------------------*
      *              * Flag di fine ciclo in esecuzione : Si           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-liv-gen-flg-end      .
       prn-fin-cic-030.
      *              *-------------------------------------------------*
      *              * Test se emissione archivio sequenziale          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rr-gen-fil           not  = "S"
                     go to prn-fin-cic-050.
      *                  *---------------------------------------------*
      *                  * Close file di export                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   x-ope                  .
           move      w-det-pth-fso-pat    to   x-pat                  .
           call      "swd/mod/prg/obj/mxport"
                                        using  x                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo export                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mxport"                         .
      *                  *---------------------------------------------*
      *                  * Eventuali messaggi di dati elaborati        *
      *                  *---------------------------------------------*
           if        x-msg                =    spaces
                     go to prn-fin-cic-999.
           move      "WR"                 to   m-ope                  .
           move      x-ms1                to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
           if        x-ms2                =    spaces
                     go to prn-fin-cic-999.
           move      "WR"                 to   m-ope                  .
           move      x-ms2                to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-cic-999.
       prn-fin-cic-050.
      *              *-------------------------------------------------*
      *              * Se non sono state trattati almeno due elementi  *
      *              * : uscita senza alcuna azione                    *
      *              *-------------------------------------------------*
           if        w-liv-gen-num-ele    not  > 1
                     go to prn-fin-cic-999.
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-gen-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-gen-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-gen-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-gen-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-gen-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-gen-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-cic-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale generale                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * generale                                *
      *                      *-----------------------------------------*
           move      90                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gen-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-cic-150.
      *              *-------------------------------------------------*
      *              * Preparazione stampa totali generali             *
      *              *-------------------------------------------------*
           move      "Totale generale                                   
      -              "      "             to   w-stp-sta-tot-lit      .
           move      spaces               to   w-stp-sta-tot-ldc      .
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-gen-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-gen-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-gen-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-gen-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-gen-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-gen-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-gen-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-gen-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-gen-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-cic-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se le personalizzazioni    *
      *              * indicano un salto pagina oppure no ad inizio    *
      *              * rotture fondamentali                            *
      *              *-------------------------------------------------*
           if        w-prs-svf-spr-slp    =    "S"
                     go to prn-fin-cic-400.
       prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Se 'No' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
       prn-fin-cic-301.
      *                  *---------------------------------------------*
      *                  * Test se linee residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    2
                     go to prn-fin-cic-302.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
      *                  *---------------------------------------------*
      *                  * Fincatura verticale della pagina            *
      *                  *---------------------------------------------*
           perform   stp-fnc-vrt-000      thru stp-fnc-vrt-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-302.
      *                  *---------------------------------------------*
      *                  * Interlinea di separazione                   *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-303.
      *                  *---------------------------------------------*
      *                  * Esecuzione stampa totali generali           *
      *                  *---------------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-304.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-cic-900.
       prn-fin-cic-400.
      *              *-------------------------------------------------*
      *              * Se 'Si' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
       prn-fin-cic-401.
      *                  *---------------------------------------------*
      *                  * Numero pagina da stampare a zero se neces-  *
      *                  * sario                                       *
      *                  *---------------------------------------------*
           if        w-prs-svf-spr-rdu    =    "S"
                     move  zero           to   w-stp-int-num-pag      .
       prn-fin-cic-402.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-403.
      *                  *---------------------------------------------*
      *                  * Fincatura verticale della pagina            *
      *                  *---------------------------------------------*
           perform   stp-fnc-vrt-000      thru stp-fnc-vrt-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-404.
      *                  *---------------------------------------------*
      *                  * Esecuzione stampa totali generali           *
      *                  *---------------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-405.
      *                  *---------------------------------------------*
      *                  * Fincatura di sopralineatura totali          *
      *                  *---------------------------------------------*
           perform   stp-fds-tot-000      thru stp-fds-tot-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-cic-999.
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 7. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr7-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr7-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 7. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr7-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr7-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 6. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr6-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr6-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per codice classe geografica cliente       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice classe geografica                    *
      *                  *---------------------------------------------*
           move      srt-cod-cgc-cod      to   w-liv-cgc-cod-cgc      .
      *                  *---------------------------------------------*
      *                  * Descrizione per la classe geografica        *
      *                  *---------------------------------------------*
           move      srt-cod-cgc-des      to   w-liv-cgc-des-cgc      .
      *                  *---------------------------------------------*
      *                  * Flag di esistenza per la classe geografica  *
      *                  *---------------------------------------------*
           move      srt-cod-cgc-flg      to   w-liv-cgc-flg-cgc      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cgc-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cgc-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cgc-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero clienti trattati per la classe       *
      *                  * geografica                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cgc-num-cli      .
       prn-ini-lr6-150.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per il totale ge-  *
      *                  * nerale                                      *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-gen-num-ele      .
       prn-ini-lr6-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se le personalizzazioni    *
      *              * indicano un salto pagina oppure no ad inizio    *
      *              * rotture fondamentali                            *
      *              *-------------------------------------------------*
           if        w-prs-svf-spr-slp    =    "S"
                     go to prn-ini-lr6-400.
       prn-ini-lr6-300.
      *              *-------------------------------------------------*
      *              * Se 'No' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Continuazione comune                        *
      *                  *---------------------------------------------*
           go to     prn-ini-lr6-500.
       prn-ini-lr6-400.
      *              *-------------------------------------------------*
      *              * Se 'Si' salto pagina ad inizio rotture fonda-   *
      *              * mentali                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero pagina da stampare a zero se neces-  *
      *                  * sario                                       *
      *                  *---------------------------------------------*
           if        w-prs-svf-spr-rdu    =    "S"
                     move  zero           to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-lr6-999.
      *                  *---------------------------------------------*
      *                  * Continuazione comune                        *
      *                  *---------------------------------------------*
           go to     prn-ini-lr6-500.
       prn-ini-lr6-500.
      *              *-------------------------------------------------*
      *              * In ogni caso, sia 'Si' che 'No' salto pagina ad *
      *              * inizio rotture fondamentali                     *
      *              *-------------------------------------------------*
       prn-ini-lr6-525.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per subroutine di    *
      *                  * sub-intestazione                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore del codice, numerico             *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-sub-cod-num      .
      *                      *-----------------------------------------*
      *                      * Valore del codice, alfanumerico         *
      *                      *-----------------------------------------*
           move      w-liv-cgc-cod-cgc    to   w-stp-sub-cod-alf      .
      *                      *-----------------------------------------*
      *                      * Descrizione per il codice               *
      *                      *-----------------------------------------*
           move      w-liv-cgc-des-cgc    to   w-stp-sub-cod-des      .
       prn-ini-lr6-550.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di sub-intestazione *
      *                      *-----------------------------------------*
           perform   stp-sub-int-000      thru stp-sub-int-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata: uscita *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-lr6-999.
       prn-ini-lr6-600.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     prn-ini-lr6-999.
       prn-ini-lr6-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 6. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr6-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr6-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-cgc-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cgc-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cgc-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-cgc-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cgc-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cgc-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr6-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale classe geografica                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * classe geografica                       *
      *                      *-----------------------------------------*
           move      20                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cgc-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr6-400.
      *              *-------------------------------------------------*
      *              * Stampa totali                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to prn-fin-lr6-999.
      *                  *---------------------------------------------*
      *                  * Subroutine per la fincatura di sopralinea-  *
      *                  * tura totali                                 *
      *                  *---------------------------------------------*
           perform   stp-fds-tot-000      thru stp-fds-tot-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr6-999.
       prn-fin-lr6-420.
      *                  *---------------------------------------------*
      *                  * Stampa totale classe geografica             *
      *                  *---------------------------------------------*
       prn-fin-lr6-430.
      *                      *-----------------------------------------*
      *                      * Preparazione literal per il totale      *
      *                      *-----------------------------------------*
           if        rr-tcg-cli           =    01
                     go to prn-fin-lr6-431
           else if   rr-tcg-cli           =    02
                     go to prn-fin-lr6-432
           else if   rr-tcg-cli           =    03
                     go to prn-fin-lr6-433.
       prn-fin-lr6-431.
           move      "Totale nazione                                    
      -              "      "             to   w-stp-sta-tot-lit      .
           go to     prn-fin-lr6-440.
       prn-fin-lr6-432.
           move      "Totale regione                                    
      -              "      "             to   w-stp-sta-tot-lit      .
           go to     prn-fin-lr6-440.
       prn-fin-lr6-433.
           move      "Totale provincia                                  
      -              "      "             to   w-stp-sta-tot-lit      .
           go to     prn-fin-lr6-440.
       prn-fin-lr6-440.
      *                      *-----------------------------------------*
      *                      * Preparazione altri parametri            *
      *                      *-----------------------------------------*
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-cgc-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-cgc-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-cgc-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-cgc-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-cgc-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-cgc-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-cgc-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-cgc-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-cgc-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-lr6-450.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di stampa totali    *
      *                      *-----------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
       prn-fin-lr6-460.
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr6-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr6-999.
       prn-fin-lr6-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per codice cliente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      srt-cod-cli-cod      to   w-liv-cli-cod-cli      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           if        w-prs-svf-cli-rgd    =    "S"
                     move  spaces         to   w-liv-cli-dpz-cli
           else      move  srt-cod-cli-dpz
                                          to   w-liv-cli-dpz-cli      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale del cliente                 *
      *                  *---------------------------------------------*
           move      srt-cod-cli-rag      to   w-liv-cli-rag-cli      .
      *                  *---------------------------------------------*
      *                  * Mnemonico del cliente                       *
      *                  *---------------------------------------------*
           move      srt-cod-cli-mne      to   w-liv-cli-mne-cli      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cli-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cli-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cli-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per il cliente     *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cli-num-ele      .
       prn-ini-lr5-200.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di clienti trattati per la classe    *
      *                  * geografica cliente                          *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-cgc-num-cli      .
       prn-ini-lr5-500.
      *              *-------------------------------------------------*
      *              * Stampa vera e propria                           *
      *              *-------------------------------------------------*
       prn-ini-lr5-525.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per subroutine di    *
      *                  * sub-intestazione                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo codice da stampare                 *
      *                      *-----------------------------------------*
           if        w-prs-svf-cli-tco    =    "M"
                     move  "A"            to   w-stp-sub-tip-cod
           else      move  "N"            to   w-stp-sub-tip-cod      .
      *                      *-----------------------------------------*
      *                      * Valore del codice, numerico             *
      *                      *-----------------------------------------*
           move      w-liv-cli-cod-cli    to   w-stp-sub-cod-num      .
      *                      *-----------------------------------------*
      *                      * Valore del codice, alfanumerico         *
      *                      *-----------------------------------------*
           move      w-liv-cli-mne-cli    to   w-stp-sub-cod-alf      .
      *                      *-----------------------------------------*
      *                      * Valore appendice al codice              *
      *                      *-----------------------------------------*
           move      w-liv-cli-dpz-cli    to   w-stp-sub-app-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione per il codice               *
      *                      *-----------------------------------------*
           move      w-liv-cli-rag-cli    to   w-stp-sub-cod-des      .
       prn-ini-lr5-550.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di sub-sub-intesta- *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           perform   stp-ssi-int-000      thru stp-ssi-int-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata: uscita *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-lr5-999.
       prn-ini-lr5-575.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di fincatura verti- *
      *                      * cale della pagina                       *
      *                      *-----------------------------------------*
           perform   stp-fnc-vrt-000      thru stp-fnc-vrt-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata: uscita *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-lr5-999.
       prn-ini-lr5-600.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     prn-ini-lr5-999.
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-cli-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cli-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cli-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-cli-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cli-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cli-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr5-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale cliente                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           move      50                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cli-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr5-400.
      *              *-------------------------------------------------*
      *              * Stampa totali                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to prn-fin-lr5-999.
      *                  *---------------------------------------------*
      *                  * Subroutine per la fincatura di sopralinea-  *
      *                  * tura totali                                 *
      *                  *---------------------------------------------*
           perform   stp-fds-tot-000      thru stp-fds-tot-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr5-999.
       prn-fin-lr5-420.
      *                  *---------------------------------------------*
      *                  * Stampa totale cliente                       *
      *                  *---------------------------------------------*
       prn-fin-lr5-430.
      *                      *-----------------------------------------*
      *                      * Preparazione literal per il totale      *
      *                      *-----------------------------------------*
           move      "Totale cliente                                    
      -              "      "             to   w-stp-sta-tot-lit      .
       prn-fin-lr5-440.
      *                      *-----------------------------------------*
      *                      * Preparazione altri parametri            *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sta-tot-ldc      .
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-cli-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-cli-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-cli-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-cli-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-cli-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-cli-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-cli-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-cli-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-cli-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-lr5-450.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di stampa totali    *
      *                      *-----------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
       prn-fin-lr5-460.
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr5-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr5-999.
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per codice classe merceologica             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico della classe                *
      *                  *---------------------------------------------*
           move      srt-cod-cla-cod      to   w-liv-cla-cod-num      .
      *                  *---------------------------------------------*
      *                  * Mnemonico della classe                      *
      *                  *---------------------------------------------*
           move      srt-cod-cla-mne      to   w-liv-cla-cod-mne      .
      *                  *---------------------------------------------*
      *                  * Numero di sequenza della classe             *
      *                  *---------------------------------------------*
           move      srt-cod-cla-sqz      to   w-liv-cla-num-sqz      .
      *                  *---------------------------------------------*
      *                  * Segnale di classe ulteriormente suddivisa   *
      *                  *---------------------------------------------*
           move      srt-cod-cla-sud      to   w-liv-cla-ult-sud      .
      *                  *---------------------------------------------*
      *                  * Descrizione per la classe                   *
      *                  *---------------------------------------------*
           move      srt-cod-cla-des      to   w-liv-cla-des-cla      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cla-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cla-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-cla-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per la classe      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-cla-num-ele      .
       prn-ini-lr4-200.
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08 and
                     rr-tor-pro           not  = 09
                     go to prn-ini-lr4-999.
       prn-ini-lr4-300.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di classi trattate per il totale     *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-cli-num-ele      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08 and
                     rr-tor-pro           not  = 09
                     go to prn-fin-lr4-999.
       prn-fin-lr4-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-cla-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cla-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cla-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-cla-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-cla-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-cla-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr4-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale classe                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * classe                                  *
      *                      *-----------------------------------------*
           move      40                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-cla-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr4-400.
      *              *-------------------------------------------------*
      *              * Stampa totali                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to prn-fin-lr4-999.
      *                  *---------------------------------------------*
      *                  * Subroutine per la fincatura di sopralinea-  *
      *                  * tura totali con solo controllo di salto     *
      *                  * pagina                                      *
      *                  *---------------------------------------------*
           perform   stp-fds-nul-000      thru stp-fds-nul-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr4-999.
       prn-fin-lr4-420.
      *                  *---------------------------------------------*
      *                  * Stampa totale classe                        *
      *                  *---------------------------------------------*
       prn-fin-lr4-430.
      *                      *-----------------------------------------*
      *                      * Preparazione literal per il totale      *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sta-tot-lit      .
           string    "... Totale "
                                delimited by   size
                     w-liv-cla-des-cla
                                delimited by   size
                                          into w-stp-sta-tot-lit      .
       prn-fin-lr4-440.
      *                      *-----------------------------------------*
      *                      * Preparazione altri parametri            *
      *                      *-----------------------------------------*
           if        rr-tor-pro           =    09
                     move  spaces         to   w-stp-sta-tot-ldc
           else      move  "."            to   w-stp-sta-tot-ldc      .
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-cla-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-cla-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-cla-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-cla-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-cla-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-cla-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-cla-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-cla-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-cla-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-lr4-450.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di stampa totali    *
      *                      *-----------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
       prn-fin-lr4-460.
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr4-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr4-999.
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per codice gruppo merceologico             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico del gruppo                  *
      *                  *---------------------------------------------*
           move      srt-cod-gru-cod      to   w-liv-gru-cod-num      .
      *                  *---------------------------------------------*
      *                  * Mnemonico del gruppo                        *
      *                  *---------------------------------------------*
           move      srt-cod-gru-mne      to   w-liv-gru-cod-mne      .
      *                  *---------------------------------------------*
      *                  * Numero di sequenza del gruppo               *
      *                  *---------------------------------------------*
           move      srt-cod-gru-sqz      to   w-liv-gru-num-sqz      .
      *                  *---------------------------------------------*
      *                  * Segnale di gruppo ulteriormente suddiviso   *
      *                  *---------------------------------------------*
           move      srt-cod-gru-sud      to   w-liv-gru-ult-sud      .
      *                  *---------------------------------------------*
      *                  * Descrizione per il gruppo                   *
      *                  *---------------------------------------------*
           move      srt-cod-gru-des      to   w-liv-gru-des-gru      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gru-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gru-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gru-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per la classe      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gru-num-ele      .
       prn-ini-lr3-200.
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08
                     go to prn-ini-lr3-999.
      *              *-------------------------------------------------*
      *              * Se la classe di appartenenza non e' ulterior-   *
      *              * mente suddivisa : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-cla-ult-sud    not  = 02
                     go to prn-ini-lr3-999.
       prn-ini-lr3-300.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di gruppi trattate per il totale     *
      *                  * classe                                      *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-cla-num-ele      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08
                     go to prn-fin-lr3-999.
      *              *-------------------------------------------------*
      *              * Se la classe di appartenenza non e' ulterior-   *
      *              * mente suddivisa : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-cla-ult-sud    not  = 02
                     go to prn-fin-lr3-999.
       prn-fin-lr3-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-gru-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-gru-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-gru-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-gru-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-gru-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-gru-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr3-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale gruppo                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * gruppo                                  *
      *                      *-----------------------------------------*
           move      30                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-gru-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr3-400.
      *              *-------------------------------------------------*
      *              * Stampa totali                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to prn-fin-lr3-999.
      *                  *---------------------------------------------*
      *                  * Subroutine per la fincatura di sopralinea-  *
      *                  * tura totali con solo controllo di salto     *
      *                  * pagina                                      *
      *                  *---------------------------------------------*
           perform   stp-fds-nul-000      thru stp-fds-nul-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr3-999.
       prn-fin-lr3-420.
      *                  *---------------------------------------------*
      *                  * Stampa totale gruppo                        *
      *                  *---------------------------------------------*
       prn-fin-lr3-430.
      *                      *-----------------------------------------*
      *                      * Preparazione literal per il totale      *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sta-tot-lit      .
           string    ".. Totale "
                                delimited by   size
                     w-liv-gru-des-gru
                                delimited by   size
                                          into w-stp-sta-tot-lit      .
       prn-fin-lr3-440.
      *                      *-----------------------------------------*
      *                      * Preparazione altri parametri            *
      *                      *-----------------------------------------*
           move      "."                  to   w-stp-sta-tot-ldc      .
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-gru-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-gru-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-gru-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-gru-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-gru-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-gru-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-gru-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-gru-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-gru-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-lr3-450.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di stampa totali    *
      *                      *-----------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
       prn-fin-lr3-460.
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr3-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr3-999.
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per codice sottogruppo merceologico        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico del sottogruppo             *
      *                  *---------------------------------------------*
           move      srt-cod-sgr-cod      to   w-liv-sgr-cod-num      .
      *                  *---------------------------------------------*
      *                  * Mnemonico del sottogruppo                   *
      *                  *---------------------------------------------*
           move      srt-cod-sgr-mne      to   w-liv-sgr-cod-mne      .
      *                  *---------------------------------------------*
      *                  * Numero di sequenza del sottogruppo          *
      *                  *---------------------------------------------*
           move      srt-cod-sgr-sqz      to   w-liv-sgr-num-sqz      .
      *                  *---------------------------------------------*
      *                  * Descrizione per il sottogruppo              *
      *                  *---------------------------------------------*
           move      srt-cod-sgr-des      to   w-liv-sgr-des-sgr      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-sgr-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-sgr-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-sgr-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per la classe      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-sgr-num-ele      .
       prn-ini-lr2-200.
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08
                     go to prn-ini-lr2-999.
      *              *-------------------------------------------------*
      *              * Se la classe di appartenenza non e' ulterior-   *
      *              * mente suddivisa : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-cla-ult-sud    not  = 02
                     go to prn-ini-lr2-999.
      *              *-------------------------------------------------*
      *              * Se il gruppo di appartenenza non e' ulterior-   *
      *              * mente suddiviso : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-gru-ult-sud    not  = 02
                     go to prn-ini-lr2-999.
       prn-ini-lr2-300.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di sottogruppi trattati per il tota- *
      *                  * le gruppo                                   *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-gru-num-ele      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Se la stampa non e' per classificazione merceo- *
      *              * logica : uscita immediata senza alcuna azione   *
      *              *-------------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08
                     go to prn-fin-lr2-999.
      *              *-------------------------------------------------*
      *              * Se la classe di appartenenza non e' ulterior-   *
      *              * mente suddivisa : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-cla-ult-sud    not  = 02
                     go to prn-fin-lr2-999.
      *              *-------------------------------------------------*
      *              * Se il gruppo di appartenenza non e' ulterior-   *
      *              * mente suddiviso : uscita immediata senza alcuna *
      *              * azione                                          *
      *              *-------------------------------------------------*
           if        w-liv-gru-ult-sud    not  = 02
                     go to prn-fin-lr2-999.
       prn-fin-lr2-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-sgr-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-sgr-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-sgr-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-sgr-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-sgr-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-sgr-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr2-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale sottogruppo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * sottogruppo                             *
      *                      *-----------------------------------------*
           move      20                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-sgr-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr2-400.
      *              *-------------------------------------------------*
      *              * Stampa totali                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to prn-fin-lr2-999.
      *                  *---------------------------------------------*
      *                  * Subroutine per la fincatura di sopralinea-  *
      *                  * tura totali con solo controllo di salto     *
      *                  * pagina                                      *
      *                  *---------------------------------------------*
           perform   stp-fds-nul-000      thru stp-fds-nul-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr2-999.
       prn-fin-lr2-420.
      *                  *---------------------------------------------*
      *                  * Stampa totale sottogruppo                   *
      *                  *---------------------------------------------*
       prn-fin-lr2-430.
      *                      *-----------------------------------------*
      *                      * Preparazione literal per il totale      *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sta-tot-lit      .
           string    ". Totale "
                                delimited by   size
                     w-liv-sgr-des-sgr
                                delimited by   size
                                          into w-stp-sta-tot-lit      .
       prn-fin-lr2-440.
      *                      *-----------------------------------------*
      *                      * Preparazione altri parametri            *
      *                      *-----------------------------------------*
           move      "."                  to   w-stp-sta-tot-ldc      .
           if        w-prs-svf-tqn-snx    =    "N"
                     if    w-liv-sgr-flg-umi
                                          =    "N"
                           move  "N"      to   w-stp-sta-tot-snq
                     else  move  "S"      to   w-stp-sta-tot-snq
           else      move  "S"            to   w-stp-sta-tot-snq      .
           move      w-liv-sgr-val-dec    to   w-stp-sta-tot-dec      .
           move      w-liv-sgr-val-umi    to   w-stp-sta-tot-umi      .
           move      w-liv-sgr-qta-p1r    to   w-stp-sta-tot-q01      .
           move      w-liv-sgr-tot-p1r    to   w-stp-sta-tot-f01      .
           move      w-liv-sgr-qta-p2r    to   w-stp-sta-tot-q02      .
           move      w-liv-sgr-tot-p2r    to   w-stp-sta-tot-f02      .
           move      w-liv-sgr-qta-p3r    to   w-stp-sta-tot-q03      .
           move      w-liv-sgr-tot-p3r    to   w-stp-sta-tot-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-tot-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-tot-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-tot-not      .
       prn-fin-lr2-450.
      *                      *-----------------------------------------*
      *                      * Richiamo subroutine di stampa totali    *
      *                      *-----------------------------------------*
           perform   stp-sta-tot-000      thru stp-sta-tot-999        .
       prn-fin-lr2-460.
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata : uscita    *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr2-999.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr2-999.
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura per prodotto                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice numerico del prodotto                *
      *                  *---------------------------------------------*
           move      srt-cod-pro-cod      to   w-liv-pro-cod-num      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      srt-cod-pro-alf      to   w-liv-pro-cod-alf      .
      *                  *---------------------------------------------*
      *                  * Descrizione per il prodotto                 *
      *                  *---------------------------------------------*
           move      srt-cod-pro-des      to   w-liv-pro-des-pro      .
      *                  *---------------------------------------------*
      *                  * Flag di unita' di misura memorizzata        *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-pro-flg-umi      .
      *                  *---------------------------------------------*
      *                  * Valore dell' unita' di misura memorizzata   *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-pro-val-umi      .
      *                  *---------------------------------------------*
      *                  * Flag di numero decimali memorizzati         *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-pro-flg-dec      .
      *                  *---------------------------------------------*
      *                  * Valore del numero decimali memorizzati      *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-val-dec      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-qta-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 1. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-qta-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 2. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totale quantita' 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-qta-p3r      .
      *                  *---------------------------------------------*
      *                  * Totale fatturato 3. periodo di riferimento  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero elementi trattati per il prodotto    *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-pro-num-ele      .
       prn-ini-lr1-200.
      *              *-------------------------------------------------*
      *              * Incrementi per il livello successivo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero prodotti trattati per uno dei se-    *
      *                  * guenti, in alternativa                      *
      *                  *  - Totale sottogruppo merceologico          *
      *                  *  - Totale gruppo merceologico               *
      *                  *  - Totale classe merceologica               *
      *                  *  - Totale cliente                           *
      *                  *---------------------------------------------*
           if        rr-tor-pro           not  = 07 and
                     rr-tor-pro           not  = 08
                     add   1              to   w-liv-cli-num-ele
           else if   w-liv-gru-ult-sud    =    02
                     add   1              to   w-liv-sgr-num-ele
           else if   w-liv-cla-ult-sud    =    02
                     add   1              to   w-liv-gru-num-ele
           else      add   1              to   w-liv-cla-num-ele      .
       prn-ini-lr1-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-ini-lr1-999.
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-100.
      *              *-------------------------------------------------*
      *              * Determinazione % di variazione                  *
      *              *-------------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-liv-pro-qta-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-pro-qta-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-pro-qta-p3r
                                          to   w-stp-clc-pdv-v03
           else      move  w-liv-pro-tot-p1r
                                          to   w-stp-clc-pdv-v01
                     move  w-liv-pro-tot-p2r
                                          to   w-stp-clc-pdv-v02
                     move  w-liv-pro-tot-p3r
                                          to   w-stp-clc-pdv-v03      .
           perform   stp-clc-pdv-000      thru stp-clc-pdv-999        .
       prn-fin-lr1-125.
      *              *-------------------------------------------------*
      *              * Determinazione valore del campo 'Note' per il   *
      *              * totale prodotto                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di determinazione : per il totale  *
      *                      * prodotto                                *
      *                      *-----------------------------------------*
           move      10                   to   w-mod-not-svf-tdd      .
      *                      *-----------------------------------------*
      *                      * Numero di caratteri disponibili         *
      *                      *-----------------------------------------*
           move      w-stp-clc-not-lun    to   w-mod-not-svf-ncd      .
      *                      *-----------------------------------------*
      *                      * 1. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-qta-p1r    to   w-mod-not-svf-q01      .
      *                      *-----------------------------------------*
      *                      * 1. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-tot-p1r    to   w-mod-not-svf-f01      .
      *                      *-----------------------------------------*
      *                      * 2. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-qta-p2r    to   w-mod-not-svf-q02      .
      *                      *-----------------------------------------*
      *                      * 2. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-tot-p2r    to   w-mod-not-svf-f02      .
      *                      *-----------------------------------------*
      *                      * 1. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p01    to   w-mod-not-svf-p01      .
      *                      *-----------------------------------------*
      *                      * 3. quantita'                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-qta-p3r    to   w-mod-not-svf-q03      .
      *                      *-----------------------------------------*
      *                      * 3. fatturato                            *
      *                      *-----------------------------------------*
           move      w-liv-pro-tot-p3r    to   w-mod-not-svf-f03      .
      *                      *-----------------------------------------*
      *                      * 2. % di variazione                      *
      *                      *-----------------------------------------*
           move      w-stp-clc-pdv-p02    to   w-mod-not-svf-p02      .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-mod-not-svf-tdo      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-mod-not-svf-ndo      .
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo del modulo               *
      *                  *---------------------------------------------*
           perform   mod-not-svf-dvn-000  thru mod-not-svf-dvn-999    .
       prn-fin-lr1-300.
      *              *-------------------------------------------------*
      *              * Stampa effettiva                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tor-pro           =    09
                     go to prn-fin-lr1-800.
       prn-fin-lr1-310.
      *                  *---------------------------------------------*
      *                  * Test se linee residue sufficienti           *
      *                  *---------------------------------------------*
           if        p-res                >    1
                     go to prn-fin-lr1-320.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-999.
      *                  *---------------------------------------------*
      *                  * Richiamo subroutine di fincatura verticale  *
      *                  * della pagina                                *
      *                  *---------------------------------------------*
           perform   stp-fnc-vrt-000      thru stp-fnc-vrt-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-999.
       prn-fin-lr1-320.
      *                  *---------------------------------------------*
      *                  * Interlinea di separazione                   *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-330.
      *                  *---------------------------------------------*
      *                  * Subroutine per la stampa del codice prodot- *
      *                  * to, della sua descrizione                   *
      *                  *---------------------------------------------*
       prn-fin-lr1-331.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice prodotto numerico            *
      *                          *-------------------------------------*
           move      w-liv-pro-cod-num    to   w-stp-ced-pro-num      .
      *                          *-------------------------------------*
      *                          * Codice prodotto alfanumerico        *
      *                          *-------------------------------------*
           move      w-liv-pro-cod-alf    to   w-stp-ced-pro-alf      .
      *                          *-------------------------------------*
      *                          * Descrizione del prodotto            *
      *                          *-------------------------------------*
           move      w-liv-pro-des-pro    to   w-stp-ced-pro-des      .
       prn-fin-lr1-332.
      *                      *-----------------------------------------*
      *                      * Esecuzione                              *
      *                      *-----------------------------------------*
           perform   stp-ced-pro-000      thru stp-ced-pro-999        .
       prn-fin-lr1-340.
      *                  *---------------------------------------------*
      *                  * Subroutine per la stampa delle voci Unita'  *
      *                  * di misura, Quantita, Fatturato, e %         *
      *                  *---------------------------------------------*
       prn-fin-lr1-341.
      *                      *-----------------------------------------*
      *                      * Preparazione                            *
      *                      *-----------------------------------------*
           move      "S"                  to   w-stp-sta-qfp-snq      .
           move      w-liv-pro-val-umi    to   w-stp-sta-qfp-umi      .
           move      w-liv-pro-val-dec    to   w-stp-sta-qfp-dec      .
           move      w-liv-pro-qta-p1r    to   w-stp-sta-qfp-q01      .
           move      w-liv-pro-tot-p1r    to   w-stp-sta-qfp-f01      .
           move      w-liv-pro-qta-p2r    to   w-stp-sta-qfp-q02      .
           move      w-liv-pro-tot-p2r    to   w-stp-sta-qfp-f02      .
           move      w-liv-pro-qta-p3r    to   w-stp-sta-qfp-q03      .
           move      w-liv-pro-tot-p3r    to   w-stp-sta-qfp-f03      .
           move      w-stp-clc-pdv-p01    to   w-stp-sta-qfp-p01      .
           move      w-stp-clc-pdv-p02    to   w-stp-sta-qfp-p02      .
           move      w-mod-not-svf-not    to   w-stp-sta-qfp-not      .
       prn-fin-lr1-342.
      *                      *-----------------------------------------*
      *                      * Esecuzione                              *
      *                      *-----------------------------------------*
           perform   stp-sta-qfp-000      thru stp-sta-qfp-999        .
       prn-fin-lr1-800.
      *                  *---------------------------------------------*
      *                  * Subroutine per l'emissione archivio sequen- *
      *                  * ziale                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Esecuzione                              *
      *                      *-----------------------------------------*
           perform   exe-gen-fil-000      thru exe-gen-fil-999        .
       prn-fin-lr1-900.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-lr1-999.
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento unita' di misura                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Per livello generale                        *
      *                  *---------------------------------------------*
           if        w-liv-gen-flg-umi    =    spaces
                     move  "S"            to   w-liv-gen-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-gen-val-umi
           else if   w-liv-gen-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-gen-flg-umi
                     move  spaces         to   w-liv-gen-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello classe geografica               *
      *                  *---------------------------------------------*
           if        w-liv-cgc-flg-umi    =    spaces
                     move  "S"            to   w-liv-cgc-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-cgc-val-umi
           else if   w-liv-cgc-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-cgc-flg-umi
                     move  spaces         to   w-liv-cgc-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello cliente                         *
      *                  *---------------------------------------------*
           if        w-liv-cli-flg-umi    =    spaces
                     move  "S"            to   w-liv-cli-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-cli-val-umi
           else if   w-liv-cli-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-cli-flg-umi
                     move  spaces         to   w-liv-cli-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello classe merceologica             *
      *                  *---------------------------------------------*
           if        w-liv-cla-flg-umi    =    spaces
                     move  "S"            to   w-liv-cla-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-cla-val-umi
           else if   w-liv-cla-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-cla-flg-umi
                     move  spaces         to   w-liv-cla-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello gruppo merceologico             *
      *                  *---------------------------------------------*
           if        w-liv-gru-flg-umi    =    spaces
                     move  "S"            to   w-liv-gru-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-gru-val-umi
           else if   w-liv-gru-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-gru-flg-umi
                     move  spaces         to   w-liv-gru-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello sottogruppo merceologico        *
      *                  *---------------------------------------------*
           if        w-liv-sgr-flg-umi    =    spaces
                     move  "S"            to   w-liv-sgr-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-sgr-val-umi
           else if   w-liv-sgr-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-sgr-flg-umi
                     move  spaces         to   w-liv-sgr-val-umi      .
      *                  *---------------------------------------------*
      *                  * Per livello prodotto                        *
      *                  *---------------------------------------------*
           if        w-liv-pro-flg-umi    =    spaces
                     move  "S"            to   w-liv-pro-flg-umi
                     move  srt-cod-pro-umi
                                          to   w-liv-pro-val-umi
           else if   w-liv-pro-val-umi    not  = srt-cod-pro-umi
                     move  "N"            to   w-liv-pro-flg-umi
                     move  spaces         to   w-liv-pro-val-umi      .
       prn-liv-det-150.
      *              *-------------------------------------------------*
      *              * Aggiornamento numero decimali quantita'         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Per livello generale                        *
      *                  *---------------------------------------------*
           if        w-liv-gen-flg-dec    =    spaces
                     move  "#"            to   w-liv-gen-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-gen-val-dec
           else if   w-liv-gen-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-gen-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello classe geografica               *
      *                  *---------------------------------------------*
           if        w-liv-cgc-flg-dec    =    spaces
                     move  "#"            to   w-liv-cgc-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-cgc-val-dec
           else if   w-liv-cgc-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-cgc-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello cliente                         *
      *                  *---------------------------------------------*
           if        w-liv-cli-flg-dec    =    spaces
                     move  "#"            to   w-liv-cli-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-cli-val-dec
           else if   w-liv-cli-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-cli-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello classe merceologica             *
      *                  *---------------------------------------------*
           if        w-liv-cla-flg-dec    =    spaces
                     move  "#"            to   w-liv-cla-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-cla-val-dec
           else if   w-liv-cla-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-cla-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello gruppo merceologico             *
      *                  *---------------------------------------------*
           if        w-liv-gru-flg-dec    =    spaces
                     move  "#"            to   w-liv-gru-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-gru-val-dec
           else if   w-liv-gru-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-gru-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello sottogruppo merceologico        *
      *                  *---------------------------------------------*
           if        w-liv-sgr-flg-dec    =    spaces
                     move  "#"            to   w-liv-sgr-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-sgr-val-dec
           else if   w-liv-sgr-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-sgr-val-dec      .
      *                  *---------------------------------------------*
      *                  * Per livello prodotto                        *
      *                  *---------------------------------------------*
           if        w-liv-pro-flg-dec    =    spaces
                     move  "#"            to   w-liv-pro-flg-dec
                     move  srt-cod-pro-dec
                                          to   w-liv-pro-val-dec
           else if   w-liv-pro-val-dec    not  = srt-cod-pro-dec
                     move  9              to   w-liv-pro-val-dec      .
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Accumuli per livello generale                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gen-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-gen-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gen-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-gen-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gen-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-gen-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gen-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-gen-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gen-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-gen-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gen-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-gen-tot-p3r      .
       prn-liv-det-250.
      *              *-------------------------------------------------*
      *              * Accumuli per livello classe geografica          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cgc-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-cgc-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cgc-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-cgc-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cgc-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-cgc-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cgc-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-cgc-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cgc-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-cgc-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cgc-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-cgc-tot-p3r      .
       prn-liv-det-300.
      *              *-------------------------------------------------*
      *              * Accumuli per livello cliente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cli-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-cli-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cli-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-cli-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cli-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-cli-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cli-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-cli-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cli-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-cli-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cli-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-cli-tot-p3r      .
       prn-liv-det-350.
      *              *-------------------------------------------------*
      *              * Accumuli per livello classe merceologica        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cla-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-cla-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cla-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-cla-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cla-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-cla-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cla-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-cla-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-cla-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-cla-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-cla-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-cla-tot-p3r      .
       prn-liv-det-400.
      *              *-------------------------------------------------*
      *              * Accumuli per livello gruppo merceologico        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gru-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-gru-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gru-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-gru-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gru-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-gru-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gru-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-gru-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-gru-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-gru-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-gru-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-gru-tot-p3r      .
       prn-liv-det-450.
      *              *-------------------------------------------------*
      *              * Accumuli per livello sottogruppo merceologico   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-sgr-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-sgr-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-sgr-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-sgr-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-sgr-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-sgr-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-sgr-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-sgr-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-sgr-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-sgr-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-sgr-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-sgr-tot-p3r      .
       prn-liv-det-500.
      *              *-------------------------------------------------*
      *              * Accumuli per livello prodotto                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totali 1. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn1      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-pro-qta-p1r
                           add      srt-val-sta-val
                                          to   w-liv-pro-tot-p1r
                     else  subtract srt-val-sta-qta
                                          from w-liv-pro-qta-p1r
                           subtract srt-val-sta-val
                                          from w-liv-pro-tot-p1r      .
      *                  *---------------------------------------------*
      *                  * Totali 2. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn2      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-pro-qta-p2r
                           add      srt-val-sta-val
                                          to   w-liv-pro-tot-p2r
                     else  subtract srt-val-sta-qta
                                          from w-liv-pro-qta-p2r
                           subtract srt-val-sta-val
                                          from w-liv-pro-tot-p2r      .
      *                  *---------------------------------------------*
      *                  * Totali 3. periodo di riferimento            *
      *                  *---------------------------------------------*
           if        srt-val-sta-sn3      =    "S"
                     if    srt-val-sta-pon
                                          not  = "N"
                           add      srt-val-sta-qta
                                          to   w-liv-pro-qta-p3r
                           add      srt-val-sta-val
                                          to   w-liv-pro-tot-p3r
                     else  subtract srt-val-sta-qta
                                          from w-liv-pro-qta-p3r
                           subtract srt-val-sta-val
                                          from w-liv-pro-tot-p3r      .
      *                  *---------------------------------------------*
      *                  * Numero documenti trattati per il prodotto   *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-pro-num-ele      .
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'intestazione della pagina                *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa del ti- *
      *              * tolo vero e proprio                             *
      *              *-------------------------------------------------*
           perform   stp-tit-pag-000      thru stp-tit-pag-999        .
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o su stampa si pone in On  *
      *              * il flag di interruzione forzata e si esce im-   *
      *              * mediatamente                                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-int-pag-999.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-int-pag-999.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa dei ri- *
      *              * ferimenti alle dipendenze e ai periodi di rife- *
      *              * rimento                                         *
      *              *-------------------------------------------------*
           perform   stp-dep-pag-000      thru stp-dep-pag-999        .
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o su stampa si pone in On  *
      *              * il flag di interruzione forzata e si esce im-   *
      *              * mediatamente                                    *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-int-pag-999.
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la stampa del titolo vero e proprio        *
      *    *-----------------------------------------------------------*
       stp-tit-pag-000.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio assoluto                  *
      *              *-------------------------------------------------*
       stp-tit-pag-010.
      *                  *---------------------------------------------*
      *                  * Incremento numero totale di intestazioni e- *
      *                  * seguite                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-int-num-int      .
      *                  *---------------------------------------------*
      *                  * Se non e' l'inizio in assoluto : nessuna a- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-stp-int-num-int    >    1
                     go to stp-tit-pag-100.
       stp-tit-pag-020.
      *                  *---------------------------------------------*
      *                  * Preparazione titolo stampato, linea 1, al-  *
      *                  * lineato a sinistra                          *
      *                  *---------------------------------------------*
       stp-tit-pag-025.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se statistica su : *
      *                      * - Fatturato nuovo                       *
      *                      * - Non-fatturato                         *
      *                      * - Fatturato normale                     *
      *                      *-----------------------------------------*
           if        rr-num-pdr           =    11
                     go to stp-tit-pag-026
           else if   rr-num-pdr           =    12
                     go to stp-tit-pag-027
           else      go to stp-tit-pag-028.
       stp-tit-pag-026.
      *                      *-----------------------------------------*
      *                      * Se statistica sul fatturato nuovo       *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-p2d-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt0      .
      *
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-p2d-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt9      .
      *
           move      spaces               to   w-stp-int-tit-stp      .
           string    "STATISTICA SUL NUOVO FATTURATO DAL "
                                delimited by   size
                     w-stp-int-wed-dt0
                                delimited by   size
                     " AL "
                                delimited by   size
                     w-stp-int-wed-dt9
                                delimited by   size
                                          into w-stp-int-tit-stp      .
           go to     stp-tit-pag-030.
       stp-tit-pag-027.
      *                      *-----------------------------------------*
      *                      * Se statistica sul non-fatturato         *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-p2d-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt0      .
      *
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-p2d-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt9      .
      *
           move      spaces               to   w-stp-int-tit-stp      .
           string    "STATISTICA SUL NON FATTURATO DAL "
                                delimited by   size
                     w-stp-int-wed-dt0
                                delimited by   size
                     " AL "
                                delimited by   size
                     w-stp-int-wed-dt9
                                delimited by   size
                                          into w-stp-int-tit-stp      .
           go to     stp-tit-pag-030.
       stp-tit-pag-028.
      *                      *-----------------------------------------*
      *                      * Se statistica sul fatturato normale     *
      *                      *-----------------------------------------*
           move      "STATISTICA SUL FATTURATO                          
      -              "                              "
                                          to   w-stp-int-tit-stp      .
           go to     stp-tit-pag-030.
       stp-tit-pag-030.
      *                  *---------------------------------------------*
      *                  * Preparazione titolo stampato, linea 2, al-  *
      *                  * lineato a sinistra                          *
      *                  *---------------------------------------------*
           if        rr-tcg-cli           =    01
                     go to stp-tit-pag-031
           else if   rr-tcg-cli           =    02
                     go to stp-tit-pag-032
           else if   rr-tcg-cli           =    03
                     go to stp-tit-pag-033.
       stp-tit-pag-031.
           move      "PER NAZIONE - CLIENTE - PRODOTTO                  
      -              "                              "
                                          to   w-stp-int-ti2-stp      .
           go to     stp-tit-pag-050.
       stp-tit-pag-032.
           move      "PER REGIONE - CLIENTE - PRODOTTO                  
      -              "                              "
                                          to   w-stp-int-ti2-stp      .
           go to     stp-tit-pag-050.
       stp-tit-pag-033.
           move      "PER PROVINCIA - CLIENTE - PRODOTTO                
      -              "                              "
                                          to   w-stp-int-ti2-stp      .
           go to     stp-tit-pag-050.
       stp-tit-pag-050.
      *                  *---------------------------------------------*
      *                  * Numero pagina da stampare a zero            *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Preparazione data di stampa                 *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-stp-int-dat-stp      .
      *                  *---------------------------------------------*
      *                  * Preparazione ragione sociale azienda        *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-stp-int-rag-azi      .
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza della ragio- *
      *                  * ne sociale azienda allineata a sinistra,    *
      *                  * senza considerare gli spazi in coda         *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-rag-azi
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      40                   to   w-stp-int-rag-azl      .
           subtract  w-stp-int-wct-c01    from w-stp-int-rag-azl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza del titolo   *
      *                  * dello stampato allineato a sinistra, senza  *
      *                  * considerare gli spazi in coda               *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-tit-stp
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      80                   to   w-stp-int-tit-stl      .
           subtract  w-stp-int-wct-c01    from w-stp-int-tit-stl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della posizione di stampa    *
      *                  * per il titolo dello stampato                *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-int-tit-stc      .
           subtract  w-stp-int-tit-stl    from w-stp-int-tit-stc      .
           divide    2                    into w-stp-int-tit-stc      .
           add       1                    to   w-stp-int-tit-stc      .
      *                  *---------------------------------------------*
      *                  * Determinazione se necessarie una o due ri-  *
      *                  * ghe per il titolo dello stampato            *
      *                  *---------------------------------------------*
           move      w-stp-int-rag-azl    to   w-stp-int-wct-c01      .
           add       2                    to   w-stp-int-wct-c01      .
           move      w-stp-int-tit-stc    to   w-stp-int-wct-c02      .
           move      w-stp-int-tit-stc    to   w-stp-int-wct-c03      .
           add       w-stp-int-tit-stl    to   w-stp-int-wct-c03      .
           subtract  1                    from w-stp-int-wct-c03      .
           move      p-sel-als-sel        to   w-stp-int-wct-c04      .
           subtract  27                   from w-stp-int-wct-c04      .
           if        w-stp-int-wct-c02    >    w-stp-int-wct-c01 and
                     w-stp-int-wct-c03    <    w-stp-int-wct-c04
                     move  "U"            to   w-stp-int-tit-uod
           else      move  "D"            to   w-stp-int-tit-uod      .
       stp-tit-pag-100.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio pagina                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero pagina da stampare        *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero caratteri di slitta-  *
      *                  * mento a destra per data e pagina in funzio- *
      *                  * ne del numero pagina                        *
      *                  *---------------------------------------------*
           if        w-stp-int-num-pag    >    9999
                     move  zero           to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    999
                     move  1              to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    99
                     move  2              to   w-stp-int-ncs-dep
           else if   w-stp-int-num-pag    >    9
                     move  3              to   w-stp-int-ncs-dep
           else      move  4              to   w-stp-int-ncs-dep      .
       stp-tit-pag-200.
      *              *-------------------------------------------------*
      *              * Esecuzione intestazione pagina                  *
      *              *-------------------------------------------------*
       stp-tit-pag-250.
      *                  *---------------------------------------------*
      *                  * Avanzamento pagina                          *
      *                  *---------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si pone in *
      *                  * On il flag di interruzione forzata e si e-  *
      *                  * sce immediatamente                          *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move  "#"            to   w-cnt-prn-flg-int
                     go to stp-tit-pag-999.
       stp-tit-pag-300.
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-tit-pag-999.
      *                  *---------------------------------------------*
      *                  * Linea di '=' iniziale                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tit-pag-350.
      *                  *---------------------------------------------*
      *                  * Ragione sociale azienda                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-int-rag-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Data :'                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  25                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data di stampa                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  18                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      w-stp-int-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal 'Pag.'                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  08                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      5                    to   p-car                  .
           subtract  w-stp-int-ncs-dep    from p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  04                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      w-stp-int-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se stampa su una linea si passa direttamen- *
      *                  * te a stampare il titolo centrale            *
      *                  *---------------------------------------------*
           if        w-stp-int-tit-uod    =    "U"
                     go to stp-tit-pag-375.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tit-pag-375.
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-int-tit-stl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-int-tit-stc    to   p-pos                  .
           move      w-stp-int-tit-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tit-pag-380.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 2. linea di titolo per lo stampato      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione lunghezza            *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-ti2-stp
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      80                   to   p-car                  .
           subtract  w-stp-int-wct-c01    from p-car                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  p-car                from p-pos                  .
           divide    2                    into p-pos                  .
           add       1                    to   p-pos                  .
           move      w-stp-int-ti2-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tit-pag-390.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Linea di '-' finale                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale per lasciare   *
      *                      * una interlinee di separazione           *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tit-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la stampa dei riferimenti alle dipendenze  *
      *    * e ai periodi di riferimento                               *
      *    *-----------------------------------------------------------*
       stp-dep-pag-000.
      *              *-------------------------------------------------*
      *              * Se il numero pagina da stampare non e' pari a   *
      *              * 1 : no stampa riferimenti a dipendenze e ai pe- *
      *              * riodi di riferimento                            *
      *              *-------------------------------------------------*
           if        w-stp-int-num-pag    not  = 1
                     go to stp-int-pag-999.
       stp-dep-pag-200.
      *              *-------------------------------------------------*
      *              * 1. periodo di riferimento                       *
      *              *-------------------------------------------------*
       stp-dep-pag-225.
      *                  *---------------------------------------------*
      *                  * Periodo di riferimento vero e proprio       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal 'Periodo di riferimento :'      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      24                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     move  "Periodi di riferimento :"
                                          to   p-alf
           else      move  "Periodo di riferimento :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero periodo di riferimento           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      027                  to   p-pos                  .
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     move  "1."           to   p-alf
           else      move  spaces         to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data minima                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     move  030            to   p-pos
           else      move  026            to   p-pos                  .
           move      rr-p1d-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Separazione                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     move  038            to   p-pos
           else      move  034            to   p-pos                  .
           move      " - "                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * 1. periodo, data massima                *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     move  041            to   p-pos
           else      move  037            to   p-pos                  .
           move      rr-p1d-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-dep-pag-250.
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
       stp-dep-pag-255.
      *                      *-----------------------------------------*
      *                      * Area editata di comodo : a spaces       *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-int-wed-e01      .
      *                      *-----------------------------------------*
      *                      * Se numero globale di dipendenze non     *
      *                      * maggiore di 1 : nessun editing          *
      *                      *-----------------------------------------*
           if        rr-dpz-ctr-dpz       not  > 1
                     go to stp-dep-pag-300.
      *                      *-----------------------------------------*
      *                      * Se selezionata una sola dipendenza: a   *
      *                      * editing per una sola dipendenza         *
      *                      *-----------------------------------------*
           if        rr-dpz-inu           not  = zero
                     go to stp-dep-pag-260.
      *                      *-----------------------------------------*
      *                      * Se selezionate tutte le dipendenze: a   *
      *                      * editing per tutte le dipendenze, altri- *
      *                      * menti, a editing per piu' dipendenze,   *
      *                      * anche se non tutte                      *
      *                      *-----------------------------------------*
           if        rr-dpz-ctr-sel       =    rr-dpz-ctr-dpz
                     go to stp-dep-pag-265
           else      go to stp-dep-pag-270.
       stp-dep-pag-260.
      *                      *-----------------------------------------*
      *                      * Editing per una sola dipendenza         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      2                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      rr-dpz-inu           to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           string    "Dipendenza "
                                delimited by   size
                     p-edt
                                delimited by spaces
                     " : "
                                delimited by   size
                     rr-dpz-inu-den
                                delimited by   size
                                          into w-stp-int-wed-e01      .
      *                          *-------------------------------------*
      *                          * A stampa                            *
      *                          *-------------------------------------*
           go to     stp-dep-pag-300.
       stp-dep-pag-265.
      *                      *-----------------------------------------*
      *                      * Editing per tutte le dipendenze         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "Tutte le dipendenze"
                                          to   w-stp-int-wed-e01      .
      *                          *-------------------------------------*
      *                          * A stampa                            *
      *                          *-------------------------------------*
           go to     stp-dep-pag-300.
       stp-dep-pag-270.
      *                      *-----------------------------------------*
      *                      * Editing per piu' di una dipendenza, an- *
      *                      * che se non tutte                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           string    "Dipendenze : "
                                delimited by   size
                     rr-dpz-inu-den
                                delimited by   size
                                          into w-stp-int-wed-e01      .
           move      zero                 to   w-stp-int-wct-c01      .
           move      14                   to   w-stp-int-wct-c02      .
       stp-dep-pag-275.
           add       1                    to   w-stp-int-wct-c01      .
           if        w-stp-int-wct-c01    >    rr-dpz-ctr-sel
                     go to stp-dep-pag-280.
           move      rr-dpz-ele-cod
                    (w-stp-int-wct-c01)   to   w-stp-int-wed-e01
                                              (w-stp-int-wct-c02 :)   .
           add       3                    to   w-stp-int-wct-c02      .
           if        w-stp-int-wct-c02    >    39
                     go to stp-dep-pag-280.
           go to     stp-dep-pag-275.
       stp-dep-pag-280.
      *                          *-------------------------------------*
      *                          * A stampa                            *
      *                          *-------------------------------------*
           go to     stp-dep-pag-300.
       stp-dep-pag-300.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se area editata a spaces : nessuna  *
      *                          * stampa                              *
      *                          *-------------------------------------*
           if        w-stp-int-wed-e01    =    spaces
                     go to stp-dep-pag-400.
      *                          *-------------------------------------*
      *                          * Determinazione numero caratteri u-  *
      *                          * tili, senza spazi in coda           *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-wed-e01
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      40                   to   w-stp-int-wct-c02      .
           subtract  w-stp-int-wct-c01    from w-stp-int-wct-c02      .
      *                          *-------------------------------------*
      *                          * Determinazione della posizione di   *
      *                          * stampa                              *
      *                          *-------------------------------------*
           move      p-sel-als-sel        to   w-stp-int-wct-c03      .
           subtract  w-stp-int-wct-c02    from w-stp-int-wct-c03      .
           add       1                    to   w-stp-int-wct-c03      .
      *                          *-------------------------------------*
      *                          * Stampa effettiva                    *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-int-wct-c02    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-int-wct-c03    to   p-pos                  .
           move      w-stp-int-wed-e01    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-dep-pag-400.
      *              *-------------------------------------------------*
      *              * 2. periodo di riferimento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il numero di periodi di riferimento non  *
      *                  * e' superiore a 1 : nessuna azione           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           not  = 02 and
                     rr-num-pdr           not  = 03
                     go to stp-dep-pag-600.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero periodo di riferimento               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      027                  to   p-pos                  .
           move      "2."                 to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 2. periodo, data minima                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      030                  to   p-pos                  .
           move      rr-p2d-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Separazione                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      038                  to   p-pos                  .
           move      " - "                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 2. periodo, data massima                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      041                  to   p-pos                  .
           move      rr-p2d-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-dep-pag-600.
      *              *-------------------------------------------------*
      *              * 3. periodo di riferimento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il numero di periodi di riferimento non  *
      *                  * e' superiore a 2 : nessuna azione           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           not  = 03
                     go to stp-dep-pag-800.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero periodo di riferimento               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      027                  to   p-pos                  .
           move      "3."                 to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 3. periodo, data minima                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      030                  to   p-pos                  .
           move      rr-p3d-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Separazione                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      038                  to   p-pos                  .
           move      " - "                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * 3. periodo, data massima                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      041                  to   p-pos                  .
           move      rr-p3d-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-dep-pag-800.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale per lasciare una in-   *
      *              * terlinea di separazione                         *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-dep-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la sub-intestazione                        *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-sub-tip-cod : Tipo codice da stampare           *
      *    *                         - N : Numerico                    *
      *    *                         - A : Alfanumerico                *
      *    *                                                           *
      *    *   - w-stp-sub-cod-num : Valore codice, se numerico        *
      *    *                                                           *
      *    *   - w-stp-sub-cod-alf : Valore codice, se alfanumerico    *
      *    *                                                           *
      *    *   - w-stp-sub-app-cod : Valore appendice al codice        *
      *    *                                                           *
      *    *   - w-stp-sub-cod-des : Descrizione per il codice         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-sub-int-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-sub-int-999.
       stp-sub-int-020.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di sub-intestazione  *
      *                  *---------------------------------------------*
       stp-sub-int-021.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo classe  *
      *                      * geografica cliente selezionata          *
      *                      *-----------------------------------------*
           if        rr-tcg-cli           =    01
                     go to stp-sub-int-025
           else if   rr-tcg-cli           =    02
                     go to stp-sub-int-026
           else if   rr-tcg-cli           =    03
                     go to stp-sub-int-027.
       stp-sub-int-025.
      *                      *-----------------------------------------*
      *                      * Classe 01 : Nazione                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Nazione :                     "
                                          to   w-stp-sub-lit-sub      .
      *                          *-------------------------------------*
      *                          * Tipo di codice, Numerico o Alfa o + *
      *                          *-------------------------------------*
           move      "A"                  to   w-stp-sub-tip-cod      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-030.
       stp-sub-int-026.
      *                      *-----------------------------------------*
      *                      * Classe 02 : Regione                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Regione :                     "
                                          to   w-stp-sub-lit-sub      .
      *                          *-------------------------------------*
      *                          * Tipo di codice, Numerico o Alfa o + *
      *                          *-------------------------------------*
           move      "A"                  to   w-stp-sub-tip-cod      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-030.
       stp-sub-int-027.
      *                      *-----------------------------------------*
      *                      * Classe 03 : Provincia                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Provincia :                   "
                                          to   w-stp-sub-lit-sub      .
      *                          *-------------------------------------*
      *                          * Tipo di codice, Numerico o Alfa o + *
      *                          *-------------------------------------*
           move      "A"                  to   w-stp-sub-tip-cod      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-030.
       stp-sub-int-030.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area di sub-inte-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-sub-ads-asx      .
           move      zero                 to   w-stp-sub-ads-lng      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-ads-lng
                                          for  all   spaces           .
       stp-sub-int-040.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione di stampa iniziale *
      *                  * per l'area di sub-intestazione              *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-sub-pos-dsi      .
           subtract  w-stp-sub-ads-lng    from w-stp-sub-pos-dsi      .
           divide    2                    into w-stp-sub-pos-dsi      .
           add       1                    to   w-stp-sub-pos-dsi      .
       stp-sub-int-100.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     if     p-res         >    17
                            go to stp-sub-int-200
                     else   go to stp-sub-int-150
           else      if     p-res         >    16
                            go to stp-sub-int-200
                     else   go to stp-sub-int-150.
       stp-sub-int-150.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata in On : si esce *
      *              * immediatamente                                  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-sub-int-999.
       stp-sub-int-200.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale subordinato, per la-   *
      *              * sciare una interlinea di separazione            *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sub-int-300.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           add       30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           subtract  15                   from p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sub-int-400.
      *              *-------------------------------------------------*
      *              * Linea centrale di sub-intestazione              *
      *              *-------------------------------------------------*
       stp-sub-int-425.
      *                  *---------------------------------------------*
      *                  * Se codice numerico a zero e codice alfanu-  *
      *                  * merico a spaces si prepara la dicitura di   *
      *                  * non classificazione e si omette l'editing   *
      *                  * complesso                                   *
      *                  *---------------------------------------------*
       stp-sub-int-427.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-stp-sub-cod-num    not  = zero   or
                     w-stp-sub-cod-alf    not  = spaces
                     go to stp-sub-int-450.
      *                      *-----------------------------------------*
      *                      * Preparazione literal fisso              *
      *                      *-----------------------------------------*
       stp-sub-int-430.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo classe  *
      *                      * geografica cliente selezionata          *
      *                      *-----------------------------------------*
           if        rr-tcg-cli           =    01
                     go to stp-sub-int-431
           else if   rr-tcg-cli           =    02
                     go to stp-sub-int-432
           else if   rr-tcg-cli           =    03
                     go to stp-sub-int-433.
       stp-sub-int-431.
      *                      *-----------------------------------------*
      *                      * Classe 01 : Nazione                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Vendite non attribuite ad alcuna nazione          
      -              "                      "
                                          to   w-stp-sub-ads-asx      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-500.
       stp-sub-int-432.
      *                      *-----------------------------------------*
      *                      * Classe 02 : Regione                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Vendite non attribuite ad alcuna regione          
      -              "                      "
                                          to   w-stp-sub-ads-asx      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-500.
       stp-sub-int-433.
      *                      *-----------------------------------------*
      *                      * Classe 03 : Provincia                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione Literal                 *
      *                          *-------------------------------------*
           move      "Vendite non attribuite ad alcuna provincia        
      -              "                      "
                                          to   w-stp-sub-ads-asx      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-500.
       stp-sub-int-450.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di sub-intestazione,   *
      *                  * allineata a sinistra                        *
      *                  *---------------------------------------------*
       stp-sub-int-455.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           move      w-stp-sub-lit-sub    to   w-stp-sub-ads-asx      .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
       stp-sub-int-460.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di codice *
      *                      * da stampare                             *
      *                      *-----------------------------------------*
           if        w-stp-sub-tip-cod    =    "A"
                     go to stp-sub-int-470.
       stp-sub-int-465.
      *                      *-----------------------------------------*
      *                      * Se tipo di codice da stampare : numeri- *
      *                      * co                                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-sub-cod-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sub-cod-ned      .
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-ned    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-sub-int-475.
       stp-sub-int-470.
      *                      *-----------------------------------------*
      *                      * Se tipo di codice da stampare : alfanu- *
      *                      * merico                                  *
      *                      *-----------------------------------------*
       stp-sub-int-471.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del valore     *
      *                          *-------------------------------------*
           if        w-stp-sub-cod-alf    =    spaces
                     go to stp-sub-int-473.
       stp-sub-int-472.
      *                          *-------------------------------------*
      *                          * Se valore diverso da spaces         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-alf    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     stp-sub-int-475.
       stp-sub-int-473.
      *                          *-------------------------------------*
      *                          * Se valore a spaces                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing valore numerico         *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-sub-cod-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      spaces               to   w-stp-sub-cod-ned      .
           string    "("        delimited by   size
                     p-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-stp-sub-cod-ned      .
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-ned    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     stp-sub-int-475.
       stp-sub-int-475.
      *                      *-----------------------------------------*
      *                      * Appendice al codice                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se appendice a spaces : nessuna a-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-stp-sub-app-cod    =    spaces
                     go to stp-sub-int-480.
      *                          *-------------------------------------*
      *                          * Concatenamento trattino di separa-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "-"                  to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                          *-------------------------------------*
      *                          * Concatenamento appendice            *
      *                          *-------------------------------------*
           move      w-stp-sub-app-cod    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
       stp-sub-int-480.
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           add       2                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-des    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
       stp-sub-int-500.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di sub-intestazione,   *
      *                  * allineata al centro                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione iniziale area allineata *
      *                      * al centro                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sub-ads-sub      .
      *                      *-----------------------------------------*
      *                      * Determinazione posizione iniziale per   *
      *                      * l'area allineata al centro              *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-sub-wct-c01      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c01
                                  for trailing spaces                 .
           divide    2                    into w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                      *-----------------------------------------*
      *                      * Spostamento area allineata a sinistra   *
      *                      * in area allineata al centro             *
      *                      *-----------------------------------------*
           move      w-stp-sub-ads-asx    to   w-stp-sub-ads-sub
                                              (w-stp-sub-wct-c01 :)   .
       stp-sub-int-550.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           move      w-stp-sub-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sub-int-600.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sub-int-700.
      *              *-------------------------------------------------*
      *              * Linea di '=' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           add       30                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           subtract  15                   from p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sub-int-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la sub-sub-intestazione                    *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-sub-tip-cod : Tipo codice da stampare           *
      *    *                         - N : Numerico                    *
      *    *                         - A : Alfanumerico                *
      *    *                                                           *
      *    *   - w-stp-sub-cod-num : Valore codice, se numerico        *
      *    *                                                           *
      *    *   - w-stp-sub-cod-alf : Valore codice, se alfanumerico    *
      *    *                                                           *
      *    *   - w-stp-sub-app-cod : Valore appendice al codice        *
      *    *                                                           *
      *    *   - w-stp-sub-cod-des : Descrizione per il codice         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-ssi-int-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-ssi-int-999.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio assoluto                  *
      *              *-------------------------------------------------*
       stp-ssi-int-010.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di sub-sub-intesta-  *
      *                  * zione                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione Literal                     *
      *                      *-----------------------------------------*
           move      "Cliente :                     "
                                          to   w-stp-sub-lit-sub      .
       stp-ssi-int-030.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area di sub-inte-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-sub-ads-asx      .
           move      zero                 to   w-stp-sub-ads-lng      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-ads-lng
                                          for  all   spaces           .
       stp-ssi-int-040.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione di stampa iniziale *
      *                  * per l'area di sub-intestazione              *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-sub-pos-dsi      .
           subtract  w-stp-sub-ads-lng    from w-stp-sub-pos-dsi      .
           divide    2                    into w-stp-sub-pos-dsi      .
           add       1                    to   w-stp-sub-pos-dsi      .
       stp-ssi-int-100.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        rr-num-pdr           =    02 or
                     rr-num-pdr           =    03
                     if     p-res         >    12
                            go to stp-ssi-int-200
                     else   go to stp-ssi-int-150
           else      if     p-res         >    11
                            go to stp-ssi-int-200
                     else   go to stp-ssi-int-150.
       stp-ssi-int-150.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata in On : si esce *
      *              * immediatamente                                  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-ssi-int-999.
       stp-ssi-int-200.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale subordinato, per la-   *
      *              * sciare una interlinee di separazione            *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ssi-int-300.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           move      all   "="            to   w-stp-sub-ads-sub      .
           move      w-stp-sub-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ssi-int-400.
      *              *-------------------------------------------------*
      *              * Linea centrale di sub-sub-intestazione          *
      *              *-------------------------------------------------*
       stp-ssi-int-425.
      *                  *---------------------------------------------*
      *                  * Se codice numerico a zero e codice alfanu-  *
      *                  * merico a spaces si prepara la dicitura di   *
      *                  * non classificazione e si omette l'editing   *
      *                  * complesso                                   *
      *                  *---------------------------------------------*
       stp-ssi-int-427.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-stp-sub-cod-num    not  = zero   or
                     w-stp-sub-cod-alf    not  = spaces
                     go to stp-ssi-int-450.
      *                      *-----------------------------------------*
      *                      * Preparazione literal fisso              *
      *                      *-----------------------------------------*
           move      "Vendite non attribuite ad alcun cliente           
      -              "                      "
                                          to   w-stp-sub-ads-asx      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-ssi-int-500.
       stp-ssi-int-450.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di sub-intestazione,   *
      *                  * allineata a sinistra                        *
      *                  *---------------------------------------------*
       stp-ssi-int-455.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           move      w-stp-sub-lit-sub    to   w-stp-sub-ads-asx      .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
       stp-ssi-int-460.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del tipo di codice *
      *                      * da stampare                             *
      *                      *-----------------------------------------*
           if        w-stp-sub-tip-cod    =    "A"
                     go to stp-ssi-int-470.
       stp-ssi-int-465.
      *                      *-----------------------------------------*
      *                      * Se tipo di codice da stampare : numeri- *
      *                      * co                                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-sub-cod-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-sub-cod-ned      .
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-ned    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-ssi-int-475.
       stp-ssi-int-470.
      *                      *-----------------------------------------*
      *                      * Se tipo di codice da stampare : alfanu- *
      *                      * merico                                  *
      *                      *-----------------------------------------*
       stp-ssi-int-471.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del valore     *
      *                          *-------------------------------------*
           if        w-stp-sub-cod-alf    =    spaces
                     go to stp-ssi-int-473.
       stp-ssi-int-472.
      *                          *-------------------------------------*
      *                          * Se valore diverso da spaces         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-alf    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     stp-ssi-int-475.
       stp-ssi-int-473.
      *                          *-------------------------------------*
      *                          * Se valore a spaces                  *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing valore numerico         *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-sub-cod-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      spaces               to   w-stp-sub-cod-ned      .
           string    "("        delimited by   size
                     p-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-stp-sub-cod-ned      .
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           add       1                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-ned    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     stp-ssi-int-475.
       stp-ssi-int-475.
      *                      *-----------------------------------------*
      *                      * Appendice al codice                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se appendice a spaces : nessuna a-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-stp-sub-app-cod    =    spaces
                     go to stp-ssi-int-480.
      *                          *-------------------------------------*
      *                          * Concatenamento trattino di separa-  *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      "-"                  to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                          *-------------------------------------*
      *                          * Concatenamento appendice            *
      *                          *-------------------------------------*
           move      w-stp-sub-app-cod    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-sub-wct-c02      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c02
                                  for trailing spaces                 .
           move      w-stp-sub-ads-lng    to   w-stp-sub-wct-c01      .
           subtract  w-stp-sub-wct-c02    from w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
       stp-ssi-int-480.
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           add       2                    to   w-stp-sub-wct-c01      .
           move      w-stp-sub-cod-des    to   w-stp-sub-ads-asx
                                              (w-stp-sub-wct-c01 :)   .
       stp-ssi-int-500.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di sub-intestazione,   *
      *                  * allineata al centro                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione iniziale area allineata *
      *                      * al centro                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-sub-ads-sub      .
      *                      *-----------------------------------------*
      *                      * Determinazione posizione iniziale per   *
      *                      * l'area allineata al centro              *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-sub-wct-c01      .
           inspect   w-stp-sub-ads-asx
                                      tallying w-stp-sub-wct-c01
                                  for trailing spaces                 .
           divide    2                    into w-stp-sub-wct-c01      .
           add       1                    to   w-stp-sub-wct-c01      .
      *                      *-----------------------------------------*
      *                      * Spostamento area allineata a sinistra   *
      *                      * in area allineata al centro             *
      *                      *-----------------------------------------*
           move      w-stp-sub-ads-asx    to   w-stp-sub-ads-sub
                                              (w-stp-sub-wct-c01 :)   .
       stp-ssi-int-550.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           move      w-stp-sub-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ssi-int-600.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ssi-int-700.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sub-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sub-pos-dsi    to   p-pos                  .
           move      all   "-"            to   w-stp-sub-ads-sub      .
           move      w-stp-sub-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ssi-int-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la fincatura verticale della pagina        *
      *    *-----------------------------------------------------------*
       stp-fnc-vrt-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-fnc-vrt-900.
       stp-fnc-vrt-005.
      *              *-------------------------------------------------*
      *              * Preparazioni preliminari alla prima fincatura   *
      *              * in assoluto                                     *
      *              *-------------------------------------------------*
       stp-fnc-vrt-010.
      *                  *---------------------------------------------*
      *                  * Preparazione titolo di sinistra per la fin- *
      *                  * catura, allineato a sinistra                *
      *                  *---------------------------------------------*
       stp-fnc-vrt-015.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di fine *
      *                      * ciclo                                   *
      *                      *-----------------------------------------*
           if        w-liv-gen-flg-end    not  = spaces
                     go to stp-fnc-vrt-025.
       stp-fnc-vrt-020.
      *                      *-----------------------------------------*
      *                      * Se non si e' in fine ciclo              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "Prodotto                                          
      -              "      "             to   w-stp-fnc-tit-asx      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-fnc-vrt-040.
       stp-fnc-vrt-025.
      *                      *-----------------------------------------*
      *                      * Se si e' in fine ciclo                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal a spaces                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-fnc-tit-asx      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-fnc-vrt-040.
       stp-fnc-vrt-040.
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza del titolo   *
      *                  * di sinistra per la fincatura allineato a    *
      *                  * sinistra, senza considerare gli spazi in    *
      *                  * coda                                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-fnc-wct-c01      .
           inspect   w-stp-fnc-tit-asx
                                      tallying w-stp-fnc-wct-c01
                                  for trailing spaces                 .
           move      56                   to   w-stp-fnc-tit-asl      .
           subtract  w-stp-fnc-wct-c01    from w-stp-fnc-tit-asl      .
           if        w-stp-fnc-tit-asl    =    zero
                     move  1              to   w-stp-fnc-tit-asl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della posizione di stampa    *
      *                  * per il titolo di sinistra della fincatura   *
      *                  *---------------------------------------------*
           move      56                   to   w-stp-fnc-tit-asc      .
           subtract  w-stp-fnc-tit-asl    from w-stp-fnc-tit-asc      .
           divide    2                    into w-stp-fnc-tit-asc      .
           add       1                    to   w-stp-fnc-tit-asc      .
       stp-fnc-vrt-200.
      *              *-------------------------------------------------*
      *              * Stampa effettiva                                *
      *              *-------------------------------------------------*
       stp-fnc-vrt-205.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di perio- *
      *                  * di di riferimento                           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    01 or
                     rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to stp-fnc-vrt-210
           else if   rr-num-pdr           =    02
                     go to stp-fnc-vrt-245
           else if   rr-num-pdr           =    03
                     go to stp-fnc-vrt-290.
       stp-fnc-vrt-210.
      *                  *---------------------------------------------*
      *                  * Se 1 periodo di riferimento                 *
      *                  *---------------------------------------------*
       stp-fnc-vrt-215.
      *                      *-----------------------------------------*
      *                      * Test se numero linee residue sufficien- *
      *                      * ti                                      *
      *                      *-----------------------------------------*
           if        p-res                >    6
                     go to stp-fnc-vrt-220.
      *                      *-----------------------------------------*
      *                      * Reintestazione                          *
      *                      *-----------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata in On : *
      *                      * si esce immediatamente                  *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-fnc-vrt-999.
       stp-fnc-vrt-220.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato,   *
      *                      * per lasciare una interlinea di sepa-    *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-225.
      *                      *-----------------------------------------*
      *                      * Stampa della linea di fincatura         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Titolo di sinistra                  *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-fnc-tit-asl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-fnc-tit-asc    to   p-pos                  .
           move      w-stp-fnc-tit-asx    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Parte di destra                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "Udm    Quantita'       Fatturato                  
      -              "   Note                  "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-230.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-235.
      *                      *-----------------------------------------*
      *                      * Stampa della linea di sottolineatura    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Per titolo di sinistra              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Per parte destra                    *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "-------------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-240.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale, per non la-   *
      *                      * sciare alcuna interlinea di separazione *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       1                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * A fine fincatura                        *
      *                      *-----------------------------------------*
           go to     stp-fnc-vrt-900.
       stp-fnc-vrt-245.
      *                  *---------------------------------------------*
      *                  * Se 2 periodi di riferimento                 *
      *                  *---------------------------------------------*
       stp-fnc-vrt-250.
      *                      *-----------------------------------------*
      *                      * Test se numero linee residue sufficien- *
      *                      * ti                                      *
      *                      *-----------------------------------------*
           if        p-res                >    7
                     go to stp-fnc-vrt-255.
      *                      *-----------------------------------------*
      *                      * Reintestazione                          *
      *                      *-----------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata in On : *
      *                      * si esce immediatamente                  *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-fnc-vrt-999.
       stp-fnc-vrt-255.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato,   *
      *                      * per lasciare una interlinea di sepa-    *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-260.
      *                      *-----------------------------------------*
      *                      * Stampa della 1. linea di fincatura      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Parte di destra                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "              1. Periodo                      2. P
      -              "eriodo            Variaz."
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-265.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-270.
      *                      *-----------------------------------------*
      *                      * Stampa della 2. linea di fincatura      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Titolo di sinistra                  *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-fnc-tit-asl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-fnc-tit-asc    to   p-pos                  .
           move      w-stp-fnc-tit-asx    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Parte di destra                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "Udm    Quantita'       Fatturato       Quantita'  
      -              "     Fatturato        %  "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-275.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-280.
      *                      *-----------------------------------------*
      *                      * Stampa della linea di sottolineatura    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Per titolo di sinistra              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Per parte destra                    *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "- --------------- -------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-285.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale, per non la-   *
      *                      * sciare alcuna interlinea di separazione *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       1                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * A fine fincatura                        *
      *                      *-----------------------------------------*
           go to     stp-fnc-vrt-900.
       stp-fnc-vrt-290.
      *                  *---------------------------------------------*
      *                  * Se 3 periodi di riferimento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se numero linee residue sufficien- *
      *                      * ti                                      *
      *                      *-----------------------------------------*
           if        p-res                >    7
                     go to stp-fnc-vrt-295.
      *                      *-----------------------------------------*
      *                      * Reintestazione                          *
      *                      *-----------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione forzata in On : *
      *                      * si esce immediatamente                  *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-fnc-vrt-999.
       stp-fnc-vrt-295.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale subordinato,   *
      *                      * per lasciare una interlinea di sepa-    *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-300.
      *                      *-----------------------------------------*
      *                      * Stampa della 1. linea di fincatura      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Parte di destra                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      141                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "              1. Periodo                      2. P
      -              "eriodo            Variaz.          3. Periodo     
      -              "       Variaz.                           "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-305.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-310.
      *                      *-----------------------------------------*
      *                      * Stampa della 2. linea di fincatura      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Titolo di sinistra                  *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-fnc-tit-asl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-fnc-tit-asc    to   p-pos                  .
           move      w-stp-fnc-tit-asx    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Parte di destra                     *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      141                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "Udm    Quantita'       Fatturato       Quantita'  
      -              "     Fatturato        %      Quantita'       Fattu
      -              "rato        %             Note           "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-315.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-320.
      *                      *-----------------------------------------*
      *                      * Stampa della linea di sottolineatura    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Per titolo di sinistra              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Per parte di destra                 *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      141                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "- --------------- ------- --------------- --------
      -              "------- ------- -------------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fnc-vrt-325.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale, per non la-   *
      *                      * sciare alcuna interlinea di separazione *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       1                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * A fine fincatura                        *
      *                      *-----------------------------------------*
           go to     stp-fnc-vrt-900.
       stp-fnc-vrt-900.
      *              *-------------------------------------------------*
      *              * Post-fincatura                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero totale fincature stampate *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-fnc-num-fnc      .
       stp-fnc-vrt-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la fincatura di sopralineatura totali      *
      *    *-----------------------------------------------------------*
       stp-fds-tot-000.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        p-res                >    2
                     go to stp-fds-tot-100.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata in On : si esce *
      *              * immediatamente                                  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-fds-tot-999.
       stp-fds-tot-100.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale subordinato, per non   *
      *              * lasciare alcuna interlinea di separazione       *
      *              *-------------------------------------------------*
           move      "VS"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       1                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-fds-tot-400.
      *              *-------------------------------------------------*
      *              * Stampa effettiva                                *
      *              *-------------------------------------------------*
       stp-fds-tot-405.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di perio- *
      *                  * di di riferimento                           *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    01 or
                     rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to stp-fds-tot-410
           else if   rr-num-pdr           =    02
                     go to stp-fds-tot-420
           else if   rr-num-pdr           =    03
                     go to stp-fds-tot-430.
       stp-fds-tot-410.
      *                  *---------------------------------------------*
      *                  * Se 1 periodo di riferimento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo di sinistra                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Parte di destra                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "-------------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     stp-fds-tot-999.
       stp-fds-tot-420.
      *                  *---------------------------------------------*
      *                  * Se 2 periodi di riferimento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo di sinistra                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Parte di destra                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      75                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "- --------------- -------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     stp-fds-tot-999.
       stp-fds-tot-430.
      *                  *---------------------------------------------*
      *                  * Se 3 periodi di riferimento                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo di sinistra                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      56                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Parte di destra                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      141                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      "--- --------------- --------------- --------------
      -              "- --------------- ------- --------------- --------
      -              "------- ------- -------------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     stp-fds-tot-999.
       stp-fds-tot-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la fincatura di sopralineatura totali con  *
      *    * solo controllo di salto pagina                            *
      *    *-----------------------------------------------------------*
       stp-fds-nul-000.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        p-res                >    1
                     go to stp-fds-nul-100.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata in On : si esce *
      *              * immediatamente                                  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-fds-nul-999.
       stp-fds-nul-100.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-fds-nul-999.
       stp-fds-nul-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la stampa del codice prodotto, della sua   *
      *    * descrizione                                               *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-ced-pro-num : Codice numerico prodotto          *
      *    *                                                           *
      *    *   - w-stp-ced-pro-alf : Codice alfanumerico prodotto      *
      *    *                                                           *
      *    *   - w-stp-ced-pro-des : Descrizione del prodotto          *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-ced-pro-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-ced-pro-999.
      *              *-------------------------------------------------*
      *              * Codice prodotto                                 *
      *              *-------------------------------------------------*
       stp-ced-pro-050.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore del codice  *
      *                  * alfanumerico del prodotto                   *
      *                  *---------------------------------------------*
           if        w-stp-ced-pro-alf    =    spaces
                     go to stp-ced-pro-150.
       stp-ced-pro-100.
      *                  *---------------------------------------------*
      *                  * Se codice alfanumerico del prodotto diverso *
      *                  * da spaces                                   *
      *                  *---------------------------------------------*
       stp-ced-pro-105.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-ced-pro-alf    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ced-pro-110.
      *                      *-----------------------------------------*
      *                      * A trattamento descrizione               *
      *                      *-----------------------------------------*
           go to     stp-ced-pro-300.
       stp-ced-pro-150.
      *                  *---------------------------------------------*
      *                  * Se codice alfanumerico del prodotto pari a  *
      *                  * spaces                                      *
      *                  *---------------------------------------------*
       stp-ced-pro-155.
      *                      *-----------------------------------------*
      *                      * Costruzione area editata di comodo con  *
      *                      * il codice numerico del prodotto         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing codice numerico prodotto    *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-ced-pro-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Composizione area editata           *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-ced-pro-e40      .
           string    "(Cod. "   delimited by   size
                     p-edt      delimited by   spaces
                     ")"        delimited by   size
                                          into w-stp-ced-pro-e40      .
       stp-ced-pro-160.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-ced-pro-e40    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ced-pro-165.
      *                      *-----------------------------------------*
      *                      * A trattamento descrizione               *
      *                      *-----------------------------------------*
           go to     stp-ced-pro-300.
       stp-ced-pro-300.
      *              *-------------------------------------------------*
      *              * Descrizione prodotto                            *
      *              *-------------------------------------------------*
       stp-ced-pro-350.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della de-   *
      *                  * scrizione del prodotto                      *
      *                  *---------------------------------------------*
           if        w-stp-ced-pro-des    =    spaces
                     go to stp-ced-pro-450.
       stp-ced-pro-400.
      *                  *---------------------------------------------*
      *                  * Se descrizione del prodotto diversa da spa- *
      *                  * ces                                         *
      *                  *---------------------------------------------*
       stp-ced-pro-405.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-stp-ced-pro-des    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ced-pro-410.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     stp-ced-pro-600.
       stp-ced-pro-450.
      *                  *---------------------------------------------*
      *                  * Se descrizione del prodotto pari a spaces   *
      *                  *---------------------------------------------*
       stp-ced-pro-460.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se il codice nume- *
      *                      * rico del prodotto e' gia' stato stampa- *
      *                      * to oppure no                            *
      *                      *-----------------------------------------*
           if        w-stp-ced-pro-alf    =    spaces
                     go to stp-ced-pro-480.
       stp-ced-pro-465.
      *                      *-----------------------------------------*
      *                      * Se il codice numerico del prodotto e'   *
      *                      * gia' stato stampato                     *
      *                      *-----------------------------------------*
       stp-ced-pro-470.
      *                          *-------------------------------------*
      *                          * Stampa literal di puntini           *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      all   "."            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ced-pro-475.
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-ced-pro-600.
       stp-ced-pro-480.
      *                      *-----------------------------------------*
      *                      * Se il codice numerico del prodotto non  *
      *                      * e' ancora stato stampato                *
      *                      *-----------------------------------------*
       stp-ced-pro-485.
      *                          *-------------------------------------*
      *                          * Costruzione area editata di comodo  *
      *                          * con il codice numerico del prodotto *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing codice numerico prodotto*
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-ced-pro-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Composizione area editata       *
      *                              *---------------------------------*
           move      spaces               to   w-stp-ced-pro-e40      .
           string    "Descrizione codice ("
                                delimited by   size
                     p-edt      delimited by   spaces
                     ") mancante !"
                                delimited by   size
                                          into w-stp-ced-pro-e40      .
       stp-ced-pro-490.
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-stp-ced-pro-e40    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ced-pro-495.
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-ced-pro-600.
       stp-ced-pro-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-ced-pro-999.
       stp-ced-pro-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per stampa voci Quantita', Fatturato, e %      *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-snq : Si/No stampa quantita'            *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-dec : Numero decimali per le quantita'  *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-umi : Unita' di misura per le quantita' *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-q01 : Quantita' 1                       *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-f01 : Valore fatturato 1                *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-q02 : Quantita' 2                       *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-f02 : Valore fatturato 2                *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-p01 : Valore % di variazione 1          *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-q03 : Quantita' 3                       *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-f03 : Valore fatturato 3                *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-p02 : Valore % di variazione 2          *
      *    *                                                           *
      *    *   - w-stp-sta-qfp-not : Valore del campo 'Note'           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-sta-qfp-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           =    "S"
                     go to stp-sta-qfp-999.
      *              *-------------------------------------------------*
      *              * Quantita' 1                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-qfp-snq    =    "N"
                     go to stp-sta-qfp-025.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      w-stp-sta-qfp-umi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Editing 1. quantita'                        *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q01    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      062                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-025.
      *              *-------------------------------------------------*
      *              * Valore fatturato 1                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f01    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      077                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-100.
      *              *-------------------------------------------------*
      *              * Se numero periodi di riferimento non superiore  *
      *              * a 1 : a stampa Note                             *
      *              *-------------------------------------------------*
           if        rr-num-pdr           not  = 02 and
                     rr-num-pdr           not  = 03
                     go to stp-sta-qfp-900.
       stp-sta-qfp-200.
      *              *-------------------------------------------------*
      *              * Quantita' 2                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-qfp-snq    =    "N"
                     go to stp-sta-qfp-225.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q02    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      094                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-225.
      *              *-------------------------------------------------*
      *              * Valore fatturato 2                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f02    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      109                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-250.
      *              *-------------------------------------------------*
      *              * Valore % di variazione 1                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-stp-sta-qfp-q01
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-qfp-q02
                                          to   w-stp-edt-pdv-2ov
           else      move  w-stp-sta-qfp-f01
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-qfp-f02
                                          to   w-stp-edt-pdv-2ov      .
           move      w-stp-sta-qfp-p01    to   w-stp-edt-pdv-pdv      .
           perform   stp-edt-pdv-000      thru stp-edt-pdv-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      126                  to   p-pos                  .
           move      w-stp-edt-pdv-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-300.
      *              *-------------------------------------------------*
      *              * Se numero periodi di riferimento non superiore  *
      *              * a 2 : a stampa Note                             *
      *              *-------------------------------------------------*
           if        rr-num-pdr           not  = 03
                     go to stp-sta-qfp-900.
       stp-sta-qfp-400.
      *              *-------------------------------------------------*
      *              * Quantita' 3                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-qfp-snq    =    "N"
                     go to stp-sta-qfp-425.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q03    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      134                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-425.
      *              *-------------------------------------------------*
      *              * Valore fatturato 3                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f03    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      149                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-450.
      *              *-------------------------------------------------*
      *              * Valore % di variazione 2                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-stp-sta-qfp-q02
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-qfp-q03
                                          to   w-stp-edt-pdv-2ov
           else      move  w-stp-sta-qfp-f02
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-qfp-f03
                                          to   w-stp-edt-pdv-2ov      .
           move      w-stp-sta-qfp-p02    to   w-stp-edt-pdv-pdv      .
           perform   stp-edt-pdv-000      thru stp-edt-pdv-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      166                  to   p-pos                  .
           move      w-stp-edt-pdv-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-900.
      *              *-------------------------------------------------*
      *              * Valore del campo 'Note'                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se lunghezza campo 'Note' a zero : uscita   *
      *                  *---------------------------------------------*
           if        w-stp-clc-not-lun    =    zero or
                     w-stp-clc-not-pos    =    zero
                     go to stp-sta-qfp-999.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-clc-not-lun    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-clc-not-pos    to   p-pos                  .
           move      w-stp-sta-qfp-not    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-qfp-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per archivio sequenziale                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       exe-gen-fil-000.
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        rr-gen-fil           not  = "S"
                     go to exe-gen-fil-999.
       exe-gen-fil-100.
      *              *-------------------------------------------------*
      *              * Composizione record di output                   *
      *              *-------------------------------------------------*
       exe-gen-fil-200.
      *              *-------------------------------------------------*
      *              * Campi di rottura                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione classe geografica               *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-liv-cgc-des-cgc    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale cliente                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-liv-cli-rag-cli    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Classe prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-liv-cla-des-cla    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Gruppo prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-liv-gru-des-gru    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione Sottogruppo prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-liv-sgr-des-sgr    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-300.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico del prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      14                   to   x-car                  .
           move      w-stp-ced-pro-alf    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Descrizione per il prodotto                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-stp-ced-pro-des    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Unita' di misura per il prodotto            *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      03                   to   x-car                  .
           move      w-stp-sta-qfp-umi    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-cli-500.
      *              *-------------------------------------------------*
      *              * Valore quantita' 1                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q01    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      15                   to   x-car                  .
           move      w-stp-edt-qta-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *              *-------------------------------------------------*
      *              * Valore fatturato 1                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f01    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      16                   to   x-car                  .
           move      w-stp-edt-fat-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-cli-520.
      *              *-------------------------------------------------*
      *              * Valore quantita' 2                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero periodi                      *
      *                  *---------------------------------------------*
           if        rr-num-pdr           <    2
                     go to exe-gen-fil-cli-800.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q02    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      15                   to   x-car                  .
           move      w-stp-edt-qta-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *              *-------------------------------------------------*
      *              * Valore fatturato 2                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f02    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      16                   to   x-car                  .
           move      w-stp-edt-fat-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-cli-530.
      *              *-------------------------------------------------*
      *              * Valore quantita' 3                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero periodi                      *
      *                  *---------------------------------------------*
           if        rr-num-pdr           <    3
                     go to exe-gen-fil-cli-800.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-qfp-q03    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      15                   to   x-car                  .
           move      w-stp-edt-qta-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *              *-------------------------------------------------*
      *              * Valore fatturato 3                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-qfp-f03    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
      *                  *---------------------------------------------*
      *                  * In campo di uscita                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      16                   to   x-car                  .
           move      w-stp-edt-fat-edt    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-cli-800.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
           move      "PR"                 to   x-ope                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       exe-gen-fil-cli-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-gen-fil-999.
       exe-gen-fil-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per stampa totali                              *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-sta-tot-lit : Literal per il totale             *
      *    *                                                           *
      *    *   - w-stp-sta-tot-lit : Leading character per quantita'   *
      *    *                         e valori, normalmente spaces      *
      *    *                                                           *
      *    *   - w-stp-sta-tot-snq : Si/No stampa quantita'            *
      *    *                                                           *
      *    *   - w-stp-sta-tot-dec : Numero decimali per la quantita', *
      *    *                         se pari a 9 : tanti decimali pa-  *
      *    *                         ri a quelli presenti nel valore   *
      *    *                                                           *
      *    *   - w-stp-sta-tot-umi : Unita' di misura                  *
      *    *                                                           *
      *    *   - w-stp-sta-tot-q01 : Quantita' 1                       *
      *    *                                                           *
      *    *   - w-stp-sta-tot-f01 : Valore fatturato 1                *
      *    *                                                           *
      *    *   - w-stp-sta-tot-q02 : Quantita' 2                       *
      *    *                                                           *
      *    *   - w-stp-sta-tot-f02 : Valore fatturato 2                *
      *    *                                                           *
      *    *   - w-stp-sta-tot-p01 : Valore % di variazione 1          *
      *    *                                                           *
      *    *   - w-stp-sta-tot-q03 : Quantita' 3                       *
      *    *                                                           *
      *    *   - w-stp-sta-tot-f03 : Valore fatturato 3                *
      *    *                                                           *
      *    *   - w-stp-sta-tot-p02 : Valore % di variazione 2          *
      *    *                                                           *
      *    *   - w-stp-sta-tot-not : Valore del campo 'Note'           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-sta-tot-000.
      *              *-------------------------------------------------*
      *              * Test se numero linee residue sufficienti        *
      *              *-------------------------------------------------*
           if        p-res                >    1
                     go to stp-sta-tot-050.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata in On : si esce *
      *              * immediatamente                                  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-sta-tot-999.
       stp-sta-tot-050.
      *              *-------------------------------------------------*
      *              * Interlinea di separazione                       *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-100.
      *              *-------------------------------------------------*
      *              * Literal per il totale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza del literal  *
      *                  * per il totale, allineato a sinistra, senza  *
      *                  * considerare gli spazi in coda               *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-sta-tot-c02      .
           inspect   w-stp-sta-tot-lit
                                      tallying w-stp-sta-tot-c02
                                  for trailing spaces                 .
           move      56                   to   w-stp-sta-tot-c01      .
           subtract  w-stp-sta-tot-c02    from w-stp-sta-tot-c01      .
      *                  *---------------------------------------------*
      *                  * Aggiunta dei due punti finali               *
      *                  *---------------------------------------------*
           add       2                    to   w-stp-sta-tot-c01      .
           move      ":"                  to   w-stp-sta-tot-lch
                                              (w-stp-sta-tot-c01)     .
      *                  *---------------------------------------------*
      *                  * Determinazione della posizione di stampa    *
      *                  * per il literal per il totale, in modo da    *
      *                  * allinearlo a destra                         *
      *                  *---------------------------------------------*
           move      56                   to   w-stp-sta-tot-c02      .
           subtract  w-stp-sta-tot-c01    from w-stp-sta-tot-c02      .
           add       1                    to   w-stp-sta-tot-c02      .
      *                  *---------------------------------------------*
      *                  * Stampa literal                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-sta-tot-c01    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-sta-tot-c02    to   p-pos                  .
           move      w-stp-sta-tot-lit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-200.
      *              *-------------------------------------------------*
      *              * Quantita' 1                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-tot-snq    =    "N"
                     go to stp-sta-tot-225.
      *                  *---------------------------------------------*
      *                  * Unita' di misura                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      058                  to   p-pos                  .
           move      w-stp-sta-tot-umi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Editing 1. quantita'                        *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-tot-q01    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-qta-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      062                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-225.
      *              *-------------------------------------------------*
      *              * Valore fatturato 1                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-f01    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-fat-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      077                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-300.
      *              *-------------------------------------------------*
      *              * Se numero periodi di riferimento non superiore  *
      *              * a 1 : a stampa Note                             *
      *              *-------------------------------------------------*
           if        rr-num-pdr           not  = 02 and
                     rr-num-pdr           not  = 03
                     go to stp-sta-tot-900.
       stp-sta-tot-400.
      *              *-------------------------------------------------*
      *              * Quantita' 2                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-tot-snq    =    "N"
                     go to stp-sta-tot-425.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-tot-q02    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-qta-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      094                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-425.
      *              *-------------------------------------------------*
      *              * Valore fatturato 2                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-f02    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-fat-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      109                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-450.
      *              *-------------------------------------------------*
      *              * Valore % di variazione 1                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-stp-sta-tot-q01
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-tot-q02
                                          to   w-stp-edt-pdv-2ov
           else      move  w-stp-sta-tot-f01
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-tot-f02
                                          to   w-stp-edt-pdv-2ov      .
           move      w-stp-sta-tot-p01    to   w-stp-edt-pdv-pdv      .
           perform   stp-edt-pdv-000      thru stp-edt-pdv-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      126                  to   p-pos                  .
           move      w-stp-edt-pdv-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-500.
      *              *-------------------------------------------------*
      *              * Se numero periodi di riferimento non superiore  *
      *              * a 2 : a stampa Note                             *
      *              *-------------------------------------------------*
           if        rr-num-pdr           not  = 03
                     go to stp-sta-tot-900.
       stp-sta-tot-600.
      *              *-------------------------------------------------*
      *              * Quantita' 2                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se 'No' stampa quantita' : a fatturato      *
      *                  *---------------------------------------------*
           if        w-stp-sta-tot-snq    =    "N"
                     go to stp-sta-tot-625.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-dec    to   w-stp-edt-qta-dec      .
           move      w-stp-sta-tot-q03    to   w-stp-edt-qta-qta      .
           perform   stp-edt-qta-000      thru stp-edt-qta-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-qta-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      134                  to   p-pos                  .
           move      w-stp-edt-qta-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-625.
      *              *-------------------------------------------------*
      *              * Valore fatturato 3                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      w-stp-sta-tot-f03    to   w-stp-edt-fat-fat      .
           perform   stp-edt-fat-000      thru stp-edt-fat-999        .
           if        w-stp-sta-tot-ldc    not  = spaces
                     inspect   w-stp-edt-fat-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-sta-tot-ldc      .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      149                  to   p-pos                  .
           move      w-stp-edt-fat-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-650.
      *              *-------------------------------------------------*
      *              * Valore % di variazione 2                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           if        rr-tpc-pdv           =    02
                     move  w-stp-sta-tot-q02
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-tot-q03
                                          to   w-stp-edt-pdv-2ov
           else      move  w-stp-sta-tot-f02
                                          to   w-stp-edt-pdv-1ov
                     move  w-stp-sta-tot-f03
                                          to   w-stp-edt-pdv-2ov      .
           move      w-stp-sta-tot-p02    to   w-stp-edt-pdv-pdv      .
           perform   stp-edt-pdv-000      thru stp-edt-pdv-999        .
      *                  *---------------------------------------------*
      *                  * In area di stampa                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      166                  to   p-pos                  .
           move      w-stp-edt-pdv-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-900.
      *              *-------------------------------------------------*
      *              * Valore del campo 'Note'                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se lunghezza campo 'Note' a zero : uscita   *
      *                  *---------------------------------------------*
           if        w-stp-clc-not-lun    =    zero or
                     w-stp-clc-not-pos    =    zero
                     go to stp-sta-tot-999.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-clc-not-lun    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-clc-not-pos    to   p-pos                  .
           move      w-stp-sta-tot-not    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-sta-tot-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per editing voce 'Quantita''                   *
      *    *-----------------------------------------------------------*
       stp-edt-qta-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : uscita con zero editato      *
      *              *-------------------------------------------------*
           if        w-stp-edt-qta-qta    =    zero
                     move  "          0    "
                                          to   w-stp-edt-qta-edt
                     go to stp-edt-qta-999.
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione numero decimali       *
      *              *-------------------------------------------------*
           if        w-stp-edt-qta-dec    not  = 9
                     if    w-stp-edt-qta-dec
                                          >    3
                           move   zero    to   w-stp-edt-qta-dec      .
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           if        w-stp-edt-qta-dec    =    9
                     move  3              to   p-dec
           else      move  w-stp-edt-qta-dec
                                          to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           if        w-stp-edt-qta-dec    =    9
                     move  "GD"           to   p-edm
           else      move  "G "           to   p-edm                  .
           move      w-stp-edt-qta-qta    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Valore editato in campo di destinazione         *
      *              *-------------------------------------------------*
           move      p-edt                to   w-stp-edt-qta-edt      .
       stp-edt-qta-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per editing voce 'Fatturato'                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/psvfedn0.cpy"                   .

      *    *===========================================================*
      *    * Subroutine per editing voce '% di variazione'             *
      *    *-----------------------------------------------------------*
       stp-edt-pdv-000.
      *              *-------------------------------------------------*
      *              * Se i due parametri sono entrambi a zero : usci- *
      *              * ta con spaces                                   *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-1ov    =    zero and
                     w-stp-edt-pdv-2ov    =    zero
                     move  "       "      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se i due parametri sono uguali : uscita con '=' *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-1ov    =    w-stp-edt-pdv-2ov
                     move  "      ="      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se il primo dei due parametri e' positivo ed il *
      *              * secondo e' negativo o zero : uscita con '-'     *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-1ov    >    zero  and
                     w-stp-edt-pdv-2ov    not  > zero
                     move  "      -"      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se il primo dei due parametri e' negativo ed il *
      *              * secondo e' positivo o zero : uscita con '+'     *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-1ov    <    zero  and
                     w-stp-edt-pdv-2ov    not  < zero
                     move  "      +"      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se il secondo dei due parametri e' positivo ed  *
      *              * il primo e' negativo o zero : uscita con '+'    *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-2ov    >    zero  and
                     w-stp-edt-pdv-1ov    not  > zero
                     move  "      +"      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se il secondo dei due parametri e' negativo ed  *
      *              * il primo e' positivo o zero : uscita con '-'    *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-2ov    <    zero  and
                     w-stp-edt-pdv-1ov    not  < zero
                     move  "      -"      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Se la % di variazione supera il limite del die- *
      *              * cimila per cento : uscita con '!'               *
      *              *-------------------------------------------------*
           if        w-stp-edt-pdv-pdv    >    9999,9 or
                     w-stp-edt-pdv-pdv    <   -9999,9
                     move  "      !"      to   w-stp-edt-pdv-edt
                     go to stp-edt-pdv-999.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      w-stp-edt-pdv-pdv    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Valore editato in campo di destinazione         *
      *              *-------------------------------------------------*
           move      p-edt                to   w-stp-edt-pdv-edt      .
       stp-edt-pdv-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per editing literal                            *
      *    *-----------------------------------------------------------*
       stp-edt-lit-000.
      *              *-------------------------------------------------*
      *              * Test su tipo di esportazione                    *
      *              *-------------------------------------------------*
           if        x-tex                =    01
                     go to stp-edt-lit-999.
       stp-edt-lit-100.
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "{"                  to   w-all-str-cat (1)      .
           move      w-stp-edt-fat-lit    to   w-all-str-cat (2)      .
           move      "}"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
           move      w-all-str-alf        to   w-stp-edt-fat-lit      .
       stp-edt-lit-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per calcolo percentuali di variazione          *
      *    *-----------------------------------------------------------*
       stp-clc-pdv-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero di periodi di   *
      *              * riferimento                                     *
      *              *-------------------------------------------------*
           if        rr-num-pdr           =    01 or
                     rr-num-pdr           =    11 or
                     rr-num-pdr           =    12
                     go to stp-clc-pdv-100
           else if   rr-num-pdr           =    02
                     go to stp-clc-pdv-200
           else if   rr-num-pdr           =    03
                     go to stp-clc-pdv-300.
       stp-clc-pdv-100.
      *              *-------------------------------------------------*
      *              * Se numero di periodi di riferimento : 01        *
      *              *-------------------------------------------------*
       stp-clc-pdv-110.
      *                  *---------------------------------------------*
      *                  * % di variazione 1 : a zero                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-clc-pdv-p01      .
       stp-clc-pdv-120.
      *                  *---------------------------------------------*
      *                  * % di variazione 2 : a zero                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-clc-pdv-p02      .
       stp-clc-pdv-130.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-clc-pdv-999.
       stp-clc-pdv-200.
      *              *-------------------------------------------------*
      *              * Se numero di periodi di riferimento : 02        *
      *              *-------------------------------------------------*
       stp-clc-pdv-210.
      *                  *---------------------------------------------*
      *                  * Calcolo % di variazione 1                   *
      *                  *---------------------------------------------*
           if        w-stp-clc-pdv-v01    =    zero or
                     w-stp-clc-pdv-v02    =    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-220.
           if        w-stp-clc-pdv-v01    >    zero and
                     w-stp-clc-pdv-v02    <    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-220.
           if        w-stp-clc-pdv-v01    <    zero and
                     w-stp-clc-pdv-v02    >    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-220.
           move      w-stp-clc-pdv-v02    to   w-stp-clc-pdv-p01      .
           subtract  w-stp-clc-pdv-v01    from w-stp-clc-pdv-p01      .
           multiply  100                  by   w-stp-clc-pdv-p01      .
           divide    w-stp-clc-pdv-v01    into w-stp-clc-pdv-p01
                                                         rounded      .
       stp-clc-pdv-220.
      *                  *---------------------------------------------*
      *                  * % di variazione 2 : a zero                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-clc-pdv-p02      .
       stp-clc-pdv-230.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-clc-pdv-999.
       stp-clc-pdv-300.
      *              *-------------------------------------------------*
      *              * Se numero di periodi di riferimento : 03        *
      *              *-------------------------------------------------*
       stp-clc-pdv-310.
      *                  *---------------------------------------------*
      *                  * Calcolo % di variazione 1                   *
      *                  *---------------------------------------------*
           if        w-stp-clc-pdv-v01    =    zero or
                     w-stp-clc-pdv-v02    =    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-320.
           if        w-stp-clc-pdv-v01    >    zero and
                     w-stp-clc-pdv-v02    <    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-320.
           if        w-stp-clc-pdv-v01    <    zero and
                     w-stp-clc-pdv-v02    >    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-320.
           move      w-stp-clc-pdv-v02    to   w-stp-clc-pdv-p01      .
           subtract  w-stp-clc-pdv-v01    from w-stp-clc-pdv-p01      .
           multiply  100                  by   w-stp-clc-pdv-p01      .
           divide    w-stp-clc-pdv-v01    into w-stp-clc-pdv-p01
                                                         rounded      .
       stp-clc-pdv-320.
      *                  *---------------------------------------------*
      *                  * Calcolo % di variazione 2                   *
      *                  *---------------------------------------------*
           if        w-stp-clc-pdv-v02    =    zero or
                     w-stp-clc-pdv-v03    =    zero
                     move  zero           to   w-stp-clc-pdv-p02
                     go to stp-clc-pdv-330.
           if        w-stp-clc-pdv-v02    >    zero and
                     w-stp-clc-pdv-v03    <    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-330.
           if        w-stp-clc-pdv-v02    <    zero and
                     w-stp-clc-pdv-v03    >    zero
                     move  zero           to   w-stp-clc-pdv-p01
                     go to stp-clc-pdv-330.
           move      w-stp-clc-pdv-v03    to   w-stp-clc-pdv-p02      .
           subtract  w-stp-clc-pdv-v02    from w-stp-clc-pdv-p02      .
           multiply  100                  by   w-stp-clc-pdv-p02      .
           divide    w-stp-clc-pdv-v02    into w-stp-clc-pdv-p02
                                                         rounded      .
       stp-clc-pdv-330.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-clc-pdv-999.
       stp-clc-pdv-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per calcolo caratteristiche campo 'Note'       *
      *    *-----------------------------------------------------------*
       stp-clc-not-000.
      *              *-------------------------------------------------*
      *              * Lunghezza del campo                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre in funzione del numero di periodi di *
      *                  * riferimento, in quanto questa statistica    *
      *                  * non prevede la stampa del dettaglio dei do- *
      *                  * cumenti                                     *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    02
                     move  000            to   w-stp-clc-not-lun
           else if   rr-num-pdr           =    03
                     move  033            to   w-stp-clc-not-lun
           else      move  039            to   w-stp-clc-not-lun      .
       stp-clc-not-500.
      *              *-------------------------------------------------*
      *              * Posizione di stampa per il campo                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre in funzione del numero di periodi di *
      *                  * riferimento, in quanto questa statistica    *
      *                  * non prevede la stampa del dettaglio dei do- *
      *                  * cumenti                                     *
      *                  *---------------------------------------------*
           if        rr-num-pdr           =    02
                     move  000            to   w-stp-clc-not-pos
           else if   rr-num-pdr           =    03
                     move  174            to   w-stp-clc-not-pos
           else      move  094            to   w-stp-clc-not-pos      .
       stp-clc-not-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxn]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxn-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    spaces
                     go to let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    w-let-arc-gxn-exc
                     go to let-arc-gxn-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      w-let-arc-gxn-cod    to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxn-400.
       let-arc-gxn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxn-des-naz       to   w-let-arc-gxn-des      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxn-900.
       let-arc-gxn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxn-flg      .
           move      all   "."            to   w-let-arc-gxn-des      .
           go to     let-arc-gxn-900.
       let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
           move      spaces               to   w-let-arc-gxn-des      .
       let-arc-gxn-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxn-cod    to   w-let-arc-gxn-exc      .
       let-arc-gxn-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxr]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxr-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-gxr-cod    =    spaces
                     go to let-arc-gxr-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxr-cod    =    w-let-arc-gxr-exc
                     go to let-arc-gxr-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxr-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxr-cod    to   rf-gxr-cod-rgn         .
           move      "pgm/geo/fls/ioc/obj/iofgxr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxr                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxr-400.
       let-arc-gxr-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxr-des-rgn       to   w-let-arc-gxr-des      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxr-900.
       let-arc-gxr-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxr-flg      .
           move      all   "."            to   w-let-arc-gxr-des      .
           go to     let-arc-gxr-900.
       let-arc-gxr-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxr-flg      .
           move      spaces               to   w-let-arc-gxr-des      .
       let-arc-gxr-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxr-cod    to   w-let-arc-gxr-exc      .
       let-arc-gxr-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxp]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxp-000.
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    spaces
                     go to let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    w-let-arc-gxp-exc
                     go to let-arc-gxp-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxp-cod    to   rf-gxp-cod-prv         .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxp-400.
       let-arc-gxp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxp-des-prv       to   w-let-arc-gxp-des      .
           move      rf-gxp-cod-rgn       to   w-let-arc-gxp-rgn      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxp-900.
       let-arc-gxp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxp-flg      .
           move      all   "."            to   w-let-arc-gxp-des      .
           move      spaces               to   w-let-arc-gxp-rgn      .
           go to     let-arc-gxp-900.
       let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
           move      spaces               to   w-let-arc-gxp-des      .
           move      spaces               to   w-let-arc-gxp-rgn      .
       let-arc-gxp-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxp-cod    to   w-let-arc-gxp-exc      .
       let-arc-gxp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [gxc]                         *
      *    *-----------------------------------------------------------*
       let-arc-gxc-000.
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-cod    =    zero
                     go to let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-gxc-cod    =    w-let-arc-gxc-exc
                     go to let-arc-gxc-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCFL    "         to   f-key                  .
           move      w-let-arc-gxc-cod    to   rf-gxc-cod-cmn         .
           move      zero                 to   rf-gxc-cod-fzn         .
           move      zero                 to   rf-gxc-cod-lct         .
           move      "pgm/geo/fls/ioc/obj/iofgxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxc-400.
       let-arc-gxc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxc-des-cfl       to   w-let-arc-gxc-des      .
           move      rf-gxc-cod-prv       to   w-let-arc-gxc-prv      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-gxc-900.
       let-arc-gxc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxc-flg      .
           move      all   "."            to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
           go to     let-arc-gxc-900.
       let-arc-gxc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxc-flg      .
           move      spaces               to   w-let-arc-gxc-des      .
           move      spaces               to   w-let-arc-gxc-prv      .
       let-arc-gxc-900.
      *              *-------------------------------------------------*
      *              * Tipo e codice letti su valori precedenti        *
      *              *-------------------------------------------------*
           move      w-let-arc-gxc-cod    to   w-let-arc-gxc-exc      .
       let-arc-gxc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli] e [dcc]                 *
      *    *-----------------------------------------------------------*
       let-cli-dcc-000.
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    zero
                     go to let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Test se codice e dipendenza pari ai valori pre- *
      *              * cedenti : ad uscita                             *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    w-let-cli-dcc-exc and
                     w-let-cli-dcc-dpz    =    w-let-cli-dcc-exd
                     go to let-cli-dcc-990.
       let-cli-dcc-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-mne      .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-naz      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-cli-dcc-cmn      .
      *                  *---------------------------------------------*
      *                  * Codice regione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rgn      .
      *                  *---------------------------------------------*
      *                  * Codice provincia                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-prv      .
       let-cli-dcc-200.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [cli]                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione di :                         *
      *                  *   - ragione sociale                         *
      *                  *   - mnemonico                               *
      *                  *   - codice nazione                          *
      *                  *   - codice comune                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-cli-rag-soc to   w-let-cli-dcc-rag
                     move  rf-cli-cod-mne to   w-let-cli-dcc-mne
                     move  rf-cli-cod-naz to   w-let-cli-dcc-naz
                     move  rf-cli-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-300.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [dcc] principale            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione di :                         *
      *                  *   - ragione sociale                         *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-rag-soc to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codici :                     *
      *                  *  - Nazione                                  *
      *                  *  - Comune                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-cod-naz to   w-let-cli-dcc-naz
                     move  rf-dcc-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-400.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [dcc] per la dipendenza     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non c'e' dipendenza : nessuna azione     *
      *                  *---------------------------------------------*
           if        w-let-cli-dcc-dpz    =    spaces
                     go to let-cli-dcc-500.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-cli-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-cli-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice :                     *
      *                  *  - Nazione                                  *
      *                  *  - Comune                                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     move  rf-dcc-cod-naz to   w-let-cli-dcc-naz
                     move  rf-dcc-cod-cmn to   w-let-cli-dcc-cmn      .
       let-cli-dcc-500.
      *              *-------------------------------------------------*
      *              * A pre-uscita                                    *
      *              *-------------------------------------------------*
           go to     let-cli-dcc-900.
       let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-rag      .
      *                  *---------------------------------------------*
      *                  * Mnemonico                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-mne      .
      *                  *---------------------------------------------*
      *                  * Codice nazione                              *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-cli-dcc-naz      .
      *                  *---------------------------------------------*
      *                  * Codice comune                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-cli-dcc-cmn      .
       let-cli-dcc-900.
      *              *-------------------------------------------------*
      *              * Pre-uscita                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice comune                       *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-cmn    to   w-let-arc-gxc-cod      .
           perform   let-arc-gxc-000      thru let-arc-gxc-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice provincia             *
      *                  *---------------------------------------------*
           move      w-let-arc-gxc-prv    to   w-let-cli-dcc-prv      .
      *                  *---------------------------------------------*
      *                  * Lettura codice provincia                    *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-prv    to   w-let-arc-gxp-cod      .
           perform   let-arc-gxp-000      thru let-arc-gxp-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione codice regione               *
      *                  *---------------------------------------------*
           move      w-let-arc-gxp-rgn    to   w-let-cli-dcc-rgn      .
       let-cli-dcc-990.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice e dipendenza letti su valori prece-  *
      *                  * denti                                       *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-cli    to   w-let-cli-dcc-exc      .
           move      w-let-cli-dcc-dpz    to   w-let-cli-dcc-exd      .
       let-cli-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-cod    =    w-let-arc-dcp-exc
                     go to let-arc-dcp-900.
       let-arc-dcp-100.
      *              *-------------------------------------------------*
      *              * Se codice a zero                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-cod    not  = zero
                     go to let-arc-dcp-200.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-dec      .
           move      zero                 to   w-let-arc-dcp-cla      .
           move      zero                 to   w-let-arc-dcp-gru      .
           move      zero                 to   w-let-arc-dcp-sgr      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-dcp-900.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Lettura da archivio [dcp]                       *
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
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-300.
      *              *-------------------------------------------------*
      *              * Se record esistente in archivio [dcp]           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a spaces                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori                       *
      *                  *---------------------------------------------*
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-dec      .
           move      rf-dcp-cla-pro       to   w-let-arc-dcp-cla      .
           move      rf-dcp-gru-pro       to   w-let-arc-dcp-gru      .
           move      rf-dcp-sgr-pro       to   w-let-arc-dcp-sgr      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-dcp-900.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Se record non esistente in archivio [dcp]       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita a '#'                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori                      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-dec      .
           move      zero                 to   w-let-arc-dcp-cla      .
           move      zero                 to   w-let-arc-dcp-gru      .
           move      zero                 to   w-let-arc-dcp-sgr      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-dcp-900.
       let-arc-dcp-900.
      *              *-------------------------------------------------*
      *              * Codice numerico letto su valore precedente      *
      *              *-------------------------------------------------*
           move      w-let-arc-dcp-cod    to   w-let-arc-dcp-exc      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp1]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp1-000.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-zp1-cla    =    w-let-arc-zp1-exc
                     go to let-arc-zp1-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp1-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice classe a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp1-cla    =    zero
                     go to let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLA    "         to   f-key                  .
           move      w-let-arc-zp1-cla    to   rf-zp1-cod-cla         .
           move      "pgm/dcp/fls/ioc/obj/iofzp1"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp1                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp1-400.
       let-arc-zp1-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp1-des-cla       to   w-let-arc-zp1-des      .
           move      rf-zp1-mne-cla       to   w-let-arc-zp1-mne      .
           move      rf-zp1-ult-sud       to   w-let-arc-zp1-sud      .
           move      rf-zp1-sqz-num       to   w-let-arc-zp1-sqz      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-zp1-900.
       let-arc-zp1-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp1-flg      .
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-let-arc-zp1-cla    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      spaces               to   w-let-arc-zp1-des      .
           string    "Classe codice ("
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     ")"
                                delimited by   size
                                          into w-let-arc-zp1-des      .
           move      p-edt                to   w-let-arc-zp1-mne      .
           go to     let-arc-zp1-600.
       let-arc-zp1-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      "Prodotti non classificati                "
                                          to   w-let-arc-zp1-des      .
       let-arc-zp1-600.
           move      spaces               to   w-let-arc-zp1-mne      .
           move      zero                 to   w-let-arc-zp1-sud      .
           move      zero                 to   w-let-arc-zp1-sqz      .
       let-arc-zp1-900.
      *              *-------------------------------------------------*
      *              * Codice numerico letto su valore precedente      *
      *              *-------------------------------------------------*
           move      w-let-arc-zp1-cla    to   w-let-arc-zp1-exc      .
       let-arc-zp1-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp2]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp2-000.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-zp2-cla    =    w-let-arc-zp2-exc and
                     w-let-arc-zp2-gru    =    w-let-arc-zp2-exg
                     go to let-arc-zp2-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice gruppo a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zp2-cla    =    zero or
                     w-let-arc-zp2-gru    =    zero
                     go to let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGRU    "         to   f-key                  .
           move      w-let-arc-zp2-cla    to   rf-zp2-cod-cla         .
           move      w-let-arc-zp2-gru    to   rf-zp2-cod-gru         .
           move      "pgm/dcp/fls/ioc/obj/iofzp2"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp2                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp2-400.
       let-arc-zp2-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp2-des-gru       to   w-let-arc-zp2-des      .
           move      rf-zp2-mne-gru       to   w-let-arc-zp2-mne      .
           move      rf-zp2-ult-sud       to   w-let-arc-zp2-sud      .
           move      rf-zp2-sqz-num       to   w-let-arc-zp2-sqz      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-zp2-900.
       let-arc-zp2-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp2-flg      .
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-let-arc-zp2-gru    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      spaces               to   w-let-arc-zp2-des      .
           string    "Gruppo codice ("
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     ")"
                                delimited by   size
                                          into w-let-arc-zp2-des      .
           move      p-edt                to   w-let-arc-zp2-mne      .
           go to     let-arc-zp2-600.
       let-arc-zp2-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp2-des      .
       let-arc-zp2-600.
           move      spaces               to   w-let-arc-zp2-mne      .
           move      zero                 to   w-let-arc-zp2-sud      .
           move      zero                 to   w-let-arc-zp2-sqz      .
       let-arc-zp2-900.
      *              *-------------------------------------------------*
      *              * Codice numerico letto su valore precedente      *
      *              *-------------------------------------------------*
           move      w-let-arc-zp2-cla    to   w-let-arc-zp2-exc      .
           move      w-let-arc-zp2-gru    to   w-let-arc-zp2-exg      .
       let-arc-zp2-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zp3]                             *
      *    *-----------------------------------------------------------*
       let-arc-zp3-000.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-zp3-cla    =    w-let-arc-zp3-exc and
                     w-let-arc-zp3-gru    =    w-let-arc-zp3-exg and
                     w-let-arc-zp3-sgr    =    w-let-arc-zp3-exs
                     go to let-arc-zp3-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottogruppo a zero               *
      *              *-------------------------------------------------*
           if        w-let-arc-zp3-cla    =    zero or
                     w-let-arc-zp3-gru    =    zero or
                     w-let-arc-zp3-sgr    =    zero
                     go to let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSGR    "         to   f-key                  .
           move      w-let-arc-zp3-cla    to   rf-zp3-cod-cla         .
           move      w-let-arc-zp3-gru    to   rf-zp3-cod-gru         .
           move      w-let-arc-zp3-sgr    to   rf-zp3-cod-sgr         .
           move      "pgm/dcp/fls/ioc/obj/iofzp3"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zp3                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zp3-400.
       let-arc-zp3-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zp3-des-sgr       to   w-let-arc-zp3-des      .
           move      rf-zp3-mne-sgr       to   w-let-arc-zp3-mne      .
           move      rf-zp3-ult-sud       to   w-let-arc-zp3-sud      .
           move      rf-zp3-sqz-num       to   w-let-arc-zp3-sqz      .
      *              *-------------------------------------------------*
      *              * Ad uscita                                       *
      *              *-------------------------------------------------*
           go to     let-arc-zp3-900.
       let-arc-zp3-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zp3-flg      .
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-let-arc-zp3-sgr    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      spaces               to   w-let-arc-zp3-des      .
           string    "Sottogruppo codice ("
                                delimited by   size
                     p-edt
                                delimited by   spaces
                     ")"
                                delimited by   size
                                          into w-let-arc-zp3-des      .
           move      p-edt                to   w-let-arc-zp3-mne      .
           go to     let-arc-zp3-600.
       let-arc-zp3-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zp3-des      .
       let-arc-zp3-600.
           move      spaces               to   w-let-arc-zp3-mne      .
           move      zero                 to   w-let-arc-zp3-sud      .
           move      zero                 to   w-let-arc-zp3-sqz      .
       let-arc-zp3-900.
      *              *-------------------------------------------------*
      *              * Codice numerico letto su valore precedente      *
      *              *-------------------------------------------------*
           move      w-let-arc-zp3-cla    to   w-let-arc-zp3-exc      .
           move      w-let-arc-zp3-gru    to   w-let-arc-zp3-exg      .
           move      w-let-arc-zp3-sgr    to   w-let-arc-zp3-exs      .
       let-arc-zp3-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per richiamo modulo per la determinazione     *
      *    * della voce 'Fatturato' sulle statistiche di vendita sul   *
      *    * fatturato da Righe Documenti                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mfatfir0.mds"                   .

      *    *===========================================================*
      *    * Subroutines per richiamo modulo per la determinazione del *
      *    * valore della voce 'Note' da esporre sulle statistiche di  *
      *    * vendita sul fatturato                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/mnotsvf0.mds"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

