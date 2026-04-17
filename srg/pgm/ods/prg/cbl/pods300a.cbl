       Identification Division.
       Program-Id.                                 pods300a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    ods                 *
      *                                Settore:    mov                 *
      *                                   Fase:    ods300a             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Evasione ordini clienti                     *
      *                                                                *
      *                    Richiamata in ods300 da funzione 'PF4'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "OP"                       *
      *                 l-eva-orc-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-eva-orc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "DI" - Dichiarazione di inizio ciclo evasione ordine cliente   *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "DI"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "OC" - Accettazione dati identificativi ordine cliente         *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "OC"                       *
      *                 l-eva-orc-tmo-orc = codice tipo movimento or-  *
      *                                     dini clienti da proporre   *
      *                                     come default               *
      *                                                                *
      *        Output : l-eva-orc-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "T1" - Bufferizzazione testata primo ordine cliente richiamato *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "T1"                       *
      *                                                                *
      *        Output : l-eva-orc-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "T+" - Confronto dati testata ordine cliente in esame con dati *
      *        di testata del documento                                *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "T+"                       *
      *                                                                *
      *        Output : l-eva-orc-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "SC" - Saldaconto per evasione ordine                          *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-eva-orc-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "WR" - Aggiornamento record [ocr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Inserimento di un nuovo record [osr]            *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "WR"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "RW" - Aggiornamento record [ocr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Modifica di un record [osr]                     *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "RW"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "DE" - Aggiornamento record [ocr] per quanto riguarda il se-   *
      *        gnale di riga ordine comunque considerata saldata in    *
      *        fase di Cancellazione di un record [osr]                *
      *                                                                *
      *        Input  : l-eva-orc-tip-ope = "DE"                       *
      *                                                                *
      *        Output : nessuno                                        *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione agenti attiva                          *
      *        *-------------------------------------------------------*
           05  w-prs-age-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Gestione valuta per il prezzo di vendita              *
      *        *-------------------------------------------------------*
           05  w-prs-ges-vpp              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Gestione legame valutario nel ciclo vendite           *
      *        *-------------------------------------------------------*
           05  w-prs-ges-lvl              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo esposizione prezzi e sconti                      *
      *        *-------------------------------------------------------*
           05  w-prs-epz-pes              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per blocco cliente in ordini di     *
      *        * spedizione                                            *
      *        *-------------------------------------------------------*
           05  w-prs-blo-cli.
      *            *---------------------------------------------------*
      *            * Tipo di blocco                                    *
      *            *---------------------------------------------------*
               10  w-prs-blo-cli-tip.
      *                *-----------------------------------------------*
      *                * Da anagrafica commerciale cliente             *
      *                *-----------------------------------------------*
                   15  w-prs-blo-cli-t01  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Libero per usi futuri                         *
      *                *-----------------------------------------------*
                   15  w-prs-blo-cli-t02  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Libero per usi futuri                         *
      *                *-----------------------------------------------*
                   15  w-prs-blo-cli-t03  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Libero per usi futuri                         *
      *                *-----------------------------------------------*
                   15  w-prs-blo-cli-t04  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Libero per usi futuri                         *
      *                *-----------------------------------------------*
                   15  w-prs-blo-cli-t05  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo trattamento spese in fattura                     *
      *        * - 00 : Ad accumulo per ogni documento evaso           *
      *        * - 01 : Ricalcolate per l'intero documento             *
      *        *-------------------------------------------------------*
           05  w-prs-ttr-spf              pic  9(02)                  .
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
      *        * Personalizzazioni per la riga di scroll               *
      *        *-------------------------------------------------------*
           05  w-prs-rig-scr.
      *            *---------------------------------------------------*
      *            * Esposizione della descrizione                     *
      *            *                                                   *
      *            *   - 00 : Descrizione completa                     *
      *            *                                                   *
      *            *   - 01 : Primi 25 caratteri della descrizione     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 02 : Primi 21 caratteri della descrizione     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 03 : Codice                                   *
      *            *          Primi 25 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 04 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Primi 21 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 05 : Codice                                   *
      *            *                                                   *
      *            *   - 06 : Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-prs-rig-scr-des      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Area gestionale [orc]                                 *
      *        *-------------------------------------------------------*
           05  w-prs-arg-orc.
      *            *---------------------------------------------------*
      *            * Tipo ordinamento righe ordine in caricamento      *
      *            *                                                   *
      *            * - '01' : Ordinamento per progressivo riga ordine  *
      *            *                                                   *
      *            * - '03' : Ordinamento per codice prodotto          *
      *            *          N.B.: Con questo tipo di ordinamento     *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            *                                                   *
      *            * - '11' : Ordinamento per ubicazione prodotti      *
      *            *          N.B.: Con questo tipo di ordinamento     *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            *                                                   *
      *            * - '91' : Ordinamento per progressivo riga ordine  *
      *            *          o codice alfanumerico prodotto, a ri-    *
      *            *          chiesta dell'utente                      *
      *            *          N.B.: Con il secondo tipo di ordinamento *
      *            *                vengono accodati i commenti e      *
      *            *                gli addebiti a fine righe.         *
      *            *---------------------------------------------------*
               10  w-prs-arg-orc-tor      pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine cliente in corso di tratta-  *
      *        * mento                                                 *
      *        *-------------------------------------------------------*
           05  w-gen-prt-orc              pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per campi di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  w-acc-cod-dpz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo                                     *
      *        *-------------------------------------------------------*
           05  w-acc-num-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Tipo movimento ordini clienti                         *
      *        *-------------------------------------------------------*
           05  w-acc-tmo-orc              pic  x(05)                  .
           05  w-acc-tmo-orc-des          pic  x(30)                  .
           05  w-acc-tmo-orc-vld          pic  9(02)                  .
           05  w-acc-tmo-orc-dpz          pic  9(02)                  .
           05  w-acc-tmo-orc-ord          pic  9(02)                  .
           05  w-acc-tmo-orc-prd          pic  9(02)                  .
           05  w-acc-tmo-orc-sgl          pic  x(03)                  .
           05  w-acc-tmo-orc-tip          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento                                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-doc              pic  9(11)                  .
           05  w-acc-num-doc-r redefines
               w-acc-num-doc.
               10  w-acc-num-doc-saa      pic  9(03)                  .
               10  w-acc-num-doc-dpz      pic  9(02)                  .
               10  w-acc-num-doc-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento righe in caricamento                 *
      *        *-------------------------------------------------------*
           05  w-acc-tip-ord              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-oct.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-dds      pic  9(07)                  .
               10  w-slc-num-oct-nds      pic  9(11)                  .
               10  w-slc-num-oct-nds-r redefines
                   w-slc-num-oct-nds.
                   15  w-slc-num-oct-nsa  pic  9(03)                  .
                   15  w-slc-num-oct-ndp  pic  9(02)                  .
                   15  w-slc-num-oct-npg  pic  9(06)                  .
               10  w-slc-num-oct-dpz      pic  9(02)                  .
               10  w-slc-num-oct-sgl      pic  x(03)                  .
               10  w-slc-num-oct-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-sel      pic  x(01)                  .
               10  w-slc-num-oct-toc      pic  x(05)                  .
               10  w-slc-num-oct-dat      pic  9(07)                  .
               10  w-slc-num-oct-num      pic  9(11)                  .
               10  w-slc-num-oct-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-c01      pic  9(02)                  .
               10  w-slc-num-oct-c02      pic  9(02)                  .
               10  w-slc-num-oct-c03      pic  9(02)                  .
               10  w-slc-num-oct-c04      pic  9(02)                  .
               10  w-slc-num-oct-c05      pic  9(02)                  .
               10  w-slc-num-oct-nli      pic  9(02)                  .
               10  w-slc-num-oct-crb      pic  9(02)                  .
               10  w-slc-num-oct-cpb      pic  9(02)                  .
               10  w-slc-num-oct-cpa      pic  9(02)                  .
               10  w-slc-num-oct-buf
                               occurs 30.
                   15  w-slc-num-oct-bpt  pic  9(11)                  .
               10  w-slc-num-oct-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-oct-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-oct-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-dsp              pic s9(10)v9(03)            .
           05  w-sav-flg-fzs              pic  x(01)                  .
           05  w-sav-snx-aoc              pic  x(01)                  .
           05  w-sav-prz-ven              pic  9(09)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento righe in caricamento      *
      *        *-------------------------------------------------------*
           05  w-exp-tip-ord.
               10  w-exp-tip-ord-num      pic  9(02)       value 2    .
               10  w-exp-tip-ord-lun      pic  9(02)       value 30   .
               10  w-exp-tip-ord-tbl.
                   15  filler             pic  x(30) value
                            "In ordine di inserimento righe"          .
                   15  filler             pic  x(30) value
                            "In ordine di codice prodotto  "          .

      *    *===========================================================*
      *    * Work-area per bufferizzazione protocolli ordini clienti   *
      *    *-----------------------------------------------------------*
       01  w-poc.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-poc-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-poc-max-ele              pic  9(05) value 99         .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-poc-tbl.
               10  w-poc-sng-ele occurs 99.
                   15  w-poc-num-prt      pic  9(11)                  .
               10  w-poc-ctr-001          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione valori originali ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
       01  w-orc.
      *        *-------------------------------------------------------*
      *        * Totale sconto in chiusura                             *
      *        *-------------------------------------------------------*
           05  w-orc-tot-scc              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Totale lordo                                          *
      *        *-------------------------------------------------------*
           05  w-orc-tot-lor              pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione valori relativi alla parte  *
      *    * evasa dell'ordine cliente                                 *
      *    *-----------------------------------------------------------*
       01  w-eoc.
      *        *-------------------------------------------------------*
      *        * Totale sconto in chiusura                             *
      *        *-------------------------------------------------------*
           05  w-eoc-tot-scc              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Totale lordo                                          *
      *        *-------------------------------------------------------*
           05  w-eoc-tot-lor              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per numero                                       *
      *        *-------------------------------------------------------*
           05  w-eoc-num-s18              pic s9(18)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe ordine cliente        *
      *    *-----------------------------------------------------------*
       01  w-bro.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-bro-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-bro-max-ele              pic  9(05) value 800        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-bro-tbl.
               10  w-bro-sng-ele occurs 800.
                   15  w-bro-num-prg      pic  9(05)       comp-3     .
                   15  w-bro-num-pro      pic  9(07)       comp-3     .
                   15  w-bro-tip-rig      pic  x(01)                  .
                   15  w-bro-des-rig      pic  x(40)                  .
                   15  w-bro-dec-qta      pic  9(01)                  .
                   15  w-bro-qta-dev      pic s9(10)v9(03) comp-3     .
                   15  w-bro-qta-dsp      pic s9(10)v9(03) comp-3     .
                   15  w-bro-flg-idr      pic  x(01)                  .
                   15  w-bro-flg-ids      pic  x(01)                  .
                   15  w-bro-flg-fzs      pic  x(01)                  .
                   15  w-bro-qta-res      pic s9(10)v9(03) comp-3     .
                   15  w-bro-snx-aoc      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per funzione saldaconto                         *
      *    *-----------------------------------------------------------*
       01  w-sdc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita da saldaconto                          *
      *        * - spaces : Saldaconto eseguito                        *
      *        * - #      : Uscita per Exit                            *
      *        *-------------------------------------------------------*
           05  w-sdc-flg-exi              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine massimo                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-max              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina attualmente visualizzato                *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-vis              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina da trattare                             *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-dat              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore righe saldaconto                            *
      *        *-------------------------------------------------------*
           05  w-sdc-ctr-rig              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work-area di appoggio                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-c01              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area di comodo per quantita' senza segno             *
      *    *-----------------------------------------------------------*
       01  w-qss.
           05  w-qss-qta-dev              pic  9(10)v9(03)            .
           05  w-qss-qta-dsp              pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work-area per caricamento righe ordine in catena          *
      *    *-----------------------------------------------------------*
       01  w-crc.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-crc-wrk-ctr              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-tes-orc-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-tes-orc.
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-buf-tes-orc-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-orc-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-orc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di superamento max elementi                      *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-sme          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine cliente                      *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wtr.
               10  w-buf-rig-orc-wtp      pic  x(01)                  .
               10  w-buf-rig-orc-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wde          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto righe da bufferizzare                     *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-cst.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-buf-rig-orc-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-buf-rig-orc-key.
      *                    *-------------------------------------------*
      *                    * Chiave                                    *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orc-kal  pic  x(33)              .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '01'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orc-kal-01  redefines
                           w-buf-rig-orc-kal.
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-buf-rig-orc-prg-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(28)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '03'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orc-kal-03  redefines
                           w-buf-rig-orc-kal.
      *                        *---------------------------------------*
      *                        * Codice alfanumerico prodotto          *
      *                        *---------------------------------------*
                           25  w-buf-rig-orc-alf-03
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-buf-rig-orc-prg-03
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(14)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '11'                *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orc-kal-11  redefines
                           w-buf-rig-orc-kal.
      *                        *---------------------------------------*
      *                        * Ubicazione                            *
      *                        *---------------------------------------*
                           25  w-buf-rig-orc-ubi-11
                                          pic  x(28)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-buf-rig-orc-dat.
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-buf-rig-orc-prg
                                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Area di comodo                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wrk.
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio chiave di ordinamento      *
      *            *---------------------------------------------------*
               10  w-buf-rig-orc-svk      pic  x(33)                  .
      *            *---------------------------------------------------*
      *            * Numero massimo di elementi                        *
      *            *---------------------------------------------------*
               10  w-buf-rig-orc-max      pic  9(05)     value 999    .
      *            *---------------------------------------------------*
      *            * Contatori elementi                                *
      *            *---------------------------------------------------*
               10  w-buf-rig-orc-ctr      pic  9(05)                  .
               10  w-buf-rig-orc-cte      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-buf-rig-orc-c01      pic  9(05)                  .
               10  w-buf-rig-orc-c02      pic  9(05)                  .
               10  w-buf-rig-orc-c03      pic  9(05)                  .
               10  w-buf-rig-orc-c04      pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione descrizione riga di scroll    *
      *    *-----------------------------------------------------------*
       01  w-des-scr.
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 00                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-000.
               10  w-des-scr-000-d40      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 01                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-001 redefines
               w-des-scr-000.
               10  w-des-scr-001-d25      pic  x(25)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-001-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 02                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-002 redefines
               w-des-scr-000.
               10  w-des-scr-002-d21      pic  x(21)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-pqs      pic  x(01)                  .
               10  w-des-scr-002-tco      pic  x(01)                  .
               10  w-des-scr-002-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 03                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-003 redefines
               w-des-scr-000.
               10  w-des-scr-003-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-003-d25      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 04                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-004 redefines
               w-des-scr-000.
               10  w-des-scr-004-pqs      pic  x(01)                  .
               10  w-des-scr-004-tco      pic  x(01)                  .
               10  w-des-scr-004-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-d21      pic  x(21)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 05                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-005 redefines
               w-des-scr-000.
               10  filler                 pic  x(26)                  .
               10  w-des-scr-005-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 06                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-006 redefines
               w-des-scr-000.
               10  filler                 pic  x(22)                  .
               10  w-des-scr-006-pqs      pic  x(01)                  .
               10  w-des-scr-006-tco      pic  x(01)                  .
               10  w-des-scr-006-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-006-cod      pic  x(14)                  .

      *    *===========================================================*
      *    * Work per routine dec-tip-rig-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-dec-tip-rig.
           05  w-dec-tip-rig-inx          pic  9(02)                  .
           05  w-dec-tip-rig-pnt          pic  9(02)                  .
           05  w-dec-tip-rig-str.
               10  w-dec-tip-rig-chr      occurs 05
                                          pic  x(01)                  .
           05  w-dec-tip-rig-cod.
               10  w-dec-tip-rig-num      occurs 03
                                          pic  9(01)                  .
           05  w-dec-tip-rig-c01          pic  9(02)                  .
           05  w-dec-tip-rig-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [oct]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-oct.
               10  w-fnd-arc-oct-sel      pic  x(01)                  .
               10  w-fnd-arc-oct-toc      pic  x(05)                  .
               10  w-fnd-arc-oct-dat      pic  9(07)                  .
               10  w-fnd-arc-oct-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zoc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zoc.
               10  w-let-arc-zoc-flg      pic  x(01)                  .
               10  w-let-arc-zoc-cod      pic  x(05)                  .
               10  w-let-arc-zoc-des      pic  x(30)                  .
               10  w-let-arc-zoc-vld      pic  9(02)                  .
               10  w-let-arc-zoc-dpz      pic  9(02)                  .
               10  w-let-arc-zoc-ord      pic  9(02)                  .
               10  w-let-arc-zoc-prd      pic  9(02)                  .
               10  w-let-arc-zoc-sgl      pic  x(03)                  .
               10  w-let-arc-zoc-tip      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-stc      pic  9(07)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [lic]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-lic.
               10  w-let-arc-lic-flg      pic  x(01)                  .
               10  w-let-arc-lic-cod      pic  9(07)                  .
               10  w-let-arc-lic-dri      pic  9(07)                  .
               10  w-let-arc-lic-drf      pic  9(07)                  .
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
               10  w-let-arc-dcc-vlt      pic  x(03)                  .
               10  w-let-arc-dcc-lng      pic  x(03)                  .
               10  w-let-arc-dcc-tai      pic  9(02)                  .
               10  w-let-arc-dcc-blo      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvf.
               10  w-let-arc-zvf-flg      pic  x(01)                  .
               10  w-let-arc-zvf-num      pic  9(03)                  .
               10  w-let-arc-zvf-cod      pic  x(03)                  .
               10  w-let-arc-zvf-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcs.
               10  w-let-arc-zcs-flg      pic  x(01)                  .
               10  w-let-arc-zcs-tip      pic  9(02)                  .
               10  w-let-arc-zcs-cod      pic  9(05)                  .
               10  w-let-arc-zcs-des      pic  x(20)                  .
               10  w-let-arc-zcs-per occurs 5
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-rag      pic  x(40)                  .
               10  w-let-arc-age-spa      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zin]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zin.
               10  w-let-arc-zin-flg      pic  x(01)                  .
               10  w-let-arc-zin-cod      pic  x(03)                  .
               10  w-let-arc-zin-tpg      pic  9(02)                  .
               10  w-let-arc-zin-des      pic  x(40)                  .
               10  w-let-arc-zin-coi      pic  9(05)                  .
               10  w-let-arc-zin-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbo]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbo.
               10  w-let-arc-zbo-flg      pic  x(01)                  .
               10  w-let-arc-zbo-cod      pic  x(03)                  .
               10  w-let-arc-zbo-tpg      pic  9(02)                  .
               10  w-let-arc-zbo-des      pic  x(40)                  .
               10  w-let-arc-zbo-coi      pic  9(05)                  .
               10  w-let-arc-zbo-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ocx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ocx.
               10  w-let-arc-ocx-flg      pic  x(01)                  .
               10  w-let-arc-ocx-prt      pic  9(11)                  .
               10  w-let-arc-ocx-prg      pic  9(05)                  .
               10  w-let-arc-ocx-trc      pic  9(02)                  .
               10  w-let-arc-ocx-des.
                   15  w-let-arc-ocx-drg occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [vet]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-vet.
               10  w-let-arc-vet-flg      pic  x(01)                  .
               10  w-let-arc-vet-cod      pic  9(07)                  .
               10  w-let-arc-vet-rag      pic  x(40)                  .
               10  w-let-arc-vet-via      pic  x(40)                  .
               10  w-let-arc-vet-loc      pic  x(40)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des.
                   15  w-let-arc-dcp-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-dcp-dui      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  9(02)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
               10  w-let-arc-dcp-ctr      pic  9(02)                  .
               10  w-let-arc-dcp-lng      pic  x(03)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zls]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det documento gia' esistente oppure no       *
      *        *-------------------------------------------------------*
           05  w-det-doc-ges.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato esistente in    *
      *            *       archivio                                    *
      *            * - N : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio                                 *
      *            * - X : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio, ma esistenza di un documento   *
      *            *       con data e numero cercato, con stesso co-   *
      *            *       dice numerazione, ma con diverso tipo mo-   *
      *            *       vimento da quello cercato                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento trovato per valore precedente de-  *
      *            * terminato a 'X'                                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-tmt      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc]       *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcc.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-ctr      pic  9(02)                  .
               10  w-det-vlt-dcc-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det funzionamento spese in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-fun-spe.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-ctr      pic  9(02)                  .
               10  w-det-fun-spe-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc] della *
      *        * dipendenza                                            *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcd.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione ubicazione       *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work per buffer documenti movimentati                     *
      *    *-----------------------------------------------------------*
       01  w-buf-doc.
      *        *-------------------------------------------------------*
      *        * Work per buffer                                       *
      *        *-------------------------------------------------------*
           05  w-buf-doc-mvm.
      *            *---------------------------------------------------*
      *            * Data di richiesta                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-drc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-ute      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di selezione                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-fds      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-tds      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero documento selezionato                      *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-nds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Data documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-c01      pic  9(02)                  .
               10  w-buf-doc-mvm-c02      pic  9(02)                  .
               10  w-buf-doc-mvm-c03      pic  9(02)                  .
               10  w-buf-doc-mvm-c04      pic  9(02)                  .
               10  w-buf-doc-mvm-c05      pic  9(02)                  .
               10  w-buf-doc-mvm-nli      pic  9(02)                  .
               10  w-buf-doc-mvm-crb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpa      pic  9(02)                  .
               10  w-buf-doc-mvm-max      pic  9(02) value 54         .
               10  w-buf-doc-mvm-buf occurs 54.
                   15  w-buf-doc-mvm-tmo  pic  x(05)                  .
                   15  w-buf-doc-mvm-num  pic  9(11)                  .
                   15  w-buf-doc-mvm-dat  pic  9(07)                  .
                   15  w-buf-doc-mvm-rsa  pic  x(40)                  .
               10  w-buf-doc-mvm-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-buf-doc-mvm-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-buf-doc-mvm-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per routine exe-rca-pcm-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-exe-rca-pcm.
      *        *-------------------------------------------------------*
      *        * Numero riga per la quale e' stato trovato il codice   *
      *        * di magazzino                                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-nrg          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice numerico di magazzino                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-cnm          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico di magazzino                      *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-cam          pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione di magazzino                              *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-dem          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-ctr          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Indice di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-inx          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Buffer numeri riga con codice di magazzino pari a     *
      *        * quello incontrato                                     *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-buf.
               10  w-exe-rca-pcm-ele      pic  9(03)                  .
               10  w-exe-rca-pcm-max      pic  9(03) value 10         .
               10  w-exe-rca-pcm-bnr occurs 10
                                          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio linee                          *
      *        *-------------------------------------------------------*
           05  w-exe-rca-pcm-sl1          pic  x(80)                  .
           05  w-exe-rca-pcm-sl2          pic  x(80)                  .
           05  w-exe-rca-pcm-sl3          pic  x(80)                  .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento ordini clienti  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per evasione ordini clienti         *
      *    *-----------------------------------------------------------*
       01  l-eva-orc.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-eva-orc-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-eva-orc-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-eva-orc-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento ordini clienti      *
      *        *-------------------------------------------------------*
           05  l-eva-orc-tmo-orc          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento ordine cliente                    *
      *        *-------------------------------------------------------*
           05  l-eva-orc-flg-orc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di function-key Tab in corso                     *
      *        *-------------------------------------------------------*
           05  l-eva-orc-fky-tab          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pods3000       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-tes'     : Work-area per bufferizzazione testata    *
      *    *  - 'w-pie'     : Work-area per bufferizzazione piede      *
      *    *  - 'w-rig'     : Work-area per bufferizzazione riga       *
      *    *  - 'w-cat-rig' : Work-area di comunicazione per gestione  *
      *    *                  catena righe                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/pods3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-eva-orc
                                               w-tes
                                               w-pie
                                               w-rig
                                               w-cat-rig              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   l-eva-orc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dichiarazione di inizio ciclo di evasione   *
      *                  *---------------------------------------------*
           if        l-eva-orc-tip-ope    =    "DI"
                     perform dic-ini-cic-000
                                          thru dic-ini-cic-999
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "OC"
                     perform acc-dti-orc-000
                                          thru acc-dti-orc-999
      *                  *---------------------------------------------*
      *                  * Bufferizzazione testata primo ordine clien- *
      *                  * te richiamato                               *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "T1"
                     perform buf-tes-orc-000
                                          thru buf-tes-orc-999
      *                  *---------------------------------------------*
      *                  * Confronto testata ordine cliente richiama-  *
      *                  * to con i dati di testata del documento      *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "T+"
                     perform cnf-tes-doc-000
                                          thru cnf-tes-doc-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per evasione righe ordine        *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "SC"
                     perform sdc-eva-orc-000
                                          thru sdc-eva-orc-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ocr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Inserimento  *
      *                  * di un nuovo record [osr]                    *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "WR"
                     perform wrt-rec-ocr-000
                                          thru wrt-rec-ocr-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ocr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Modifica     *
      *                  * di un record [osr]                          *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "RW"
                     perform rew-rec-ocr-000
                                          thru rew-rec-ocr-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento record [ocr] per quanto ri-   *
      *                  * guarda il segnale di riga ordine comunque   *
      *                  * considerata saldata in fase di Cancellazio- *
      *                  * ne di un record [osr]                       *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "DE"
                     perform del-rec-ocr-000
                                          thru del-rec-ocr-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-eva-orc-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       exe-fun-opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-gen-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice dipendenza in uso        *
      *              *-------------------------------------------------*
           move      l-eva-orc-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione agenti attiva                *
      *                  *---------------------------------------------*
           perform   prs-age-snx-000      thru prs-age-snx-999        .
      *                  *---------------------------------------------*
      *                  * Gestione valuta per il prezzo di vendita    *
      *                  *---------------------------------------------*
           perform   prs-ges-vpp-000      thru prs-ges-vpp-999        .
      *                  *---------------------------------------------*
      *                  * Gestione legame valutario nel ciclo vendite *
      *                  *---------------------------------------------*
           perform   prs-ges-lvl-000      thru prs-ges-lvl-999        .
      *                  *---------------------------------------------*
      *                  * Tipo esposizione prezzi e sconti            *
      *                  *---------------------------------------------*
           perform   prs-epz-pes-000      thru prs-epz-pes-999        .
      *                  *---------------------------------------------*
      *                  * Modalita' di blocco cliente in fase di spe- *
      *                  * dizione                                     *
      *                  *---------------------------------------------*
           perform   prs-blo-cli-000      thru prs-blo-cli-999        .
      *                  *---------------------------------------------*
      *                  * Tipo trattamento spese in fattura           *
      *                  *---------------------------------------------*
           perform   prs-ttr-spf-000      thru prs-ttr-spf-999        .
      *                  *---------------------------------------------*
      *                  * Spese per la fatturazione                   *
      *                  *---------------------------------------------*
           perform   prs-spe-fat-000      thru prs-spe-fat-999        .
      *                  *---------------------------------------------*
      *                  * Esposizione della riga in scroll            *
      *                  *---------------------------------------------*
           perform   prs-rig-scr-000      thru prs-rig-scr-999        .
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento righe ordine in caricamen- *
      *                  * to                                          *
      *                  *---------------------------------------------*
           perform   prs-orc-tor-000      thru prs-orc-tor-999        .
      *              *-------------------------------------------------*
      *              * Apertura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qev-roc-opn-000  thru det-qev-roc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione ubicazioni        *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-opn-000  thru det-prm-ubi-opn-999    .
       exe-fun-opn-999.
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
      *    * Lettura personalizzazione : Gestione valuta per il prezzo *
      *    *                             di vendita                    *
      *    *-----------------------------------------------------------*
       prs-ges-vpp-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[ges-vpp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-ges-vpp
           else      move  zero           to   w-prs-ges-vpp          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ges-vpp        =    00 or
                     w-prs-ges-vpp        =    11 or
                     w-prs-ges-vpp        =    21
                     go to prs-ges-vpp-999.
           move      00                   to   w-prs-ges-vpp          .
       prs-ges-vpp-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Gestione legame valutario nel *
      *    *                             ciclo vendite                 *
      *    *-----------------------------------------------------------*
       prs-ges-lvl-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[ges-lvl]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-ges-lvl
           else      move  zero           to   w-prs-ges-lvl          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ges-lvl        =    00 or
                     w-prs-ges-lvl        =    11 or
                     w-prs-ges-lvl        =    21
                     go to prs-ges-lvl-999.
           move      00                   to   w-prs-ges-lvl          .
       prs-ges-lvl-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo esposizione prezzi e     *
      *    *                             sconti                        *
      *    *-----------------------------------------------------------*
       prs-epz-pes-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[epz-pes]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-epz-pes
           else      move  zero           to   w-prs-epz-pes          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-epz-pes        =    11 or
                     w-prs-epz-pes        =    21
                     go to prs-epz-pes-999.
           move      11                   to   w-prs-epz-pes          .
       prs-epz-pes-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di bloc- *
      *    * co cliente in fase di spedizione                          *
      *    *-----------------------------------------------------------*
       prs-blo-cli-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/mov/ods300[tip-blo]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente si spostano i    *
      *              * valori letti in area di destinazione, altri-    *
      *              * menti si normalizza l'area di destinazione      *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-blo-cli-tip
           else      move  "S"            to   w-prs-blo-cli-t01
                     move  spaces         to   w-prs-blo-cli-t02
                     move  spaces         to   w-prs-blo-cli-t03
                     move  spaces         to   w-prs-blo-cli-t04
                     move  spaces         to   w-prs-blo-cli-t05      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-blo-cli-t01    not  = "S" and
                     w-prs-blo-cli-t01    not  = "B"
                     move  "S"            to   w-prs-blo-cli-t01      .
       prs-blo-cli-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo trattamento spese in     *
      *    *                             fattura                       *
      *    *-----------------------------------------------------------*
       prs-ttr-spf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[ttr-spf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-num          to   w-prs-ttr-spf
           else      move  zero           to   w-prs-ttr-spf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ttr-spf        =    00 or
                     w-prs-ttr-spf        =    01
                     go to prs-ttr-spf-999.
           move      00                   to   w-prs-ttr-spf          .
       prs-ttr-spf-999.
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
      *                  *---------------------------------------------*
      *                  * Open tabella [zsf]                          *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
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
      *              * Close tabella [zsf]                             *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-spe-fat-999.
       prs-spe-fat-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di espo- *
      *    * sizione della riga di scroll                              *
      *    *-----------------------------------------------------------*
       prs-rig-scr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/mov/ods300[rig-scr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente si spostano i    *
      *              * valori letti in area di destinazione, altri-    *
      *              * menti si normalizza l'area di destinazione      *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rig-scr
           else      move  zero           to   w-prs-rig-scr-des      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione descrizione    *
      *              *-------------------------------------------------*
           if        w-prs-rig-scr-des    not  = 00 and
                     w-prs-rig-scr-des    not  = 01 and
                     w-prs-rig-scr-des    not  = 02 and
                     w-prs-rig-scr-des    not  = 03 and
                     w-prs-rig-scr-des    not  = 04 and
                     w-prs-rig-scr-des    not  = 05 and
                     w-prs-rig-scr-des    not  = 06
                     move  00             to   w-prs-rig-scr-des      .
       prs-rig-scr-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Tipo ordinamento righe ordine *
      *    * in caricamento                                            *
      *    *-----------------------------------------------------------*
       prs-orc-tor-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/mov/ods300a[tor-cev]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-arg-orc-tor
           else      move  spaces         to   w-prs-arg-orc-tor      .
       prs-orc-tor-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-arg-orc-tor    not  = "01" and
                     w-prs-arg-orc-tor    not  = "03" and
                     w-prs-arg-orc-tor    not  = "11" and
                     w-prs-arg-orc-tor    not  = "91"
                     move  "01"           to   w-prs-arg-orc-tor      .
       prs-orc-tor-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-gen-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Chiusura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [oct]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * [ocx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * [zoc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qev-roc-cls-000  thru det-qev-roc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione ubicazioni       *
      *              *-------------------------------------------------*
           perform   det-prm-ubi-cls-000  thru det-prm-ubi-cls-999    .
       exe-fun-cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-gen-ctr-opn        =    zero
                     move  spaces         to   l-eva-orc-exi-sts
           else      move  "#"            to   l-eva-orc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio ciclo di evasione                 *
      *    *-----------------------------------------------------------*
       dic-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatore numero elementi in ta-    *
      *              * bella protocolli ordini clienti                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-poc-num-ele          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di Function-Key Tab in     *
      *              * corso                                           *
      *              *-------------------------------------------------*
           move      spaces               to   l-eva-orc-fky-tab      .
       dic-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi ordine cliente           *
      *    *-----------------------------------------------------------*
       acc-dti-orc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area di accettazione       *
      *              *-------------------------------------------------*
           perform   nor-wrk-acc-000      thru nor-wrk-acc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo centrale                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      23                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "RICHIAMO ORDINE CLIENTE"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordini clienti                            *
      *              *-------------------------------------------------*
           move      l-eva-orc-tmo-orc    to   w-acc-tmo-orc          .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Ad accettazione data documento                  *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-200.
       acc-dti-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi ordine         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo movimento ordini clienti               *
      *                  *---------------------------------------------*
           perform   acc-tmo-orc-000      thru acc-tmo-orc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
       acc-dti-orc-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-100.
       acc-dti-orc-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-200.
       acc-dti-orc-350.
      *                  *---------------------------------------------*
      *                  * Tipo ordinamento righe in caricamento       *
      *                  *---------------------------------------------*
           perform   acc-tip-ord-000      thru acc-tip-ord-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-300.
       acc-dti-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione dell'ordine                         *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orc        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to acc-dti-orc-100.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Secolo/Anno                                 *
      *                  *---------------------------------------------*
           move      w-acc-dat-doc        to   s-dat                  .
           move      s-saa                to   w-acc-num-doc-saa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      w-acc-cod-dpz        to   w-acc-num-doc-dpz      .
      *              *-------------------------------------------------*
      *              * Eventuale normalizzazione tipo ordinamento      *
      *              *-------------------------------------------------*
           if        w-prs-arg-orc-tor    =    "91" and
                     w-acc-tip-ord        =    zero
                     move  01             to   w-acc-tip-ord          .
      *              *-------------------------------------------------*
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                  *---------------------------------------------*
      *                  * Se documento non esistente                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "N"
                     go to acc-dti-orc-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-420.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con tipo movimen- *
      *                  * to diverso                                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "X"
                     go to acc-dti-orc-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Movimento esistente, ma di tipo '"
                                delimited by   size
                     w-det-doc-ges-tmt
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-440.
      *              *-------------------------------------------------*
      *              * Test su flag di ordine chiuso                   *
      *              *-------------------------------------------------*
           if        rf-oct-flg-och       =    spaces
                     go to acc-dti-orc-460.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Ordine cliente chiuso !                           
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-460.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      rf-oct-num-prt       to   w-acc-num-prt          .
       acc-dti-orc-500.
      *              *-------------------------------------------------*
      *              * Test se l'ordine e' gia' stato richiamato       *
      *              *-------------------------------------------------*
           move      zero                 to   w-poc-ctr-001          .
       acc-dti-orc-520.
           add       1                    to   w-poc-ctr-001          .
           if        w-poc-ctr-001        >    w-poc-num-ele
                     go to acc-dti-orc-540.
           if        w-acc-num-prt        not  = w-poc-num-prt
                                                (w-poc-ctr-001)
                     go to acc-dti-orc-520.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Ordine cliente gia' richiamato !                  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-540.
      *              *-------------------------------------------------*
      *              * Incremento contatore protocolli ordini clienti  *
      *              *-------------------------------------------------*
           add       1                    to   w-poc-num-ele          .
           if        w-poc-num-ele        not  >  w-poc-max-ele
                     go to acc-dti-orc-600.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo                         *
      *                  *---------------------------------------------*
           move      "Numero ordini clienti trattati oltre il massimo   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-orc-800.
       acc-dti-orc-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe ordine da trattare        *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-orc-prt      .
           perform   buf-rig-orc-000      thru buf-rig-orc-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-orc-flg    =    spaces
                     go to acc-dti-orc-620.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine cliente gia' evaso !                       
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-620.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo in tabella    *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-poc-num-prt
                                              (w-poc-num-ele)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo ordine cliente in cor- *
      *              * so di trattamento                               *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-orc          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-900.
       acc-dti-orc-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-eva-orc-exi-sts      .
       acc-dti-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-999.
       acc-dti-orc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo movimento per ordini clienti    *
      *    *-----------------------------------------------------------*
       acc-tmo-orc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tmo-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zoc-ope      .
           move      w-acc-tmo-orc        to   w-cod-des-zoc-cod      .
           move      14                   to   w-cod-des-zoc-lin      .
           move      09                   to   w-cod-des-zoc-pos      .
           move      14                   to   w-cod-des-zoc-dln      .
           move      15                   to   w-cod-des-zoc-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
       acc-tmo-orc-110.
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           if        w-cod-des-zoc-ope    =    "F+"
                     go to acc-tmo-orc-115.
           if        w-cod-des-zoc-ope    =    "AC"
                     go to acc-tmo-orc-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tmo-orc-115.
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
           go to     acc-tmo-orc-110.
       acc-tmo-orc-120.
           move      w-cod-des-zoc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tmo-orc-999.
       acc-tmo-orc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-tmo-orc          .
       acc-tmo-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-acc-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-acc-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-acc-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-acc-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
           move      w-let-arc-zoc-tip    to   w-acc-tmo-orc-tip      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc-vld    not  = 02
                     go to acc-tmo-orc-500.
           if        w-acc-cod-dpz        =    w-acc-tmo-orc-dpz
                     go to acc-tmo-orc-500.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-500.
      *                  *---------------------------------------------*
      *                  * Test su tipo ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    =    spaces
                     go to acc-tmo-orc-600.
      *                      *-----------------------------------------*
      *                      * Se Pro-forma                            *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "P"
                     go to acc-tmo-orc-520.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine pro-forma non gestibile !                  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-520.
      *                      *-----------------------------------------*
      *                      * Se Forecast                             *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "F"
                     go to acc-tmo-orc-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine 'forecast' non gestibile !                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tmo-orc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : tipo movimento per ordini clienti *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-tmo-orc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tipo movimento ordini *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-acc-tmo-orc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data documento                       *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-100.
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-acc-dat-doc          .
       acc-dat-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-dat-doc-999.
       acc-dat-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-doc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           if        w-prs-arg-orc-tor    not  = "91"
                     move  "DO  "         to   v-key                  .
       acc-dat-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-600.
           if        v-key                =    "UP  "
                     go to acc-dat-doc-600
           else      go to acc-dat-doc-100.
       acc-dat-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data documento                    *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero documento                     *
      *    *-----------------------------------------------------------*
       acc-num-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-doc-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "Tasto di pagina precedente per visualizzare gli or
      -              "dini emessi alla data indicata"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-num-doc-999.
       acc-num-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-num-doc-prg      .
       acc-num-doc-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           if        w-prs-arg-orc-tor    not  = "91"
                     move  "DO  "         to   v-key                  .
       acc-num-doc-350.
      *              *-------------------------------------------------*
      *              * Se Select                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-num-doc-380.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-acc-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-acc-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-acc-tmo-orc        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-acc-num-doc-prg    to   w-slc-num-oct-npg      .
           move      w-acc-cod-dpz        to   w-slc-num-oct-dpz      .
           move      w-acc-tmo-orc-sgl    to   w-slc-num-oct-sgl      .
           move      w-acc-dat-doc        to   w-slc-num-oct-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   slc-num-oct-000      thru slc-num-oct-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           if        w-prs-arg-orc-tor    not  = "91"
                     move  "DO  "         to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
       acc-num-doc-380.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-buf-doc-mvm-drc      .
           move      s-ute                to   w-buf-doc-mvm-ute      .
           move      w-tes-cod-dpz        to   w-buf-doc-mvm-dpz      .
      *
           if        w-acc-dat-doc        =    zero
                     move  w-tes-dat-doc  to   w-buf-doc-mvm-drc
           else if   w-tes-dat-doc        =    zero
                     move  s-dat          to   w-buf-doc-mvm-drc
           else      move  w-acc-dat-doc  to   w-buf-doc-mvm-drc      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   buf-doc-mvm-000      thru buf-doc-mvm-999        .
      *                      *-----------------------------------------*
      *                      * Test sul flag di uscita                 *
      *                      *-----------------------------------------*
           if        w-buf-doc-mvm-fds    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Valori selezionati                          *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tds    to   w-acc-tmo-orc          .
           move      w-buf-doc-mvm-nds    to   w-acc-num-doc          .
           move      w-buf-doc-mvm-dds    to   w-acc-dat-doc          .
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi         *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
       acc-num-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero documento                  *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo ordinamento righe in caricamen- *
      *    *                      to                                   *
      *    *-----------------------------------------------------------*
       acc-tip-ord-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-prs-arg-orc-tor    not  = "91"
                     go to acc-tip-ord-999.
       acc-tip-ord-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-ord-lun    to   v-car                  .
           move      w-exp-tip-ord-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      16                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-exp-tip-ord-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-tip-ord        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tip-ord-999.
       acc-tip-ord-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-tip-ord          .
       acc-tip-ord-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tip-ord-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ord-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-ord-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
           move      zero                 to   w-acc-num-prt          .
           move      spaces               to   w-acc-tmo-orc          .
           move      spaces               to   w-acc-tmo-orc-des      .
           move      zero                 to   w-acc-tmo-orc-vld      .
           move      zero                 to   w-acc-tmo-orc-dpz      .
           move      zero                 to   w-acc-tmo-orc-ord      .
           move      zero                 to   w-acc-tmo-orc-prd      .
           move      spaces               to   w-acc-tmo-orc-sgl      .
           move      spaces               to   w-acc-tmo-orc-tip      .
           move      zero                 to   w-acc-num-doc          .
           move      zero                 to   w-acc-tip-ord          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *-----------------------------------------------------------*
       buf-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-orc-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di superamento massimo     *
      *              * elementi                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orc-sme      .
      *              *-------------------------------------------------*
      *              * Preparazione castelletto righe in memoria       *
      *              *-------------------------------------------------*
           perform   buf-rig-orc-pcr-000  thru buf-rig-orc-pcr-999    .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-bro-num-ele          .
      *              *-------------------------------------------------*
      *              * Azzeramento contatore righe nel buffer          *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orc-cte      .
      *              *-------------------------------------------------*
      *              * Test su numero elementi letti                   *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-ctr    not   > zero
                     go to buf-rig-orc-500.
       buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe nel buffer           *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-rig-orc-cte      .
      *              *-------------------------------------------------*
      *              * Test su contatore righe                         *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-cte    >    w-buf-rig-orc-ctr
                     go to buf-rig-orc-500.
           if        w-buf-rig-orc-cte    >    w-buf-rig-orc-max
                     go to buf-rig-orc-500.
           if        w-buf-rig-orc-prg
                    (w-buf-rig-orc-cte)   =    zero
                     go to buf-rig-orc-500.
       buf-rig-orc-220.
      *              *-------------------------------------------------*
      *              * Normalizzazione riga                            *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       buf-rig-orc-240.
      *              *-------------------------------------------------*
      *              * Lettura riga in corso di trattamento            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orc-prt    to   rf-ocr-num-prt         .
           move      w-buf-rig-orc-prg
                    (w-buf-rig-orc-cte)   to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-200.
       buf-rig-orc-260.
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to   w-buf-rig-orc-wtr      .
      *              *-------------------------------------------------*
      *              * Se tipo riga "C" : a bufferizzazione riga       *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-wtp    =    "C"
                     go to buf-rig-orc-300.
      *              *-------------------------------------------------*
      *              * Test su flag di riga chiusa                     *
      *              *-------------------------------------------------*
           if        rf-ocr-flg-rch       not  = spaces
                     go to buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Test su segnale di riga comunque considerata    *
      *              * saldata                                         *
      *              *-------------------------------------------------*
           if        rf-ocr-sdr-ccs       not  = spaces
                     go to buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Se tipo riga "A" : a normalizzazione flag di u- *
      *              * scita e bufferizzazione riga ordine             *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-wtp    =    "A"
                     go to buf-rig-orc-280.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine cli- *
      *              * ente                                            *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *              *-------------------------------------------------*
      *              * Se riga evasa : riciclo                         *
      *              *-------------------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to buf-rig-orc-200.
       buf-rig-orc-280.
      *              *-------------------------------------------------*
      *              * Altrimenti normalizzazione flag di uscita       *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orc-flg      .
       buf-rig-orc-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga ordine                     *
      *              *-------------------------------------------------*
           if        w-bro-num-ele        <    w-bro-max-ele
                     go to buf-rig-orc-305.
      *                  *---------------------------------------------*
      *                  * Set del flag di superamento numero massimo  *
      *                  * elementi                                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-rig-orc-sme      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orc-500.
       buf-rig-orc-305.
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           add       1                    to   w-bro-num-ele          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      rf-ocr-num-prg       to   w-bro-num-prg
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Codice numerico di magazzino                *
      *                  *---------------------------------------------*
           move      rf-ocr-num-pro       to   w-bro-num-pro
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-bro-tip-rig
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura preliminare file [ocx]          *
      *                      *-----------------------------------------*
           move      rf-ocr-num-prt       to   w-let-arc-ocx-prt      .
           move      rf-ocr-num-prg       to   w-let-arc-ocx-prg      .
           move      11                   to   w-let-arc-ocx-trc      .
           perform   let-arc-ocx-000      thru let-arc-ocx-999        .
      *
           if        w-let-arc-ocx-flg    not  = spaces
                     go to buf-rig-orc-308.
      *
           move      w-let-arc-ocx-des    to   w-buf-rig-orc-wde      .
      *
           go to     buf-rig-orc-350.
       buf-rig-orc-308.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di e-   *
      *                      * stensione alla descrizione              *
      *                      *-----------------------------------------*
           if        rf-ocr-des-ext       =    0
                     go to buf-rig-orc-310
           else if   rf-ocr-des-ext       =    1
                     go to buf-rig-orc-320
           else if   rf-ocr-des-ext       =    2
                     go to buf-rig-orc-330.
       buf-rig-orc-310.
      *                      *-----------------------------------------*
      *                      * Se nessuna estensione : bufferizzazione *
      *                      * descrizione contenuta nel record [ocr]  *
      *                      *-----------------------------------------*
           move      rf-ocr-des-rig       to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-320.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [ocx]            *
      *                      *-----------------------------------------*
           go to     buf-rig-orc-350.
       buf-rig-orc-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx]            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-ocr-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-bro-des-rig
                                              (w-bro-num-ele)         .
       buf-rig-orc-356.
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-orc-wtp    =    "C" or
                     w-buf-rig-orc-wtp    =    "A"
                     move  zero           to   w-bro-qta-dev
                                              (w-bro-num-ele)
                     go to buf-rig-orc-360.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      d-qev-roc-qta-dev    to   w-bro-qta-dev
                                              (w-bro-num-ele)         .
       buf-rig-orc-360.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-ocr-dec-qta       to   w-bro-dec-qta
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in ordine di spedizione           *
      *                  *---------------------------------------------*
           move      zero                 to   w-bro-qta-dsp
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di residuo                  *
      *                  *---------------------------------------------*
           move      "<"                  to   w-bro-flg-idr
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di saldo                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-flg-ids
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag di forzatura a saldo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' residua pari alla quantita' da e- *
      *                  * vadere                                      *
      *                  *---------------------------------------------*
           move      w-bro-qta-dev
                    (w-bro-num-ele)       to   w-bro-qta-res
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-bro-snx-aoc
                                              (w-bro-num-ele)         .
       buf-rig-orc-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ocr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-200.
       buf-rig-orc-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-999.
       buf-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Subroutine di preparazione castelletto righe in memoria   *
      *    *-----------------------------------------------------------*
       buf-rig-orc-pcr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orc-ctr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
       buf-rig-orc-pcr-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orc-prt    to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della start                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-pcr-900.
       buf-rig-orc-pcr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [ocr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-pcr-900.
       buf-rig-orc-pcr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ocr-num-prt       not  = w-buf-rig-orc-prt
                     go to buf-rig-orc-pcr-900.
       buf-rig-orc-pcr-400.
       buf-rig-orc-pcr-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-rig-orc-ctr      .
      *              *-------------------------------------------------*
      *              * Test sul contatore                              *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-ctr    >    w-buf-rig-orc-max
                     go to buf-rig-orc-pcr-900.
       buf-rig-orc-pcr-520.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-ocr-qta-ord       to   w-edt-qta-inc-qta      .
           move      rf-ocr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati per il buffer        *
      *              *-------------------------------------------------*
           perform   buf-rig-orc-obk-000  thru buf-rig-orc-obk-999    .
       buf-rig-orc-pcr-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-pcr-200.
       buf-rig-orc-pcr-900.
      *              *-------------------------------------------------*
      *              * Ordinamento finale delle righe bufferizzate     *
      *              *-------------------------------------------------*
           perform   buf-rig-orc-obr-000  thru buf-rig-orc-obr-999    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-pcr-999.
       buf-rig-orc-pcr-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Preparazione chiave per bufferizzazione righe             *
      *    *-----------------------------------------------------------*
       buf-rig-orc-obk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare chiave              *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orc-kal
                                              (w-buf-rig-orc-ctr)     .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare dati                *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orc-prg
                                              (w-buf-rig-orc-ctr)     .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to   w-buf-rig-orc-wtr      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle personalizzazioni  *
      *              *-------------------------------------------------*
           if        w-prs-arg-orc-tor    =    "01"
                     go to buf-rig-orc-obk-010
           else if   w-prs-arg-orc-tor    =    "03"
                     go to buf-rig-orc-obk-030
           else if   w-prs-arg-orc-tor    =    "11"
                     go to buf-rig-orc-obk-110
           else if   w-prs-arg-orc-tor    =    "91" and
                     w-acc-tip-ord        =    01
                     go to buf-rig-orc-obk-010
           else if   w-prs-arg-orc-tor    =    "91" and
                     w-acc-tip-ord        =    02
                     go to buf-rig-orc-obk-030
           else      go to buf-rig-orc-obk-010.
       buf-rig-orc-obk-010.
      *              *-------------------------------------------------*
      *              * Se ordinamento per progressivo riga             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ocr-num-prg       to   w-buf-rig-orc-prg-01
                                              (w-buf-rig-orc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orc-obk-900.
       buf-rig-orc-obk-030.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice alfanumerico prodotto *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo riga non "P"                    *
      *                      *-----------------------------------------*
           if        w-buf-rig-orc-wtp    =    "P"
                     go to buf-rig-orc-obk-032.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      all "z"              to   w-buf-rig-orc-alf-03
                                              (w-buf-rig-orc-ctr)     .
      *                      *-----------------------------------------*
      *                      * A bufferizzazione progressivo riga      *
      *                      *-----------------------------------------*
           go to     buf-rig-orc-obk-034.
       buf-rig-orc-obk-032.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-buf-rig-orc-alf-03
                                              (w-buf-rig-orc-ctr)     .
       buf-rig-orc-obk-034.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-ocr-num-prg       to   w-buf-rig-orc-prg-03
                                              (w-buf-rig-orc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orc-obk-900.
       buf-rig-orc-obk-110.
      *              *-------------------------------------------------*
      *              * Se ordinamento per ubicazione                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo riga non "P"                    *
      *                      *-----------------------------------------*
           if        w-buf-rig-orc-wtp    =    "P"
                     go to buf-rig-orc-obk-120.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione ubicazione              *
      *                      *-----------------------------------------*
           move      all "z"              to   w-buf-rig-orc-ubi-11
                                              (w-buf-rig-orc-ctr)     .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     buf-rig-orc-obk-140.
       buf-rig-orc-obk-120.
      *                      *-----------------------------------------*
      *                      * Determinazione ubicazione               *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-prm-ubi-tip-ope      .
           move      rf-ocr-cod-dpz       to   d-prm-ubi-cod-dpz      .
           move      01                   to   d-prm-ubi-tip-mag      .
           move      rf-ocr-num-pro       to   d-prm-ubi-num-mag      .
           move      rf-ocr-sgl-vrn       to   d-prm-ubi-var-mag      .
           perform   det-prm-ubi-cll-000  thru det-prm-ubi-cll-999    .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione ubicazione              *
      *                      *-----------------------------------------*
           move      d-prm-ubi-ubi-lit    to   w-buf-rig-orc-ubi-11
                                              (w-buf-rig-orc-ctr)     .
       buf-rig-orc-obk-140.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-rig-orc-obk-900.
       buf-rig-orc-obk-900.
      *              *-------------------------------------------------*
      *              * Preparazione dati di ordinamento                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-ocr-num-prg       to   w-buf-rig-orc-prg
                                              (w-buf-rig-orc-ctr)     .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-obk-999.
       buf-rig-orc-obk-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *                                                           *
      *    * Ordinamento righe bufferizzate                            *
      *    *-----------------------------------------------------------*
       buf-rig-orc-obr-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-ctr    <    2
                     go to buf-rig-orc-obr-999.
       buf-rig-orc-obr-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-orc-c01      .
       buf-rig-orc-obr-100.
           add       1                    to   w-buf-rig-orc-c01      .
           if        w-buf-rig-orc-c01    =    w-buf-rig-orc-ctr
                     go to buf-rig-orc-obr-999.
           move      w-buf-rig-orc-c01    to   w-buf-rig-orc-c02
                                               w-buf-rig-orc-c03      .
           move      w-buf-rig-orc-key
                    (w-buf-rig-orc-c01)   to   w-buf-rig-orc-svk      .
       buf-rig-orc-obr-200.
           add       1                    to   w-buf-rig-orc-c02      .
           if        w-buf-rig-orc-c02    >    w-buf-rig-orc-ctr
                     go to buf-rig-orc-obr-300.
           if        w-buf-rig-orc-key
                    (w-buf-rig-orc-c02)   >    w-buf-rig-orc-svk
                     go to buf-rig-orc-obr-200.
           move      w-buf-rig-orc-c02    to   w-buf-rig-orc-c03      .
           move      w-buf-rig-orc-key
                    (w-buf-rig-orc-c02)   to   w-buf-rig-orc-svk      .
           go to     buf-rig-orc-obr-200.
       buf-rig-orc-obr-300.
           move      w-buf-rig-orc-c01    to   w-buf-rig-orc-c04      .          
           if        w-buf-rig-orc-svk    >    w-buf-rig-orc-key
                                              (w-buf-rig-orc-c04)
                     go to buf-rig-orc-obr-100.
           move      w-buf-rig-orc-ele
                    (w-buf-rig-orc-c03)   to   w-buf-rig-orc-ele (999).
           move      w-buf-rig-orc-ele
                    (w-buf-rig-orc-c04)   to   w-buf-rig-orc-ele
                                              (w-buf-rig-orc-c03)     .
           move      w-buf-rig-orc-ele
                    (999)                 to   w-buf-rig-orc-ele
                                              (w-buf-rig-orc-c04)     .
           go to     buf-rig-orc-obr-100.
       buf-rig-orc-obr-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-orc-wtp    =    "P" or
                     w-buf-rig-orc-wtp    =    "S" or
                     w-buf-rig-orc-wtp    =    "M"  ) and
                     w-buf-rig-orc-wtf    =    spaces
                     go to cpz-des-rgs-200.
       cpz-des-rgs-100.
      *              *-------------------------------------------------*
      *              * Se non si sta trattando un codice di magazzino  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, in quanto la descrizione    *
      *                  * gia' composta va' bene                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-200.
      *              *-------------------------------------------------*
      *              * Se si sta trattando un codice di magazzino      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare dell'area di    *
      *                  * lavoro per la composizione                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-des-scr-000          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to cpz-des-rgs-400
           else if   w-prs-rig-scr-des    =    01
                     go to cpz-des-rgs-420
           else if   w-prs-rig-scr-des    =    02
                     go to cpz-des-rgs-440
           else if   w-prs-rig-scr-des    =    03
                     go to cpz-des-rgs-460
           else if   w-prs-rig-scr-des    =    04
                     go to cpz-des-rgs-480
           else if   w-prs-rig-scr-des    =    05
                     go to cpz-des-rgs-500
           else if   w-prs-rig-scr-des    =    06
                     go to cpz-des-rgs-520
           else      go to cpz-des-rgs-400.
       cpz-des-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 40 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-000-d40      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-001-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-002-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-003-d25      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-480.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-004-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-004-d21      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-500.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-005-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-520.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-006-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-orc-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata primo ordine cliente richiamato   *
      *    *-----------------------------------------------------------*
       buf-tes-orc-000.
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
      *              * Tentativo di lettura record [ocx]               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ocx]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ocx]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-orc        to   rf-ocx-num-prt         .
           move      zero                 to   rf-ocx-num-prg         .
           move      01                   to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
      *              *-------------------------------------------------*
      *              * Flag di accettazione pagina testata 1           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-pt1 (1)      .
      *              *-------------------------------------------------*
      *              * Flag di accettazione pagina testata 2           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-pt2 (1)      .
      *              *-------------------------------------------------*
      *              * Flag di accettazione sigla valuta per fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-tes-fda-vpf (1)      .
       buf-tes-orc-100.
      *              *-------------------------------------------------*
      *              * Valori contenuti direttamente in record [oct]   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori necessari in ogni caso               *
      *                  *---------------------------------------------*
           move      rf-oct-tip-arc       to   w-tes-tip-arc (1)      .
           move      rf-oct-cod-arc       to   w-tes-cod-arc (1)      .
           move      rf-oct-dpz-arc       to   w-tes-dpz-arc (1)      .
           move      rf-oct-cod-lng       to   w-tes-cod-lng (1)      .
           move      rf-oct-tip-ids       to   w-tes-tip-ids (1)      .
           move      rf-oct-tip-frn       to   w-tes-tip-frn (1)      .
           move      rf-oct-arc-plf       to   w-tes-arc-plf (1)      .
           move      rf-oct-dpz-plf       to   w-tes-dpz-plf (1)      .
           if        w-tes-tip-frn (1)    =    21
                     move  rf-oct-tip-ftz to   w-tes-tip-ftz (1)      .
           move      rf-oct-ass-iva       to   w-tes-ass-iva (1)      .
           move      rf-oct-ctp-ven       to   w-tes-ctp-ven (1)      .
           if        rf-oct-fat-sep       =    "S"
                     move  "O"            to   w-tes-fat-sep (1)      .
           move      rf-oct-inl-pgt       to   w-tes-inl-pgt (1)      .
           move      rf-oct-sgl-vpf       to   w-tes-sgl-vpf (1)      .
           move      rf-oct-dec-vpf       to   w-tes-dec-vpf (1)      .
           move      rf-oct-tdc-vpf       to   w-tes-tdc-vpf (1)      .
           move      rf-oct-cod-lst       to   w-tes-cod-lst (1)      .
           move      rf-oct-csr-aac       to   w-tes-csr-aac (1)      .
           move      rf-oct-psr-aac (1)   to   w-tes-psr-aac (1, 1)   .
           move      rf-oct-psr-aac (2)   to   w-tes-psr-aac (1, 2)   .
           move      rf-oct-psr-aac (3)   to   w-tes-psr-aac (1, 3)   .
           move      rf-oct-psr-aac (4)   to   w-tes-psr-aac (1, 4)   .
           move      rf-oct-psr-aac (5)   to   w-tes-psr-aac (1, 5)   .
           move      rf-oct-csc-aac       to   w-tes-csc-aac (1)      .
           move      rf-oct-psc-aac       to   w-tes-psc-aac (1)      .
           move      rf-oct-cpv-aac       to   w-tes-cpv-aac (1)      .
           move      rf-oct-ppv-aac (1)   to   w-tes-ppv-aac (1, 1)   .
           move      rf-oct-ppv-aac (2)   to   w-tes-ppv-aac (1, 2)   .
           move      rf-oct-ppv-aac (3)   to   w-tes-ppv-aac (1, 3)   .
           move      rf-oct-voc-des (1)   to   w-tes-voc-des (1, 1)   .
           move      rf-oct-voc-des (2)   to   w-tes-voc-des (1, 2)   .
           move      rf-oct-voc-des (3)   to   w-tes-voc-des (1, 3)   .
           move      rf-oct-voc-des (4)   to   w-tes-voc-des (1, 4)   .
           move      rf-oct-voc-des (5)   to   w-tes-voc-des (1, 5)   .
           move      rf-oct-voc-des (6)   to   w-tes-voc-des (1, 6)   .
           move      rf-oct-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-oct-scp-aap       to   w-tes-scp-aap (1)      .
           move      rf-oct-cod-abi       to   w-tes-cod-abi (1)      .
           move      rf-oct-cod-cab       to   w-tes-cod-cab (1)      .
           move      rf-oct-ccc-app       to   w-tes-ccc-app (1)      .
           move      rf-oct-nos-ban       to   w-tes-nos-ban (1)      .
           move      rf-oct-nos-ccp       to   w-tes-nos-ccp (1)      .
           move      rf-oct-add-spi       to   w-tes-add-spi (1)      .
           move      rf-oct-add-spb       to   w-tes-add-spb (1)      .
           move      rf-oct-pag-dsm       to   w-tes-pag-dsm (1)      .
           move      rf-oct-pag-qaf       to   w-tes-pag-qaf (1)      .
           move      rf-oct-pag-act       to   w-tes-pag-act (1)      .
           move      rf-oct-cod-age       to   w-tes-cod-age (1)      .
           move      rf-oct-fsp-doc       to   w-tes-fsp-doc (1)      .
           move      rf-oct-pvf-age       to   w-tes-pvf-age (1)      .
           move      rf-oct-tip-vpa       to   w-tes-tip-vpa (1)      .
           move      rf-oct-cpv-aaa       to   w-tes-cpv-aaa (1)      .
           move      rf-oct-ppv-aaa (1)   to   w-tes-ppv-aaa (1, 1)   .
           move      rf-oct-ppv-aaa (2)   to   w-tes-ppv-aaa (1, 2)   .
           move      rf-oct-ppv-aaa (3)   to   w-tes-ppv-aaa (1, 3)   .
           move      rf-oct-cod-ime       to   w-tes-cod-ime (1)      .
           move      rf-oct-pvf-ime       to   w-tes-pvf-ime (1)      .
           move      rf-oct-tot-scp       to   w-pie-tot-scp (1)      .
           move      rf-oct-per-scp       to   w-pie-per-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Codice vettore alternativo, se indicato in  *
      *                  * ordine                                      *
      *                  *---------------------------------------------*
           if        rf-oct-cod-vet       not  = zero
                     move  rf-oct-cod-vet to   w-pie-cod-vet (1)      .
       buf-tes-orc-110.
      *                  *---------------------------------------------*
      *                  * Spese per fatturazione                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da bufferizzare                 *
      *                      *-----------------------------------------*
           if        w-prs-ttr-spf        not  = 00
                     go to buf-tes-orc-170.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione da record [oct]         *
      *                      *-----------------------------------------*
           move      zero                 to   w-buf-tes-orc-ctr      .
       buf-tes-orc-112.
           add       1                    to   w-buf-tes-orc-ctr      .
           if        w-buf-tes-orc-ctr    >    6
                     go to buf-tes-orc-170.
           move      rf-oct-spe-snx
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-snx
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-mad
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-mad
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-per
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-per
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-ibl
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-ibl
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-ibt-spe
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-ibt
                                              (1, w-buf-tes-orc-ctr)  .
           move      rf-oct-spe-imp
                    (w-buf-tes-orc-ctr)   to   w-pie-spe-imp
                                              (1, w-buf-tes-orc-ctr)  .
           go to     buf-tes-orc-112.
       buf-tes-orc-170.
      *              *-------------------------------------------------*
      *              * Valori contenuti indirettamente in record [oct] *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica archivio                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo archi-  *
      *                      * vio                                     *
      *                      *-----------------------------------------*
           if        w-tes-tip-arc (1)    =    "C"
                     go to buf-tes-orc-172
           else      go to buf-tes-orc-180.
       buf-tes-orc-172.
      *                      *-----------------------------------------*
      *                      * Tipo archivio : Clienti                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura file [cli]                  *
      *                          *-------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cod-arc-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-cod-arc-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-cod-arc-loc (1)  .
           move      w-let-arc-cli-piv    to   w-tes-cod-arc-piv (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-arc-stc (1)  .
      *                          *-------------------------------------*
      *                          * Test su codice sottoconto associato *
      *                          * al cliente                          *
      *                          *-------------------------------------*
           if        w-tes-cod-arc-stc (1)
                                          not  = zero
                     go to buf-tes-orc-173.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca il sottoconto contabile per il cliente !    
      -              "               "    to   w-err-box-err-msg      .
      *                              *---------------------------------*
      *                              * A trattamento errore            *
      *                              *---------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-173.
      *                          *-------------------------------------*
      *                          * Lettura record [dcc] principale     *
      *                          *-------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Test su esito lettura               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-orc-174.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "               "    to   w-err-box-err-msg      .
      *                              *---------------------------------*
      *                              * A trattamento errore            *
      *                              *---------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-174.
      *                          *-------------------------------------*
      *                          * Test su segnale di consegne blocca- *
      *                          * te                                  *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-blo    not  = 02
                     go to buf-tes-orc-174-001.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "ATTENZIONE : Cliente con consegne bloccate !      
      -              "               "    to   w-err-box-err-msg      .
      *                              *---------------------------------*
      *                              * A seconda della personalizza-   *
      *                              * zione relativa alla modalita'   *
      *                              * di blocco del cliente si prose- *
      *                              * gue o meno                      *
      *                              *---------------------------------*
           if        w-prs-blo-cli-t01    =    "B"
                     go to buf-tes-orc-900.
      *                              *---------------------------------*
      *                              * Emissione avviso                *
      *                              *---------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
       buf-tes-orc-174-001.
      *                          *-------------------------------------*
      *                          * Determinazione valori testata in-   *
      *                          * dotti da record [dcc] principale    *
      *                          *-------------------------------------*
           move      "C"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
       buf-tes-orc-174-002.
      *                          *-------------------------------------*
      *                          * Spese per fatturazione se da rical- *
      *                          * colare da anagrafica                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su personalizzazione per   *
      *                              * modalita' di addebito           *
      *                              *---------------------------------*
           if        w-prs-ttr-spf        not  = 01
                     go to buf-tes-orc-175.
      *                              *---------------------------------*
      *                              * Determinazione funzionamento    *
      *                              * spese                           *
      *                              *---------------------------------*
           move      "C"                  to   w-det-fun-spe-tdt      .
           perform   det-fun-spe-000      thru det-fun-spe-999        .
       buf-tes-orc-175.
           go to     buf-tes-orc-180.
       buf-tes-orc-180.
      *                  *---------------------------------------------*
      *                  * Anagrafica dipendenza archivio              *
      *                  *---------------------------------------------*
           if        w-tes-dpz-arc (1)    =    spaces
                     go to buf-tes-orc-200.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo archi-  *
      *                      * vio                                     *
      *                      *-----------------------------------------*
           if        w-tes-tip-arc (1)    =    "C"
                     go to buf-tes-orc-182
           else      go to buf-tes-orc-200.
       buf-tes-orc-182.
      *                      *-----------------------------------------*
      *                      * Tipo archivio : Clienti                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura file [dcc]                  *
      *                          *-------------------------------------*
           move      w-tes-cod-arc (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpz-arc (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpz-arc-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpz-arc-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-arc-loc (1)  .
      *                          *-------------------------------------*
      *                          * Determinazione valori testata in-   *
      *                          * dotti da record [dcc] relativo alla *
      *                          * dipendenza del cliente              *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-orc-183.
           move      "C"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpz-arc (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-orc-183.
      *                          *-------------------------------------*
      *                          * Determinazione valori piede asso-   *
      *                          * ciati al vettore                    *
      *                          *-------------------------------------*
           perform   det-vlp-vet-000      thru det-vlp-vet-999        .
           go to     buf-tes-orc-200.
       buf-tes-orc-200.
      *                  *---------------------------------------------*
      *                  * Indirizzo di spedizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo indi-   *
      *                      * rizzo di spedizione                     *
      *                      *-----------------------------------------*
           go to     buf-tes-orc-201
                     buf-tes-orc-202
                     buf-tes-orc-203
                     buf-tes-orc-204
                     depending            on   w-tes-tip-ids (1)      .
       buf-tes-orc-201.
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione : Sede legale   *
      *                      *-----------------------------------------*
           move      w-tes-cod-arc-rag (1)
                                          to   w-tes-rag-ids (1)      .
           move      spaces               to   w-tes-rs2-ids (1)      .
           move      w-tes-cod-arc-via (1)
                                          to   w-tes-via-ids (1)      .
           move      w-tes-cod-arc-loc (1)
                                          to   w-tes-loc-ids (1)      .
           go to     buf-tes-orc-210.
       buf-tes-orc-202.
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione : Manuale       *
      *                      *-----------------------------------------*
           move      rf-ocx-rag-ids       to   w-tes-rag-ids (1)      .
           move      rf-ocx-rs2-ids       to   w-tes-rs2-ids (1)      .
           move      rf-ocx-via-ids       to   w-tes-via-ids (1)      .
           move      rf-ocx-loc-ids       to   w-tes-loc-ids (1)      .
           go to     buf-tes-orc-210.
       buf-tes-orc-203.
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione : Sede          *
      *                      *-----------------------------------------*
           move      w-tes-rag-sed (1)    to   w-tes-rag-ids (1)      .
           move      spaces               to   w-tes-rs2-ids (1)      .
           move      w-tes-via-sed (1)    to   w-tes-via-ids (1)      .
           move      w-tes-loc-sed (1)    to   w-tes-loc-ids (1)      .
           go to     buf-tes-orc-210.
       buf-tes-orc-204.
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione : Dipendenza    *
      *                      *-----------------------------------------*
           move      w-tes-dpz-arc-rag (1)
                                          to   w-tes-rag-ids (1)      .
           move      spaces               to   w-tes-rs2-ids (1)      .
           move      w-tes-dpz-arc-via (1)
                                          to   w-tes-via-ids (1)      .
           move      w-tes-dpz-arc-loc (1)
                                          to   w-tes-loc-ids (1)      .
           go to     buf-tes-orc-210.
       buf-tes-orc-210.
      *                  *---------------------------------------------*
      *                  * Archivio per la fatturazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se fornitura diretta: oltre             *
      *                      *-----------------------------------------*
           if        w-tes-tip-frn (1)    =    01
                     go to buf-tes-orc-220.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente per fatturazione     *
      *                      *-----------------------------------------*
           move      w-tes-arc-plf (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-arc-plf-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-arc-plf-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-arc-plf-loc (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-arc-stc (1)  .
      *                      *-----------------------------------------*
      *                      * Test su codice sottoconto associato al  *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           if        w-tes-cod-arc-stc (1)
                                          not  = zero
                     go to buf-tes-orc-212.
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Manca il sottoconto contabile del cliente per fatt
      -              "urazione !     "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-212.
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale principale cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-arc-plf (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Se record non esistente             *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-orc-214.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Non esiste l'anagrafica commerciale del cliente pe
      -              "r fatturazione "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                              *---------------------------------*
      *                              * A trattamento errore            *
      *                              *---------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-214.
      *                          *-------------------------------------*
      *                          * Determinazione valori testata in-   *
      *                          * dotti da record [dcc] principale    *
      *                          *-------------------------------------*
           move      "F"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
      *                          *-------------------------------------*
      *                          * Spese per fatturazione se da rical- *
      *                          * colare da anagrafica                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su personalizzazione per   *
      *                              * modalita' di addebito           *
      *                              *---------------------------------*
           if        w-prs-ttr-spf        not  = 01
                     go to buf-tes-orc-215.
      *                              *---------------------------------*
      *                              * Determinazione funzionamento    *
      *                              * spese                           *
      *                              *---------------------------------*
           move      "F"                  to   w-det-fun-spe-tdt      .
           perform   det-fun-spe-000      thru det-fun-spe-999        .
       buf-tes-orc-215.
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale dipendenza cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-arc-plf (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpz-plf (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpz-plf-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpz-plf-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-plf-loc (1)  .
      *                          *-------------------------------------*
      *                          * Determinazione valori testata in-   *
      *                          * dotti da record [dcc] relativo alla *
      *                          * dipendenza del cliente per fattura- *
      *                          * zione                               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-orc-220.
           move      "F"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpz-plf (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-orc-220.
      *                  *---------------------------------------------*
      *                  * Anagrafica valuta                           *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vpf-des (1)  .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio valuta per fattura-  *
      *                  * zione alla data del documento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-tes-sgl-vpf (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-tes-tdc-vpf (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione            *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-ope    =    "CC"
                     go to buf-tes-orc-230.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Coefficiente di cambio indeterminato !            
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-orc-900.
       buf-tes-orc-230.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione coefficiente di cambio  *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tes-cdc-vpf (1)      .
       buf-tes-orc-260.
      *                  *---------------------------------------------*
      *                  * Tabella codici iva per descrizione assog-   *
      *                  * gettamento                                  *
      *                  *---------------------------------------------*
           move      w-tes-ass-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-ass-iva-des (1)  .
           if        w-tes-ass-iva (1)    =    zero
                     move  "Soggetto ad iva"
                                          to   w-tes-ass-iva-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica piano dei conti                  *
      *                  *---------------------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica voci descrittive                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-orc-ctr      .
       buf-tes-orc-270.
           add       1                    to   w-buf-tes-orc-ctr      .
           if        w-buf-tes-orc-ctr    >    6
                     go to buf-tes-orc-280.
           move      w-buf-tes-orc-ctr    to   w-let-arc-zvf-num      .
           move      w-tes-voc-des
                    (1, w-buf-tes-orc-ctr)
                                          to   w-let-arc-zvf-cod      .
           perform   let-arc-zvf-000      thru let-arc-zvf-999        .
           move      w-let-arc-zvf-des    to   w-tes-voc-des-des
                                              (1, w-buf-tes-orc-ctr)  .
           go to     buf-tes-orc-270.
       buf-tes-orc-280.
      *                  *---------------------------------------------*
      *                  * Anagrafica listini                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-lst (1)    to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
           move      w-let-arc-zls-des    to   w-tes-cod-lst-des (1)  .
           move      w-let-arc-zls-drg    to   w-tes-cod-lst-drg (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in riga      *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zcs-tip      .
           move      w-tes-csr-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csr-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psr-acc (1, 1)   .
           move      w-let-arc-zcs-per (2)
                                          to   w-tes-psr-acc (1, 2)   .
           move      w-let-arc-zcs-per (3)
                                          to   w-tes-psr-acc (1, 3)   .
           move      w-let-arc-zcs-per (4)
                                          to   w-tes-psr-acc (1, 4)   .
           move      w-let-arc-zcs-per (5)
                                          to   w-tes-psr-acc (1, 5)   .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in chiusura  *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zcs-tip      .
           move      w-tes-csc-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csc-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psc-acc (1)      .
      *                  *---------------------------------------------*
      *                  * Anagrafica agenti                           *
      *                  *---------------------------------------------*
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-rag    to   w-tes-cod-age-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop (1)    to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
           move      w-let-arc-zfp-des    to   w-tes-cod-fop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica istituti di credito              *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-cod-abi-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica sportelli istituti di credito    *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cod-cab (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cod-cab-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostra banca per bonifico        *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ban (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ban-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostro c/c postale per pagamento *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ccp (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ccp-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese incasso          *
      *                  *---------------------------------------------*
           move      w-tes-add-spi (1)    to   w-let-arc-zin-cod      .
           move      00                   to   w-let-arc-zin-tpg      .
           perform   let-arc-zin-000      thru let-arc-zin-999        .
           move      w-let-arc-zin-des    to   w-tes-add-spi-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese odslo            *
      *                  *---------------------------------------------*
           move      w-tes-add-spb (1)    to   w-let-arc-zbo-cod      .
           move      00                   to   w-let-arc-zbo-tpg      .
           perform   let-arc-zbo-000      thru let-arc-zbo-999        .
           move      w-let-arc-zbo-des    to   w-tes-add-spb-des (1)  .
       buf-tes-orc-300.
      *              *-------------------------------------------------*
      *              * Trattamento sconto in chiusura                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione percentuale sconto in chiu- *
      *                  * sura                                        *
      *                  *---------------------------------------------*
           move      rf-oct-per-scc       to   w-pie-per-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione in work di comodo totale     *
      *                  * sconto in chiusura ordine cliente           *
      *                  *---------------------------------------------*
           move      rf-oct-tot-scc       to   w-orc-tot-scc          .
      *                  *---------------------------------------------*
      *                  * Determinazione totale lordo ordine cliente  *
      *                  *---------------------------------------------*
           move      zero                 to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (01)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (02)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (03)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (09)  to   w-orc-tot-lor          .
       buf-tes-orc-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-orc-999.
       buf-tes-orc-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con status ad errore                 *
      *                  *---------------------------------------------*
           move      "#"                  to   l-eva-orc-exi-sts      .
       buf-tes-orc-999.
           exit.

      *    *===========================================================*
      *    * Confronto tra dati testata ordine cliente in esame con    *
      *    * dati testata del documento                                *
      *    *-----------------------------------------------------------*
       cnf-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Valori bloccanti                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque bloccanti                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo e codice archivio                  *
      *                      *-----------------------------------------*
           if        rf-oct-tip-arc       =    w-tes-tip-arc (1) and
                     rf-oct-cod-arc       =    w-tes-cod-arc (1)
                     go to cnf-tes-doc-002.
           move      "Codice cliente diverso da quello impostato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-002.
      *                      *-----------------------------------------*
      *                      * Codice lingua                           *
      *                      *-----------------------------------------*
           if        rf-oct-cod-lng       =    w-tes-cod-lng (1)
                     go to cnf-tes-doc-003.
           move      "Codice lingua diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-003.
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione                 *
      *                      *-----------------------------------------*
           if        rf-oct-tip-ids       =    w-tes-tip-ids (1)
                     go to cnf-tes-doc-010.
           move      "Tipo indirizzo di spedizione diverso da quello imp
      -              "ostato !       "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-010.
      *                      *-----------------------------------------*
      *                      * Test su tipo fornitura                  *
      *                      *-----------------------------------------*
           if        rf-oct-tip-frn       =    w-tes-tip-frn (1)
                     go to cnf-tes-doc-041.
           move      "Tipo fornitura diverso da quello impostato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-041.
      *                      *-----------------------------------------*
      *                      * Test su cliente per fatturazione        *
      *                      *-----------------------------------------*
           if        rf-oct-arc-plf       =    w-tes-arc-plf (1)
                     go to cnf-tes-doc-042.
           move      "Cliente per fatturazione diverso da quello imposta
      -              "to !           "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-042.
      *                      *-----------------------------------------*
      *                      * Test su assoggettamento iva             *
      *                      *-----------------------------------------*
           if        rf-oct-ass-iva       =    w-tes-ass-iva (1)
                     go to cnf-tes-doc-044.
           move      "Assoggettamento iva diverso da quello impostato ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-044.
      *                      *-----------------------------------------*
      *                      * Test su contropartita vendite           *
      *                      *-----------------------------------------*
           if        rf-oct-ctp-ven       =    w-tes-ctp-ven (1)
                     go to cnf-tes-doc-046.
           move      "Contropartita vendite diversa da quello impostato 
      -              "!              "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-046.
      *                      *-----------------------------------------*
      *                      * Test su inoltro pagamenti               *
      *                      *-----------------------------------------*
           if        rf-oct-inl-pgt       =    w-tes-inl-pgt (1)
                     go to cnf-tes-doc-048.
           move      "Inoltro pagamenti diverso da quello impostato !   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-048.
      *                      *-----------------------------------------*
      *                      * Test su sigla valuta per fatturazione   *
      *                      *-----------------------------------------*
           if        rf-oct-sgl-vpf       =    w-tes-sgl-vpf (1)
                     go to cnf-tes-doc-050.
           move      "Sigla valuta per fatturazione diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-050.
      *                      *-----------------------------------------*
      *                      * Test su decimali valuta per fatturazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           if        rf-oct-dec-vpf       =    w-tes-dec-vpf (1)
                     go to cnf-tes-doc-052.
           move      "Numero decimali valuta per fatturazione diverso da
      -              " quello della  "    to   w-err-box-err-msg      .
           move      " valuta impostata !                               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-052.
      *                      *-----------------------------------------*
      *                      * Test su tipo di cambio valuta per fat-  *
      *                      * turazione                               *
      *                      *-----------------------------------------*
           if        rf-oct-tdc-vpf       =    w-tes-tdc-vpf (1)
                     go to cnf-tes-doc-053.
           move      "Tipo di cambio valuta per fatturazione diverso da 
      -              "quello della   "    to   w-err-box-err-msg      .
           move      " valuta impostata !                               
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-053.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza pagamento manuale *
      *                      *-----------------------------------------*
           if        rf-oct-pag-dsm       =    w-tes-pag-dsm (1)
                     go to cnf-tes-doc-054.
           move      "Data scadenza pagamento manuale diversa da quella 
      -              "impostata !    "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-054.
      *                      *-----------------------------------------*
      *                      * Test su quota a forfait pagamento       *
      *                      *-----------------------------------------*
           if        rf-oct-pag-qaf       =    w-tes-pag-qaf (1)
                     go to cnf-tes-doc-056.
           move      "Quota a forfait del pagamento diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-056.
      *                      *-----------------------------------------*
      *                      * Test su codice agente                   *
      *                      *-----------------------------------------*
           if        rf-oct-cod-age       =    w-tes-cod-age (1)
                     go to cnf-tes-doc-058.
           move      "Codice agente diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-058.
      *                      *-----------------------------------------*
      *                      * Test su trattamento provvigioni         *
      *                      *-----------------------------------------*
           if        rf-oct-fsp-doc       =    w-tes-fsp-doc (1)
                     go to cnf-tes-doc-060.
           move      "Trattamento provvigionale diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-060.
      *                      *-----------------------------------------*
      *                      * Test su tipo vendita per l'agente       *
      *                      *-----------------------------------------*
           if        rf-oct-tip-vpa       =    w-tes-tip-vpa (1)
                     go to cnf-tes-doc-062.
           move      "Tipo vendita per l'agente diversa da quella impost
      -              "ata !          "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-062.
      *                      *-----------------------------------------*
      *                      * Test su fatturazione separata per il    *
      *                      * documento                               *
      *                      *-----------------------------------------*
           if        rf-oct-fat-sep       not  = "S"
                     go to cnf-tes-doc-064.
           move      "L'ordine richiamato indica che deve essere fattura
      -              "ato a parte !  "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-064.
           go to     cnf-tes-doc-100.
       cnf-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Valori non-bloccanti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque non bloccanti               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su dipendenza archivio             *
      *                      *-----------------------------------------*
           if        rf-oct-dpz-arc       =    w-tes-dpz-arc (1)
                     go to cnf-tes-doc-120.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Dipendenza cliente diversa da quella impostata !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-120.
      *                      *-----------------------------------------*
      *                      * Test su dipendenza archivio per fattu-  *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           if        rf-oct-dpz-plf       =    w-tes-dpz-plf (1)
                     go to cnf-tes-doc-162.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Dipendenza cliente per fatturazione diversa da que
      -              "la impostata ! "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-162.
      *                      *-----------------------------------------*
      *                      * Test su forma di pagamento              *
      *                      *-----------------------------------------*
           if        rf-oct-cod-fop       =    w-tes-cod-fop (1)
                     go to cnf-tes-doc-164.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Forma di pagamento diversa da quella impostata !"
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-164.
      *                      *-----------------------------------------*
      *                      * Test su codice ABI d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-abi (1)    =    zero
                     go to cnf-tes-doc-166.
           if        rf-oct-cod-abi       =    w-tes-cod-abi (1)
                     go to cnf-tes-doc-166.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice ABI per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-166.
      *                      *-----------------------------------------*
      *                      * Test su codice CAB d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-cab (1)    =    zero
                     go to cnf-tes-doc-168.
           if        rf-oct-cod-cab       =    w-tes-cod-cab (1)
                     go to cnf-tes-doc-168.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice CAB per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-168.
      *                      *-----------------------------------------*
      *                      * Test su c/c d'appoggio                  *
      *                      *-----------------------------------------*
           if        w-tes-ccc-app (1)    =    spaces
                     go to cnf-tes-doc-170.
           if        rf-oct-ccc-app       =    w-tes-ccc-app (1)
                     go to cnf-tes-doc-170.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "C/C del cliente per l'appoggio diverso da quello i
      -              "mpostato !     "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-170.
      *                      *-----------------------------------------*
      *                      * Test su nostra banca per bonifico       *
      *                      *-----------------------------------------*
           if        w-tes-nos-ban (1)    =    spaces
                     go to cnf-tes-doc-172.
           if        rf-oct-nos-ban       =    w-tes-nos-ban (1)
                     go to cnf-tes-doc-172.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostra banca per appoggio bonifico diversa da quel
      -              "la impostata ! "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-172.
      *                      *-----------------------------------------*
      *                      * Test su nostro c/c postale per bonifico *
      *                      *-----------------------------------------*
           if        w-tes-nos-ccp (1)    =    spaces
                     go to cnf-tes-doc-174.
           if        rf-oct-nos-ccp       =    w-tes-nos-ccp (1)
                     go to cnf-tes-doc-174.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostro c/c postale diverso da quello impostato !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-174.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-500.
       cnf-tes-doc-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per testata documento *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Acconto pagamento per il documento          *
      *                  *---------------------------------------------*
           add       rf-oct-pag-act       to   w-tes-pag-act (1)      .
      *                  *---------------------------------------------*
      *                  * Provvigioni a forfait per il documento      *
      *                  *---------------------------------------------*
           add       rf-oct-pvf-age       to   w-tes-pvf-age (1)      .
       cnf-tes-doc-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per piede documento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione in work di comodo totale     *
      *                  * sconto in chiusura ordine cliente           *
      *                  *---------------------------------------------*
           move      rf-oct-tot-scc       to   w-orc-tot-scc          .
      *                  *---------------------------------------------*
      *                  * Determinazione totale lordo ordine cliente  *
      *                  *---------------------------------------------*
           move      zero                 to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (01)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (02)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (03)  to   w-orc-tot-lor          .
           add       rf-oct-tot-rig (09)  to   w-orc-tot-lor          .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-oct-per-scc       not  = w-pie-per-scc (1)
                     move  zero           to   w-pie-per-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Totale sconto pagamento                     *
      *                  *---------------------------------------------*
           add       rf-oct-tot-scp       to   w-pie-tot-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto pagamento                *
      *                  *---------------------------------------------*
           if        rf-oct-per-scp       not  = w-pie-per-scp (1)
                     move  zero           to   w-pie-per-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Spese in fattura                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da aggiornare                   *
      *                      *-----------------------------------------*
           if        w-prs-ttr-spf        not  = 00
                     go to cnf-tes-doc-800.
      *                      *-----------------------------------------*
      *                      * Importi spese in fattura                *
      *                      *-----------------------------------------*
           add       rf-oct-spe-imp (1)   to   w-pie-spe-imp (1, 1)   .
           add       rf-oct-spe-imp (2)   to   w-pie-spe-imp (1, 2)   .
           add       rf-oct-spe-imp (3)   to   w-pie-spe-imp (1, 3)   .
           add       rf-oct-spe-imp (4)   to   w-pie-spe-imp (1, 4)   .
           add       rf-oct-spe-imp (5)   to   w-pie-spe-imp (1, 5)   .
           add       rf-oct-spe-imp (6)   to   w-pie-spe-imp (1, 6)   .
      *                      *-----------------------------------------*
      *                      * Percentuali spese in fattura            *
      *                      *-----------------------------------------*
           if        rf-oct-spe-snx (1)   =    0 or
                     rf-oct-spe-per (1)   not  = w-pie-spe-per (1, 1)
                     move  zero           to   w-pie-spe-per (1, 1)   .
           if        rf-oct-spe-snx (2)   =    0 or
                     rf-oct-spe-per (2)   not  = w-pie-spe-per (1, 2)
                     move  zero           to   w-pie-spe-per (1, 2)   .
           if        rf-oct-spe-snx (3)   =    0 or
                     rf-oct-spe-per (3)   not  = w-pie-spe-per (1, 3)
                     move  zero           to   w-pie-spe-per (1, 3)   .
           if        rf-oct-spe-snx (4)   =    0 or
                     rf-oct-spe-per (4)   not  = w-pie-spe-per (1, 4)
                     move  zero           to   w-pie-spe-per (1, 4)   .
           if        rf-oct-spe-snx (5)   =    0 or
                     rf-oct-spe-per (5)   not  = w-pie-spe-per (1, 5)
                     move  zero           to   w-pie-spe-per (1, 5)   .
           if        rf-oct-spe-snx (6)   =    0 or
                     rf-oct-spe-per (6)   not  = w-pie-spe-per (1, 6)
                     move  zero           to   w-pie-spe-per (1, 6)   .
       cnf-tes-doc-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cnf-tes-doc-999.
       cnf-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-950.
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   l-eva-orc-exi-sts      .
       cnf-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
      *    *-----------------------------------------------------------*
       sdc-eva-orc-000.
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
      *              * Richiamo funzione di saldaconto                 *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
           if        w-sdc-flg-exi        not  = spaces
                     move  "#"            to   l-eva-orc-exi-sts      .
       sdc-eva-orc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-exi          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-bro-num-ele        to   w-sdc-npg-max          .
           divide    14                   into w-sdc-npg-max
                                        giving w-sdc-npg-max
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-max          .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           perform   pmt-fun-sdc-000      thru pmt-fun-sdc-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina visualizzata                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-bro-num-ele
                     go to  acc-fun-sdc-800.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    14                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina da trattare              *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        not  = w-sdc-npg-vis
                     perform vis-pag-sdc-000
                                          thru vis-pag-sdc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-sdc-npg-dat
                                        giving w-sdc-wrk-rig          .
           multiply  14                   by   w-sdc-wrk-rig          .
           subtract  w-sdc-wrk-rig        from w-sdc-ctr-rig
                                        giving w-sdc-wrk-lin          .
           add       7                    to   w-sdc-wrk-lin          .
      *              *-------------------------------------------------*
      *              * Se Tab in corso                                 *
      *              *-------------------------------------------------*
           if        l-eva-orc-fky-tab    =    spaces
                     go to acc-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di Tab in corso        *
      *                  *---------------------------------------------*
           move      spaces               to   l-eva-orc-fky-tab      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' da spedire                        *
      *                  *---------------------------------------------*
           perform   acc-qta-dsp-000      thru acc-qta-dsp-999        .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           perform   acc-snx-aoc-000      thru acc-snx-aoc-999        .
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-fun-sdc-920.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-500.
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-500.
      *                      *-----------------------------------------*
      *                      * Se Prev Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to  acc-fun-sdc-600.
           subtract  1                    from w-sdc-npg-dat          .
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  13                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-600.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                =    "TAB "
                     move   w-bro-num-ele to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "BACK"
                     move   1             to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-200.
       acc-fun-sdc-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-fun-sdc-810.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-fun-sdc-810.
      *              *-------------------------------------------------*
      *              * Test su risposta dell'utente                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900
           else if   v-key                =    "EXIT"
                     go to acc-fun-sdc-920
           else if   v-key                =    "UP  "
                     go to acc-fun-sdc-820
           else      go to acc-fun-sdc-800.
       acc-fun-sdc-820.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           subtract  1                    from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-999.
       acc-fun-sdc-920.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sdc-flg-exi          .
           go to     acc-fun-sdc-999.
       acc-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Funzione saldaconto              *
      *    *-----------------------------------------------------------*
       pmt-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-100.
      *              *-------------------------------------------------*
      *              * Intestazione documento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      w-acc-tmo-orc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-acc-tmo-orc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-200.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
       pmt-fun-sdc-320.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rig-scr-des    =    00
                     go to pmt-fun-sdc-340
           else if   w-prs-rig-scr-des    =    01
                     go to pmt-fun-sdc-360
           else if   w-prs-rig-scr-des    =    02
                     go to pmt-fun-sdc-380
           else if   w-prs-rig-scr-des    =    03
                     go to pmt-fun-sdc-400
           else if   w-prs-rig-scr-des    =    04
                     go to pmt-fun-sdc-420
           else if   w-prs-rig-scr-des    =    05
                     go to pmt-fun-sdc-440
           else if   w-prs-rig-scr-des    =    06
                     go to pmt-fun-sdc-460
           else      go to pmt-fun-sdc-340.
       pmt-fun-sdc-340.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               Descrizione              |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------------------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-360.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- --------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-380.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------- ------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Codice            Descrizione       |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-------------- -------------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Codice            Descrizione     |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------ ---------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- --------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      |Da evader
      -              "e | Da spedire| I |  Residuo  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------- ------------------|---------
      -              "--|-----------|---|-----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-700.
      *              *-------------------------------------------------*
      *              * Colonne per saldaconto                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       pmt-fun-sdc-710.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to pmt-fun-sdc-900.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                        |         
      -              "  |           |   |           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-fun-sdc-710.
       pmt-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina saldaconto                         *
      *    *-----------------------------------------------------------*
       vis-pag-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Aggiornamento numero pagina visualizzata        *
      *              *-------------------------------------------------*
           move      w-sdc-npg-dat        to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Determinazione contatore righe iniziale         *
      *              *-------------------------------------------------*
           multiply  14                   by   w-sdc-npg-dat
                                      giving   w-sdc-wrk-rig          .
           subtract  14                   from w-sdc-wrk-rig          .
      *              *-------------------------------------------------*
      *              * Ciclo di visualizzazione                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       vis-pag-sdc-100.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to  vis-pag-sdc-900.
           add       1                    to   w-sdc-wrk-rig          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che sia una riga vuo-  *
      *                  * ta o piena                                  *
      *                  *---------------------------------------------*
           if        w-sdc-wrk-rig        >    w-bro-num-ele
                     go to vis-pag-sdc-200
           else      go to vis-pag-sdc-300.
       vis-pag-sdc-200.
      *                  *---------------------------------------------*
      *                  * Se riga vuota                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                        |         
      -              "  |           |   |           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-300.
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga piena                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-bro-des-rig
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' da evadere                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita' da incolonnare    *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-dev
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      42                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-bro-tip-rig
                    (w-sdc-wrk-rig)       not  = "A"
                     go to vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      59                   to   v-pos                  .
           if        w-bro-snx-aoc
                    (w-sdc-wrk-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A riciclo su riga successiva            *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-400.
       vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' da spedire                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-bro-dec-qta
                    (w-sdc-wrk-rig)       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      54                   to   v-pos                  .
           move      w-bro-qta-dsp
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di residuo          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-bro-flg-idr
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di saldo            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      68                   to   v-pos                  .
           move      w-bro-flg-ids
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Quantita' residua                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-res
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      70                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-400.
      *                      *-----------------------------------------*
      *                      * Riciclo su riga successiva              *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Quantita' da spedire      *
      *    *-----------------------------------------------------------*
       acc-qta-dsp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "A" or
                     w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-dsp-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-qta-dsp
                    (w-sdc-ctr-rig)       to   w-sav-qta-dsp          .
           move      w-bro-flg-fzs
                    (w-sdc-ctr-rig)       to   w-sav-flg-fzs          .
       acc-qta-dsp-100.
      *              *-------------------------------------------------*
      *              * Note operative                                  *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "[F1] - Tasto Funzione 1 per saldo riga ordine con 
      -              "quantita' in bolla a zero"
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "[1] "               to   v-pfk (14)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-qta-dsp
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Cancellazione eventuali note operative          *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-dsp-999.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-qta-dsp-200.
      *                  *---------------------------------------------*
      *                  * Controllo che il valore non sia stato va-   *
      *                  * riato                                       *
      *                  *---------------------------------------------*
           if        v-mod                not  = spaces
                     move  w-sav-qta-dsp  to   w-bro-qta-dsp
                                              (w-sdc-ctr-rig)
                     move  w-sav-flg-fzs  to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)
                     go to acc-qta-dsp-100.
      *                  *---------------------------------------------*
      *                  * Esecuzione ricerca per codice magazzino     *
      *                  *---------------------------------------------*
           perform   exe-rca-pcm-000      thru exe-rca-pcm-999        .
      *                  *---------------------------------------------*
      *                  * Se esito negativo : a reimpostazione        *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-nrg    =    zero
                     go to acc-qta-dsp-100.
      *                  *---------------------------------------------*
      *                  * Determinazione numero riga su cui posizio-  *
      *                  * narsi                                       *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-nrg    to   w-sdc-ctr-rig          .
           subtract  1                    from w-sdc-ctr-rig          .
      *                  *---------------------------------------------*
      *                  * Simulazione tasto Down                      *
      *                  *---------------------------------------------*
           move      "DOWN"               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-qta-dsp-999.
       acc-qta-dsp-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-bro-qta-dsp
                                              (w-sdc-ctr-rig)         .
       acc-qta-dsp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * da spedire e segno quantita' da evadere     *
      *                  *---------------------------------------------*
           if        w-bro-qta-dev
                    (w-sdc-ctr-rig)       >    zero and
                     w-bro-qta-dsp
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-dsp-100
           else if   w-bro-qta-dev
                    (w-sdc-ctr-rig)       <    zero and
                     w-bro-qta-dsp
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-dsp-100.
       acc-qta-dsp-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-dsp-600.
           if        w-bro-qta-dsp
                    (w-sdc-ctr-rig)       not  = w-sav-qta-dsp
                     move   w-sav-qta-dsp to   w-bro-qta-dsp
                                              (w-sdc-ctr-rig)
                     go to  acc-qta-dsp-100.
       acc-qta-dsp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere e quantita' da spedire *
      *                  * in work di comodo senza segno               *
      *                  *---------------------------------------------*
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-qss-qta-dev          .
           move      w-bro-qta-dsp
                    (w-sdc-ctr-rig)       to   w-qss-qta-dsp          .
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-dsp-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-bro-qta-dsp
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dsp-610.
      *                          *-------------------------------------*
      *                          * Quantita' da spedire                *
      *                          *-------------------------------------*
           move      zero                 to   w-bro-qta-dsp
                                              (w-sdc-ctr-rig)         .
           move      zero                 to   w-qss-qta-dsp          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-700.
       acc-qta-dsp-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' da spedire                *
      *                          *-------------------------------------*
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-bro-qta-dsp
                                              (w-sdc-ctr-rig)         .
           move      w-bro-qta-dev
                    (w-sdc-ctr-rig)       to   w-qss-qta-dsp          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-700.
       acc-qta-dsp-620.
      *                  *---------------------------------------------*
      *                  * Se Pf1                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to  acc-qta-dsp-640.
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se quantita' da spedire maggiore o  *
      *                          * uguale a quella da evadere : spaces *
      *                          *-------------------------------------*
           if        w-qss-qta-dsp        <    w-qss-qta-dev
                     go to  acc-qta-dsp-630.
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-700.
       acc-qta-dsp-630.
      *                          *-------------------------------------*
      *                          * Altrimenti : si inverte il valore   *
      *                          * attuale del flag                    *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     move  "S"            to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)
           else      move  spaces         to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-700.
       acc-qta-dsp-640.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se valore impostato pari al prece-  *
      *                          * dente : nessuna variazione          *
      *                          *-------------------------------------*
           if        w-bro-qta-dsp
                    (w-sdc-ctr-rig)        not  = w-sav-qta-dsp
                     go to  acc-qta-dsp-650.
           go to     acc-qta-dsp-700.
       acc-qta-dsp-650.
      *                          *-------------------------------------*
      *                          * Se valore impostato diverso dal     *
      *                          * precedente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' da spedire maggio- *
      *                              * re o uguale a quella da evade-  *
      *                              * re : spaces                     *
      *                              *---------------------------------*
           if        w-qss-qta-dsp        <    w-qss-qta-dev
                     go to  acc-qta-dsp-655.
           move      spaces               to   w-bro-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-700.
       acc-qta-dsp-655.
      *                              *---------------------------------*
      *                              * Altrimenti : valore inalterato  *
      *                              *---------------------------------*
           go to     acc-qta-dsp-700.
       acc-qta-dsp-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dsp-710.
           move      spaces               to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-720.
       acc-qta-dsp-710.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' da spedire maggio- *
      *                              * re o uguale a quella da evade-  *
      *                              * re : spaces                     *
      *                              *---------------------------------*
           if        w-qss-qta-dsp        <    w-qss-qta-dev
                     go to  acc-qta-dsp-715.
           move      spaces               to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-720.
       acc-qta-dsp-715.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a '<'                   *
      *                              *---------------------------------*
           move      "<"                  to   w-bro-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-720.
       acc-qta-dsp-720.
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dsp-730.
           move      "S"                  to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-740.
       acc-qta-dsp-730.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' da spedire minore  *
      *                              * o uguale a quella da evadere :  *
      *                              * viene forzato il valore spaces  *
      *                              *---------------------------------*
           if        w-qss-qta-dsp        >    w-qss-qta-dev
                     go to  acc-qta-dsp-735.
           move      spaces               to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-740.
       acc-qta-dsp-735.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a "+"                   *
      *                              *---------------------------------*
           move      "+"                  to   w-bro-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-740.
       acc-qta-dsp-740.
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-bro-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dsp-750.
           move      zero                 to   w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-760.
       acc-qta-dsp-750.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' da spedire maggio- *
      *                              * re o uguale a quella da evade-  *
      *                              * re : spaces                     *
      *                              *---------------------------------*
           if        w-qss-qta-dsp        <    w-qss-qta-dev
                     go to  acc-qta-dsp-755.
           move      zero                 to   w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-760.
       acc-qta-dsp-755.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore e' pari  *
      *                              * alla differenza tra quantita'   *
      *                              * da evadere e quantita' da spe-  *
      *                              * dire                            *
      *                              *---------------------------------*
           subtract  w-bro-qta-dsp
                    (w-sdc-ctr-rig)       from w-bro-qta-dev
                                              (w-sdc-ctr-rig)
                                        giving w-bro-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dsp-760.
       acc-qta-dsp-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' da spedire                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-bro-qta-dsp
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-bro-flg-idr
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-bro-flg-ids
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita'                   *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-res
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      70                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-dsp-800.
       acc-qta-dsp-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Si/No Addebito o Commento *
      *    *-----------------------------------------------------------*
       acc-snx-aoc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-sdc-ctr-rig        =    zero
                     go to acc-snx-aoc-999.
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-bro-tip-rig
                    (w-sdc-ctr-rig)       not  = "C"
                     go to acc-snx-aoc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-snx-aoc
                    (w-sdc-ctr-rig)       to   w-sav-snx-aoc          .
       acc-snx-aoc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-snx-aoc-150.
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-snx-aoc
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-snx-aoc-999.
       acc-snx-aoc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-snx-aoc-400.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = w-sav-snx-aoc
                     move   w-sav-snx-aoc to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
                     go to  acc-snx-aoc-100.
      *                  *---------------------------------------------*
      *                  * Se valore presente lo si abblenca, altri-   *
      *                  * menti lo si forza a 'S'                     *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
           else      move  "S"            to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
       acc-snx-aoc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "S" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "N" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-snx-aoc-100.
       acc-snx-aoc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       =    "N"
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Si/No addebito o commento   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-snx-aoc-800.
       acc-snx-aoc-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ricerca per codice magazzino                   *
      *    *-----------------------------------------------------------*
       exe-rca-pcm-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero riga su cui posizionar- *
      *              * si                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-nrg      .
      *              *-------------------------------------------------*
      *              * Salvataggio del video attuale                   *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-020.
      *              *-------------------------------------------------*
      *              * Accettazione tipo e codice magazzino da ricer-  *
      *              * care                                            *
      *              *-------------------------------------------------*
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
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione work-area di accettazione   *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-cnm      .
           move      spaces               to   w-exe-rca-pcm-cam      .
           move      spaces               to   w-exe-rca-pcm-dem      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Titolo centrale                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      "RICERCA PER CODICE PRODOTTO"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Tipo codice di magazzino                *
      *                      *-----------------------------------------*
       exe-rca-pcm-030.
      *                      *-----------------------------------------*
      *                      * Codice di magazzino                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Codice prodotto :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Carattere di presa visione              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo codice di magazzino                    *
      *                  *---------------------------------------------*
           perform   acc-tip-mag-000      thru acc-tip-mag-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
       exe-rca-pcm-200.
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico di magazzino            *
      *                  *---------------------------------------------*
           perform   acc-cod-mag-000      thru acc-cod-mag-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
           if        v-key                =    "UP  "
                     go to exe-rca-pcm-100.
       exe-rca-pcm-300.
      *                  *---------------------------------------------*
      *                  * Carattere di presa visione                  *
      *                  *---------------------------------------------*
           perform   acc-pre-vis-000      thru acc-pre-vis-999        .
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-900.
           if        v-key                =    "DO  "
                     go to exe-rca-pcm-400.
           if        v-key                =    "UP  "
                     go to exe-rca-pcm-200.
       exe-rca-pcm-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori necessari *
      *              * per la rcerca                                   *
      *              *-------------------------------------------------*
           if        w-exe-rca-pcm-cnm    =    zero
                     go to exe-rca-pcm-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione numzero elementi del buffer    *
      *              * numeri riga con codice prodotto pari a quello   *
      *              * impostato                                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ele      .
      *              *-------------------------------------------------*
      *              * Ricerca effettiva in buffer righe ordine        *
      *              *-------------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-420.
           add       1                    to   w-exe-rca-pcm-ctr      .
           if        w-exe-rca-pcm-ctr    >    w-bro-num-ele
                     go to exe-rca-pcm-500.
           if        w-exe-rca-pcm-cnm    =    w-bro-num-pro
                                              (w-exe-rca-pcm-ctr)
                     go to exe-rca-pcm-440.
           go to     exe-rca-pcm-420.
       exe-rca-pcm-440.
      *                  *---------------------------------------------*
      *                  * Se codice di magazzino trovato              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione numero riga in buffer   *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ele    <    w-exe-rca-pcm-max
                     add  1               to   w-exe-rca-pcm-ele      .
           move      w-exe-rca-pcm-ctr    to   w-exe-rca-pcm-bnr
                                              (w-exe-rca-pcm-ele)     .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     exe-rca-pcm-420.
       exe-rca-pcm-500.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del numero elementi buffe- *
      *              * rizzato                                         *
      *              *-------------------------------------------------*
           if        w-exe-rca-pcm-ele    =    zero
                     go to exe-rca-pcm-520
           else if   w-exe-rca-pcm-ele    =    1
                     go to exe-rca-pcm-540
           else      go to exe-rca-pcm-560.
       exe-rca-pcm-520.
      *              *-------------------------------------------------*
      *              * Se zero elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Non e' stata trovata alcuna riga ordine con il cod
      -              "ice prodotto   "    to   w-err-box-err-msg      .
           move      "impostato !                                       
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     exe-rca-pcm-100.
       exe-rca-pcm-540.
      *              *-------------------------------------------------*
      *              * Se 1 elemento bufferizzato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore in uscita per numero riga            *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-bnr (1)
                                          to   w-exe-rca-pcm-nrg      .
      *                  *---------------------------------------------*
      *                  * A uscita                                    *
      *                  *---------------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-560.
      *              *-------------------------------------------------*
      *              * Se piu' elementi bufferizzati                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 12 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl1      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 14 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl2      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione linea 15 attuale             *
      *                  *---------------------------------------------*
           move      "GL"                 to   v-ope                  .
           move      15                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      v-alf                to   w-exe-rca-pcm-sl3      .
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
           move      01                   to   v-pos                  .
           move      21                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione titolo                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl1    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione tipo prodotto             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl2    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Rivisualizzazione codice prodotto           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-exe-rca-pcm-sl3    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fincatura                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "N.R Da Evadere Cons.Rich. Cons.Prev. | N.R Da Evad
      -              "ere Cons.Rich. Cons.Prev."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "--- ---------- ---------- ---------- | --- -------
      -              "--- ---------- ----------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Barrette di divisione                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      75                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                                     |            
      -              "                         "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      17                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      18                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      19                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      20                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-562.
      *                  *---------------------------------------------*
      *                  * Ciclo di visualizzazione elementi bufferiz- *
      *                  * zati                                        *
      *                  *---------------------------------------------*
           move      zero                 to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-564.
           add       1                    to   w-exe-rca-pcm-ctr      .
           if        w-exe-rca-pcm-ctr    >    w-exe-rca-pcm-ele
                     go to exe-rca-pcm-566.
      *                      *-----------------------------------------*
      *                      * Determinazione indice elemento nel buf- *
      *                      * fer righe ordine                        *
      *                      *-----------------------------------------*
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   w-exe-rca-pcm-inx      .
      *                      *-----------------------------------------*
      *                      * Numero riga                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      spaces               to   v-edm                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  42             to   v-pos
           else      move  03             to   v-pos                  .
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' da evadere                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           if        w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   >    99999 or
                     w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   <   -99999
                     move  06             to   v-car
           else      move  05             to   v-car                  .
           move      w-bro-dec-qta
                    (w-exe-rca-pcm-inx)   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   >    99999 or
                     w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   <   -99999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  45             to   v-pos
           else      move  06             to   v-pos                  .
           move      w-bro-qta-dev
                    (w-exe-rca-pcm-inx)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data consegna richiesta                 *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Formato di stampa consegna richiesta    *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Data consegna prevista                  *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di conferma                        *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     exe-rca-pcm-564.
       exe-rca-pcm-566.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ciclo di selezione di una delle righe buf-  *
      *                  * ferizzate                                   *
      *                  *---------------------------------------------*
           move      1                    to   w-exe-rca-pcm-ctr      .
       exe-rca-pcm-568.
      *                      *-----------------------------------------*
      *                      * Accettazione function key               *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      w-exe-rca-pcm-ctr    to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     subtract 5           from v-lin                  .
           add       15                   to   v-lin                  .
           if        w-exe-rca-pcm-ctr    >    5
                     move  57             to   v-pos
           else      move  18             to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "RTRN"               to   v-pfk (19)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-rca-pcm-570.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione della function   *
      *                      * key impostata                           *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to exe-rca-pcm-572
           else if   v-key                =    "SLCT" or
                     v-key                =    spaces
                     go to exe-rca-pcm-574
           else if   v-key                =    "DOWN"
                     go to exe-rca-pcm-576
           else if   v-key                =    "UP  "
                     go to exe-rca-pcm-578.
       exe-rca-pcm-572.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A uscita                            *
      *                          *-------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-574.
      *                      *-----------------------------------------*
      *                      * Se Select o Return                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valore in uscita per numero riga    *
      *                          *-------------------------------------*
           move      w-exe-rca-pcm-bnr
                    (w-exe-rca-pcm-ctr)   to   w-exe-rca-pcm-nrg      .
      *                          *-------------------------------------*
      *                          * A uscita                            *
      *                          *-------------------------------------*
           go to     exe-rca-pcm-900.
       exe-rca-pcm-576.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ctr    <    w-exe-rca-pcm-ele
                     add 1                to   w-exe-rca-pcm-ctr      .
           go to     exe-rca-pcm-568.
       exe-rca-pcm-578.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        w-exe-rca-pcm-ctr    >    1
                     subtract 1           from w-exe-rca-pcm-ctr      .
           go to     exe-rca-pcm-568.
       exe-rca-pcm-900.
      *              *-------------------------------------------------*
      *              * Ripristino video precedentemente salvato        *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     exe-rca-pcm-999.
       exe-rca-pcm-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Ti- *
      *    * po codice di magazzino                                    *
      *    *-----------------------------------------------------------*
       acc-tip-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tip-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Co- *
      *    * dice di magazzino                                         *
      *    *-----------------------------------------------------------*
       acc-cod-mag-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-mag-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo magazzino : Prodotto di vendita        *
      *                  *---------------------------------------------*
           perform   acc-cod-dcp-000      thru acc-cod-dcp-999        .
           go to     acc-cod-mag-400.
       acc-cod-mag-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-mag-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-mag-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Co- *
      *    * dice prodotto [dcp]                                       *
      *    *-----------------------------------------------------------*
       acc-cod-dcp-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-dcp-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-exe-rca-pcm-cnm    to   w-cod-cod-dcp-num      .
           move      w-exe-rca-pcm-cam    to   w-cod-cod-dcp-alf      .
           move      15                   to   w-cod-cod-dcp-lin      .
           move      21                   to   w-cod-cod-dcp-pos      .
           move      15                   to   w-cod-cod-dcp-dln      .
           move      37                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-cod-dcp-110.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-cod-dcp-115.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-cod-dcp-120.
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-dcp-115.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-cod-dcp-110.
       acc-cod-dcp-120.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-cod-dcp-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-dcp-999.
      *              *-------------------------------------------------*
      *              * Valore impostato                                *
      *              *-------------------------------------------------*
           move      v-alf                to   w-exe-rca-pcm-cam      .
           move      v-num                to   w-exe-rca-pcm-cnm      .
       acc-cod-dcp-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      w-exe-rca-pcm-cnm    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-dcp-flg    not  = spaces
                     go to acc-cod-dcp-100.
      *                  *---------------------------------------------*
      *                  * Valore a zero nom ammesso, a meno che non   *
      *                  * si sia in Up                                *
      *                  *---------------------------------------------*
           if        w-exe-rca-pcm-cnm    not  = zero
                     go to acc-cod-dcp-600.
           if        v-key                =    "UP  "
                     go to acc-cod-dcp-600
           else      go to acc-cod-dcp-100.
       acc-cod-dcp-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione prodotto                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione prodotto    *
      *                      * [dcp]                                   *
      *                      *-----------------------------------------*
           move      w-let-arc-dcp-des    to   w-exe-rca-pcm-dem      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione descrizione prodotto    *
      *                      *-----------------------------------------*
           perform   vis-des-mag-000      thru vis-des-mag-999        .
       acc-cod-dcp-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-dcp-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo per ricerca per codice magazzino :  *
      *    * Descrizione di magazzino                                  *
      *    *-----------------------------------------------------------*
       vis-des-mag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      w-exe-rca-pcm-dem    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-des-mag-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo per ricerca per codice magazzino : Ca- *
      *    * rattere di presa visione                                  *
      *    *-----------------------------------------------------------*
       acc-pre-vis-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-pre-vis-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      16                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "RTRN"               to   v-pfk (19)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pre-vis-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-pre-vis-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-pre-vis-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-pre-vis-999.
           exit.

      *    *===========================================================*
      *    * Caricamento righe in catena                               *
      *    *-----------------------------------------------------------*
       car-rig-cat-000.
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
      *              * Inizializzazione totale lordo righe ordine eva- *
      *              * se                                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-eoc-tot-lor          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer righe ordine          *
      *              *-------------------------------------------------*
           move      zero                 to   w-crc-wrk-ctr          .
       car-rig-cat-100.
           add       1                    to   w-crc-wrk-ctr          .
           if        w-crc-wrk-ctr        >    w-bro-num-ele
                     go to car-rig-cat-700.
       car-rig-cat-102.
      *                  *---------------------------------------------*
      *                  * Test se righe saldate con quantita' a zero  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se quantita' a zero                *
      *                      *-----------------------------------------*
           if        w-bro-qta-dsp
                    (w-crc-wrk-ctr)       not  = zero
                     go to car-rig-cat-104.
      *                      *-----------------------------------------*
      *                      * Test su flag di saldo                   *
      *                      *-----------------------------------------*
           if        w-bro-flg-fzs
                    (w-crc-wrk-ctr)       =    spaces
                     go to car-rig-cat-104.
      *                      *-----------------------------------------*
      *                      * Preparazione parametri per update       *
      *                      *-----------------------------------------*
           move      w-bro-flg-fzs
                    (w-crc-wrk-ctr)       to   w-rig-coc-fzs (1)      .
           move      w-gen-prt-orc        to   w-rig-coc-prt (1)      .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   w-rig-coc-prg (1)      .
      *                      *-----------------------------------------*
      *                      * Update riga ordine                      *
      *                      *-----------------------------------------*
           perform   wrt-rec-ocr-000      thru wrt-rec-ocr-999        .
      *                      *-----------------------------------------*
      *                      * A riga successiva                       *
      *                      *-----------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-104.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig
                    (w-crc-wrk-ctr)       =    "A" or
                     w-bro-tip-rig
                    (w-crc-wrk-ctr)       =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-bro-snx-aoc
                    (w-crc-wrk-ctr)       not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-bro-qta-dsp
                    (w-crc-wrk-ctr)       =    zero
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-150.
      *                  *---------------------------------------------*
      *                  * Append in catena                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Append non possibile : uscita con    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  car-rig-cat-900.
      *                      *-----------------------------------------*
      *                      * Append                                  *
      *                      *-----------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga                        *
      *                  *---------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ocr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ocr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-orc        to   rf-ocr-num-prt         .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       car-rig-cat-300.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in re-    *
      *                      * cord [ocr]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Valori da bufferizzare comunque     *
      *                          *-------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
           move      rf-ocr-bld-flb       to   w-rig-bld-flb (1)      .
           move      rf-ocr-bld-tpb       to   w-rig-bld-tpb (1)      .
           move      rf-ocr-bld-rgb       to   w-rig-bld-rgb (1)      .
           move      rf-ocr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
           move      rf-ocr-tip-mag       to   w-rig-tip-mag (1)      .
           move      rf-ocr-num-pro       to   w-rig-num-pro (1)      .
           move      rf-ocr-alf-pro       to   w-rig-alf-pro (1)      .
           move      rf-ocr-sgl-vrn       to   w-rig-sgl-vrn (1)      .
           move      rf-ocr-cop-scl       to   w-rig-cop-scl (1)      .
           move      rf-ocr-des-ext       to   w-rig-des-ext (1)      .
           move      rf-ocr-des-rig       to   w-rig-des-rig (1)      .
           move      rf-ocr-tip-pro       to   w-rig-tip-pro (1)      .
           move      rf-ocr-cod-iva       to   w-rig-coi-rig (1)      .
           move      rf-ocr-ctp-ven       to   w-rig-ctv-rig (1)      .
           move      rf-ocr-umi-ven       to   w-rig-umi-ven (1)      .
           move      rf-ocr-dec-qta       to   w-rig-dec-qta (1)      .
           move      rf-ocr-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-ocr-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-ocr-qta-a02       to   w-rig-qta-a02 (1)      .
           move      rf-ocr-flg-puq       to   w-rig-flg-puq (1)      .
           move      rf-ocr-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-ocr-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-ocr-qta-a03       to   w-rig-qta-a03 (1)      .
           move      rf-ocr-dec-prz       to   w-rig-dec-prz (1)      .
           move      rf-ocr-sgl-vps       to   w-rig-sgl-vps (1)      .
           move      rf-ocr-dec-vps       to   w-rig-dec-vps (1)      .
           move      rf-ocr-tdc-vps       to   w-rig-tdc-vps (1)      .
           move      rf-ocr-prz-lrs       to   w-rig-prz-lrs (1)      .
           move      rf-ocr-prz-nts       to   w-rig-prz-nts (1)      .
           move      rf-ocr-sgl-vpp       to   w-rig-sgl-vpp (1)      .
           move      rf-ocr-dec-vpp       to   w-rig-dec-vpp (1)      .
           move      rf-ocr-tdc-vpp       to   w-rig-tdc-vpp (1)      .
           move      rf-ocr-prz-ven       to   w-rig-prz-ven (1)      .
           move      rf-ocr-snx-2pz       to   w-rig-snx-2pz (1)      .
           move      rf-ocr-prz-a02       to   w-rig-prz-a02 (1)      .
           move      rf-ocr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-ocr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-ocr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-ocr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-ocr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-ocr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-ocr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-ocr-map-vpl       to   w-rig-map-vpl (1)      .
           move      rf-ocr-epz-rgf       to   w-rig-epz-rgf (1)      .
           move      rf-ocr-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-ocr-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-ocr-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-ocr-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-ocr-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-ocr-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
           move      rf-ocr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ocr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ocr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ocr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ocr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
           move      rf-ocr-prz-net       to   w-rig-prz-net (1)      .
           move      rf-ocr-epz-pes       to   w-rig-epz-pes (1)      .
           move      rf-ocr-sgl-vpc       to   w-rig-sgl-vpc (1)      .
           move      rf-ocr-dec-vpc       to   w-rig-dec-vpc (1)      .
           move      rf-ocr-tdc-vpc       to   w-rig-tdc-vpc (1)      .
           move      rf-ocr-cdc-vpc       to   w-rig-cdc-vpc (1)      .
           move      rf-ocr-dec-cos       to   w-rig-dec-cos (1)      .
           move      rf-ocr-cos-rif       to   w-rig-cos-rif (1)      .
           move      rf-ocr-iau-rig       to   w-rig-iau-rig (1)      .
           move      rf-ocr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-ocr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-ocr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-ocr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
           move      rf-ocr-fsp-rig       to   w-rig-fsp-rig (1)      .
           move      rf-ocr-cpv-rig       to   w-rig-cpv-rig (1)      .
           move      rf-ocr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-ocr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-ocr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
           move      rf-ocr-pvf-rig       to   w-rig-pvf-rig (1)      .
           move      rf-ocr-ocl-dat       to   w-rig-ocl-dat (1)      .
           move      rf-ocr-ocl-num       to   w-rig-ocl-num (1)      .
           move      rf-ocr-cmc-tip       to   w-rig-cmc-tip (1)      .
           move      rf-ocr-cmc-dat       to   w-rig-cmc-dat (1)      .
           move      rf-ocr-cmc-num       to   w-rig-cmc-num (1)      .
           move      rf-ocr-tmo-orc       to   w-rig-coc-tip (1)      .
           move      rf-ocr-dat-doc       to   w-rig-coc-dat (1)      .
           move      rf-ocr-num-doc       to   w-rig-coc-num (1)      .
           move      rf-ocr-num-prt       to   w-rig-coc-prt (1)      .
           move      rf-ocr-num-prg       to   w-rig-coc-prg (1)      .
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Valori contenuti o dedotti da buffer    *
      *                      * righe ordine                            *
      *                      *-----------------------------------------*
       car-rig-cat-420.
      *                          *-------------------------------------*
      *                          * Quantita' per la vendita            *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"
                     move  zero           to   w-rig-qta-ven (1)
           else      move  w-bro-qta-dsp
                          (w-crc-wrk-ctr) to   w-rig-qta-ven (1)      .
       car-rig-cat-425.
      *                          *-------------------------------------*
      *                          * Trattamento della seconda quantita' *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se trattamento necessario  *
      *                              *---------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-430.
           if        w-rig-qta-ven (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-430.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-rig-qta-ven (1)    to   w-det-qta-a02-qim      .
           move      w-rig-dec-2qt (1)    to   w-det-qta-a02-ndu      .
      *
           if        w-rig-flg-puq (1)    =    "D"
                     move  10             to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "C"
                     move  100            to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "K"
                     move  1000           to   w-det-qta-a02-cmu
           else      move  1              to   w-det-qta-a02-cmu      .
      *
           move      1                    to   w-det-qta-a02-cdu      .
           perform   det-qta-a02-000      thru det-qta-a02-999        .
           move      w-det-qta-a02-qts    to   w-rig-qta-a02 (1)      .
       car-rig-cat-430.
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio valuta per   *
      *                          * i prezzi standard alla data del do- *
      *                          * cumento                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-tes-cdc-vpf (1)    to   w-rig-cdc-vps (1)      .
       car-rig-cat-440.
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio valuta per   *
      *                          * il prezzo alla data del documento   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se sigla valuta per il prezzo   *
      *                              * pari a quella di fatturazione,  *
      *                              * bufferizzazione coefficiente di *
      *                              * cambio valuta per fatturazione  *
      *                              *---------------------------------*
           if        w-rig-sgl-vpp (1)    =    w-tes-sgl-vpf (1) and
                     w-rig-dec-vpp (1)    =    w-tes-dec-vpf (1) and
                     w-rig-tdc-vpp (1)    =    w-tes-tdc-vpf (1)
                     move  w-tes-cdc-vpf (1)
                                          to   w-rig-cdc-vpp (1)
                     go to car-rig-cat-450.
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio per la valuta per il     *
      *                              * prezzo alla data del documento  *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpp (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-tdc-vpp (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio per la valuta per il     *
      *                              * prezzo determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpp (1)      .
       car-rig-cat-450.
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio valuta per   *
      *                          * il legame valutario alla data del   *
      *                          * documento                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da effettuare           *
      *                              *---------------------------------*
           if        w-tes-mac-lvl (1)    =    00
                     go to car-rig-cat-480.
           if        w-rig-sgl-vpl (1)    =    spaces
                     go to car-rig-cat-480.
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio per il legame valutario  *
      *                              * alla data del documento         *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpl (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-tdc-vpl (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione nega-   *
      *                              * tivo : bufferizzazione coeffi-  *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpl (1)      .
       car-rig-cat-480.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-482
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-484
           else      go to car-rig-cat-486.
       car-rig-cat-482.
      *                              *---------------------------------*
      *                              * Se tipo riga : Commento         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-484.
      *                              *---------------------------------*
      *                              * Se tipo riga : Addebito         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore contenuto in record  *
      *                                  * [ocr]                       *
      *                                  *-----------------------------*
           move      rf-ocr-imp-rig       to   w-rig-imp-rig (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-486.
      *                              *---------------------------------*
      *                              * Se altro tipo riga              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-500.
      *                          *-------------------------------------*
      *                          * Flag di forzatura saldo             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-502
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-504
           else      go to car-rig-cat-506.
       car-rig-cat-502.
      *                              *---------------------------------*
      *                              * Se tipo riga : Commento         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore forzato a 'S'        *
      *                                  *-----------------------------*
           move      "S"                  to   w-rig-coc-fzs (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-504.
      *                              *---------------------------------*
      *                              * Se tipo riga : Addebito         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore dipendente dalla im- *
      *                                  * postazione saldaconto per   *
      *                                  * Si/No addebito o commento   *
      *                                  *-----------------------------*
           if        w-bro-snx-aoc
                    (w-crc-wrk-ctr)       =    "S"
                     move  "S"            to   w-rig-coc-fzs (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-506.
      *                              *---------------------------------*
      *                              * Se altro tipo riga              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore dipendente dalla im- *
      *                                  * postazione saldaconto       *
      *                                  *-----------------------------*
           move      w-bro-flg-fzs
                    (w-crc-wrk-ctr)       to   w-rig-coc-fzs (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-520.
      *                      *-----------------------------------------*
      *                      * Fine trattamento valori contenuti o de- *
      *                      * dotti da buffer righe ordine            *
      *                      *-----------------------------------------*
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in re-  *
      *                      * cord [ocr]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [ocx]  *
      *                              *---------------------------------*
           move      w-gen-prt-orc        to   w-let-arc-ocx-prt      .
           move      w-bro-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-ocx-prg      .
           move      11                   to   w-let-arc-ocx-trc      .
           perform   let-arc-ocx-000      thru let-arc-ocx-999        .
      *
           if        w-let-arc-ocx-flg    not  = spaces
                     go to car-rig-cat-605.
      *
           move      w-let-arc-ocx-des    to   w-rig-des-rig (1)      .
      *
           go to     car-rig-cat-640.
       car-rig-cat-605.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di estensione                   *
      *                              *---------------------------------*
           if        w-rig-des-ext (1)    =    0
                     go to car-rig-cat-610
           else if   w-rig-des-ext (1)    =    1
                     go to car-rig-cat-620
           else if   w-rig-des-ext (1)    =    2
                     go to car-rig-cat-630.
       car-rig-cat-610.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : nessuna *
      *                              * azione in quanto la descrizione *
      *                              * e' gia' stata bufferizzata      *
      *                              *---------------------------------*
           go to     car-rig-cat-640.
       car-rig-cat-620.
      *                              *---------------------------------*
      *                              * Se estensione nel file [ocx]    *
      *                              *---------------------------------*
           go to     car-rig-cat-640.
       car-rig-cat-630.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      w-tes-tip-arc (1)    to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-arc-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-640.
       car-rig-cat-640.
      *                          *-------------------------------------*
      *                          * Anagrafica titoli esenzione         *
      *                          *-------------------------------------*
           move      w-rig-coi-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-coi-rig-des (1)  .
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctv-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctv-rig-des (1)  .
       car-rig-cat-650.
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-660.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale lordo righe ordine e-  *
      *                  * vase                                        *
      *                  *---------------------------------------------*
           if       (w-rig-tip-rig-tpr (1)
                                          =    "P" ) and
                     w-rig-tip-pro (1)    =    01 or
                     w-rig-tip-pro (1)    =    02 or
                     w-rig-tip-pro (1)    =    03 or
                     w-rig-tip-pro (1)    =    09
                     add  w-rig-imp-rig (1)
                                          to   w-eoc-tot-lor
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M" or
                     w-rig-tip-rig-tpr (1)
                                          =    "S"
                     add  w-rig-imp-rig (1)
                                          to   w-eoc-tot-lor          .
       car-rig-cat-680.
      *              *-------------------------------------------------*
      *              * Riciclo su riga ordine successiva               *
      *              *-------------------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-700.
      *              *-------------------------------------------------*
      *              * Determinazione sconto in chiusura relativo alle *
      *              * righe ordine evase                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sconto in chiusura dell'intero ordine a  *
      *                  * zero : nessuna determinazione               *
      *                  *---------------------------------------------*
           if        w-orc-tot-scc        =    zero
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Se totale lordo dell'intero ordine a zero : *
      *                  * nessuna determinazione                      *
      *                  *---------------------------------------------*
           if        w-orc-tot-lor        =    zero
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Se totale lordo righe evase a zero : nes-   *
      *                  * suna determinazione                         *
      *                  *---------------------------------------------*
           if        w-eoc-tot-lor        =    zero
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Se totale lordo righe evase pari al totale  *
      *                  * lordo intero ordine : sconto in chiusura    *
      *                  * pari a quello dell'intero ordine            *
      *                  *---------------------------------------------*
           if        w-eoc-tot-lor        =    w-orc-tot-lor
                     add   w-orc-tot-scc  to   w-pie-tot-scc (1)
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  w-orc-tot-scc        by   w-eoc-tot-lor
                                        giving w-eoc-num-s18          .
           divide    w-orc-tot-lor        into w-eoc-num-s18
                                               rounded                .
      *                  *---------------------------------------------*
      *                  * Se importo calcolato maggiore dello sconto  *
      *                  * in chiusura dell'intero ordine : sconto in  *
      *                  * chiusura pari a quello dell'intero ordine   *
      *                  *---------------------------------------------*
           if        w-orc-tot-scc        >    zero      and
                     w-eoc-num-s18        >    w-orc-tot-scc
                     move  w-orc-tot-scc  to   w-eoc-num-s18
           else if   w-orc-tot-scc        <    zero      and
                     w-eoc-num-s18        <    w-orc-tot-scc
                     move  w-orc-tot-scc  to   w-eoc-num-s18          .
      *                  *---------------------------------------------*
      *                  * Aggiornamento sconto in chiusura            *
      *                  *---------------------------------------------*
           add       w-eoc-num-s18        to   w-pie-tot-scc (1)      .
       car-rig-cat-750.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
           move      "Numero righe documento oltre il massimo previsto !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       car-rig-cat-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/ods/prg/obj/pods3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      zero                 to   w-rig-bld-flb (1)      .
           move      zero                 to   w-rig-bld-tpb (1)      .
           move      zero                 to   w-rig-bld-rgb (1)      .
           move      spaces               to   w-rig-tip-rig (1)      .
           move      spaces               to   w-rig-tip-rig-tpr (1)  .
           move      spaces               to   w-rig-tip-rig-tfu (1)  .
           move      zero                 to   w-rig-tip-rig-cac (1)  .
           move      spaces               to   w-rig-tip-rig-ast (1)  .
           move      zero                 to   w-rig-tip-mag (1)      .
           move      zero                 to   w-rig-num-pro (1)      .
           move      spaces               to   w-rig-alf-pro (1)      .
           move      spaces               to   w-rig-sgl-vrn (1)      .
           move      spaces               to   w-rig-cop-scl (1)      .
           move      zero                 to   w-rig-des-ext (1)      .
           move      spaces               to   w-rig-des-por (1)      .
           move      spaces               to   w-rig-des-rig (1)      .
           move      zero                 to   w-rig-tip-pro (1)      .
           move      zero                 to   w-rig-coi-rig (1)      .
           move      spaces               to   w-rig-coi-rig-des (1)  .
           move      zero                 to   w-rig-ctv-rig (1)      .
           move      spaces               to   w-rig-ctv-rig-des (1)  .
           move      spaces               to   w-rig-umi-ven (1)      .
           move      zero                 to   w-rig-dec-qta (1)      .
           move      zero                 to   w-rig-qta-ven (1)      .
           move      zero                 to   w-rig-snx-2qt (1)      .
           move      zero                 to   w-rig-dec-2qt (1)      .
           move      zero                 to   w-rig-qta-a02 (1)      .
           move      zero                 to   w-rig-snx-3qt (1)      .
           move      zero                 to   w-rig-dec-3qt (1)      .
           move      zero                 to   w-rig-qta-a03 (1)      .
           move      zero                 to   w-rig-dec-prz (1)      .
           move      spaces               to   w-rig-sgl-vps (1)      .
           move      zero                 to   w-rig-dec-vps (1)      .
           move      spaces               to   w-rig-tdc-vps (1)      .
           move      zero                 to   w-rig-cdc-vps (1)      .
           move      zero                 to   w-rig-prz-lrs (1)      .
           move      zero                 to   w-rig-prz-nts (1)      .
           move      spaces               to   w-rig-sgl-vpp (1)      .
           move      zero                 to   w-rig-dec-vpp (1)      .
           move      spaces               to   w-rig-tdc-vpp (1)      .
           move      zero                 to   w-rig-cdc-vpp (1)      .
           move      zero                 to   w-rig-prz-ven (1)      .
           move      zero                 to   w-rig-snx-2pz (1)      .
           move      zero                 to   w-rig-prz-a02 (1)      .
           move      spaces               to   w-rig-sgl-vpl (1)      .
           move      zero                 to   w-rig-dec-vpl (1)      .
           move      spaces               to   w-rig-tdc-vpl (1)      .
           move      zero                 to   w-rig-prz-vpl (1)      .
           move      zero                 to   w-rig-cdc-vpl (1)      .
           move      zero                 to   w-rig-ccr-vpl (1)      .
           move      zero                 to   w-rig-plm-vpl (1)      .
           move      spaces               to   w-rig-tlm-vpl (1)      .
           move      spaces               to   w-rig-map-vpl (1)      .
           move      zero                 to   w-rig-epz-rgf (1)      .
           move      zero                 to   w-rig-csr-aap (1)      .
           move      zero                 to   w-rig-psr-aap (1, 1)   .
           move      zero                 to   w-rig-psr-aap (1, 2)   .
           move      zero                 to   w-rig-psr-aap (1, 3)   .
           move      zero                 to   w-rig-psr-aap (1, 4)   .
           move      zero                 to   w-rig-psr-aap (1, 5)   .
           move      zero                 to   w-rig-per-scr (1, 1)   .
           move      zero                 to   w-rig-per-scr (1, 2)   .
           move      zero                 to   w-rig-per-scr (1, 3)   .
           move      zero                 to   w-rig-per-scr (1, 4)   .
           move      zero                 to   w-rig-per-scr (1, 5)   .
           move      zero                 to   w-rig-prz-net (1)      .
           move      zero                 to   w-rig-epz-pes (1)      .
           move      spaces               to   w-rig-sgl-vpc (1)      .
           move      zero                 to   w-rig-dec-vpc (1)      .
           move      spaces               to   w-rig-tdc-vpc (1)      .
           move      zero                 to   w-rig-cdc-vpc (1)      .
           move      zero                 to   w-rig-dec-cos (1)      .
           move      zero                 to   w-rig-cos-rif (1)      .
           move      zero                 to   w-rig-imp-rig (1)      .
           move      zero                 to   w-rig-iau-rig (1)      .
           move      zero                 to   w-rig-cpv-aap (1)      .
           move      zero                 to   w-rig-ppv-aap (1, 1)   .
           move      zero                 to   w-rig-ppv-aap (1, 2)   .
           move      zero                 to   w-rig-ppv-aap (1, 3)   .
           move      zero                 to   w-rig-fsp-rig (1)      .
           move      zero                 to   w-rig-cpv-rig (1)      .
           move      zero                 to   w-rig-ppv-rig (1, 1)   .
           move      zero                 to   w-rig-ppv-rig (1, 2)   .
           move      zero                 to   w-rig-ppv-rig (1, 3)   .
           move      zero                 to   w-rig-pvf-rig (1)      .
           move      zero                 to   w-rig-ocl-dat (1)      .
           move      spaces               to   w-rig-ocl-num (1)      .
           move      spaces               to   w-rig-cmc-tip (1)      .
           move      zero                 to   w-rig-cmc-dat (1)      .
           move      zero                 to   w-rig-cmc-num (1)      .
           move      spaces               to   w-rig-coc-tip (1)      .
           move      zero                 to   w-rig-coc-dat (1)      .
           move      zero                 to   w-rig-coc-num (1)      .
           move      zero                 to   w-rig-coc-prt (1)      .
           move      zero                 to   w-rig-coc-prg (1)      .
           move      spaces               to   w-rig-coc-fzs (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-flg-puq (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ocr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Inserimento di un nuovo record [osr]              *
      *    *-----------------------------------------------------------*
       wrt-rec-ocr-000.
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
      *              * Test su valore del flag contenuto in w-rig      *
      *              *-------------------------------------------------*
           if        w-rig-coc-fzs (1)    not  = "S"
                     go to wrt-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ocr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-coc-prt (1)    to   rf-ocr-num-prt         .
           move      w-rig-coc-prg (1)    to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wrt-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           move      "#"                  to   rf-ocr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ocr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       wrt-rec-ocr-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ocr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Modifica di un record [osr]                       *
      *    *-----------------------------------------------------------*
       rew-rec-ocr-000.
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
      *              * Test su valore attuale e precedente del flag    *
      *              * contenuto in w-rig                              *
      *              *-------------------------------------------------*
           if        w-rig-coc-fzs (1)    =    w-rig-coc-fzs (2)
                     go to rew-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ocr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-coc-prt (1)    to   rf-ocr-num-prt         .
           move      w-rig-coc-prg (1)    to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rew-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           if        w-rig-coc-fzs (1)    =    spaces
                     move  spaces         to   rf-ocr-sdr-ccs
           else      move  "#"            to   rf-ocr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ocr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       rew-rec-ocr-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento record [ocr] per quanto riguarda il se-     *
      *    * gnale di riga ordine comunque considerata saldata in      *
      *    * fase di Cancellazione di un record [osr]                  *
      *    *-----------------------------------------------------------*
       del-rec-ocr-000.
      *              *-------------------------------------------------*
      *              * Test su valore precedente del flag contenuto    *
      *              * in w-rig                                        *
      *              *-------------------------------------------------*
           if        w-rig-coc-fzs (2)    not  = "S"
                     go to del-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Ottenimento record [ocr]                        *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-coc-prt (1)    to   rf-ocr-num-prt         .
           move      w-rig-coc-prg (1)    to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-rec-ocr-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           move      spaces               to   rf-ocr-sdr-ccs         .
      *              *-------------------------------------------------*
      *              * Aggiornamento record [ocr]                      *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Rilascio record [ocr]                           *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       del-rec-ocr-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-ocr-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to    w-dec-tip-rig-str     .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di w-rig relativa al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      spaces               to    w-rig-tip-rig (1)     .
           move      spaces               to    w-rig-tip-rig-tpr (1) .
           move      spaces               to    w-rig-tip-rig-tfu (1) .
           move      zero                 to    w-rig-tip-rig-cac (1) .
           move      spaces               to    w-rig-tip-rig-ast (1) .
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
           move      "#"                  to   w-rig-tip-rig-ast (1)  .
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
                                          to   w-rig-tip-rig-tpr (1)  .
      *              *-------------------------------------------------*
      *              * Test se esiste un tipo funzionamento            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo per tipi riga P o L                    *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "L"
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
                                          to   w-rig-tip-rig-tfu (1)  .
       dec-tip-rig-200.
      *              *-------------------------------------------------*
      *              * Ricerca di un codice addebito o commento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se tipo riga A o C                     *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "C"
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
                    (w-dec-tip-rig-c01)   =    w-rig-tip-rig-tpr (1)
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
           move      w-dec-tip-rig-cod    to   w-rig-tip-rig-cac (1)  .
       dec-tip-rig-300.
      *              *-------------------------------------------------*
      *              * Preparazione stringa tipo riga per accettazione *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione puntatore per string       *
      *                  *---------------------------------------------*
           move      01                   to   w-dec-tip-rig-pnt      .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-rig-tip-rig-tpr (1)
                                          to   w-rig-tip-rig (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spazi : oltre                      *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    spaces
                     go to dec-tip-rig-320.
      *                      *-----------------------------------------*
      *                      * String del tipo funzionamento           *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    w-rig-tip-rig-tfu (1)
                                delimited by   spaces
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-320.
      *                  *---------------------------------------------*
      *                  * Asterisco                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non presente : oltre                 *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-ast (1)
                                          =    spaces
                     go to dec-tip-rig-400.
      *                      *-----------------------------------------*
      *                      * String asterisco                        *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    "*"        delimited by   size
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-tip-rig-999.
       dec-tip-rig-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [oct]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-oct-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porc3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to  fnd-arc-oct-999.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
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
           move      w-acc-cod-dpz        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/orc/prg/obj/porc3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tmo-orc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-alf                to   w-fnd-arc-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-dat                to   w-fnd-arc-oct-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-num                to   w-fnd-arc-oct-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-oct-toc    to   w-acc-tmo-orc          .
           move      w-fnd-arc-oct-dat    to   w-acc-dat-doc          .
           move      w-fnd-arc-oct-num    to   w-acc-num-doc          .
       fnd-arc-oct-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-acc-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-acc-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-acc-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-acc-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
           move      w-let-arc-zoc-tip    to   w-acc-tmo-orc-tip      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc-vld    not  = 02
                     go to fnd-arc-oct-500.
           if        w-acc-cod-dpz        =    w-acc-tmo-orc-dpz
                     go to fnd-arc-oct-500.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-500.
      *                  *---------------------------------------------*
      *                  * Test su tipo ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "P"
                     go to fnd-arc-oct-520.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine pro-forma non gestibile !                  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-520.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "F"
                     go to fnd-arc-oct-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine 'forecast' non gestibile !                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-oct-900.
       fnd-arc-oct-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-oct-999.
       fnd-arc-oct-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-acc                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-oct-sel      .
       fnd-arc-oct-999.
           exit.

      *    *===========================================================*
      *    * Select archivio [oct] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-toc      .
           move      zero                 to   w-slc-num-oct-dat      .
           move      zero                 to   w-slc-num-oct-num      .
           move      zero                 to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-oct-saa      .
       slc-num-oct-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-saa    to   w-slc-num-oct-nsa      .
           move      w-slc-num-oct-dpz    to   w-slc-num-oct-ndp      .
       slc-num-oct-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-oct-saa    to   rf-oct-scl-ann         .
           move      w-slc-num-oct-dpz    to   rf-oct-cod-dpz         .
           move      w-slc-num-oct-sgl    to   rf-oct-sgl-num         .
           move      w-slc-num-oct-nds    to   rf-oct-num-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-800.
       slc-num-oct-200.
      *              *-------------------------------------------------*
      *              * Read-next su [oct]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-500.
       slc-num-oct-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oct-scl-ann       not  = w-slc-num-oct-saa or
                     rf-oct-cod-dpz       not  = w-slc-num-oct-dpz or
                     rf-oct-sgl-num       not  = w-slc-num-oct-sgl or
                     rf-oct-num-doc       not  = w-slc-num-oct-nds
                     go to slc-num-oct-500.
       slc-num-oct-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-crb    >    30
                     go to slc-num-oct-520.
       slc-num-oct-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   w-slc-num-oct-bpt
                                              (w-slc-num-oct-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-oct-200.
       slc-num-oct-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    =    zero
                     go to slc-num-oct-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    >    1
                     go to slc-num-oct-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [oct]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt (1)
                                          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-crb    to   w-slc-num-oct-cpb      .
           subtract  1                    from w-slc-num-oct-cpb      .
           divide    6                    into w-slc-num-oct-cpb      .
           add       1                    to   w-slc-num-oct-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-oct-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Selezionare l'ordine deside
      -              "rato                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   slc-num-oct-950      thru slc-num-oct-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-nli      .
       slc-num-oct-555.
           if        w-slc-num-oct-nli    >    6
                     subtract  6          from w-slc-num-oct-nli
                     go to slc-num-oct-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-oct-nli      .
       slc-num-oct-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-oct-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-oct-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-oct-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-oct-cpa    <    w-slc-num-oct-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-oct-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-oct-582
           else if   v-key                =    "UP  "
                     go to slc-num-oct-584
           else if   v-key                =    "DOWN"
                     go to slc-num-oct-586
           else if   v-key                =    "EXIT"
                     go to slc-num-oct-598
           else if   v-key                =    "NXSC"
                     go to slc-num-oct-592
           else if   v-key                =    "PRSC"
                     go to slc-num-oct-594
           else      go to slc-num-oct-575.
       slc-num-oct-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore selezionato           *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c01)   to   w-slc-num-oct-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [oct]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-prt    to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-c01      .
           if        w-slc-num-oct-nli    =    10
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     add   1              to   w-slc-num-oct-c01
                     go to slc-num-oct-588
           else      go to slc-num-oct-575.
       slc-num-oct-588.
           if        w-slc-num-oct-nli    =    15
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-590.
           perform   slc-num-oct-950      thru slc-num-oct-989        .
           go to     slc-num-oct-550.
       slc-num-oct-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-cpa      .
           go to     slc-num-oct-596.
       slc-num-oct-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-cpa      .
       slc-num-oct-596.
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-c01      .
           multiply  6                    by   w-slc-num-oct-c01      .
           subtract  5                    from w-slc-num-oct-c01      .
           go to     slc-num-oct-590.
       slc-num-oct-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-toc    to   w-acc-tmo-orc          .
           move      w-slc-num-oct-dat    to   w-acc-dat-doc          .
           move      w-slc-num-oct-num    to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orc        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to slc-num-oct-900.
       slc-num-oct-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-vld    to   w-acc-tmo-orc-vld      .
           move      w-let-arc-zoc-dpz    to   w-acc-tmo-orc-dpz      .
           move      w-let-arc-zoc-ord    to   w-acc-tmo-orc-ord      .
           move      w-let-arc-zoc-prd    to   w-acc-tmo-orc-prd      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
           move      w-let-arc-zoc-tip    to   w-acc-tmo-orc-tip      .
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-vld    not  = 02
                     go to slc-num-oct-840.
           if        w-acc-cod-dpz        =    w-acc-tmo-orc-dpz
                     go to slc-num-oct-840.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento incompatibile con il codice dipende
      -              "nza            "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-840.
      *                  *---------------------------------------------*
      *                  * Test su tipo ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "P"
                     go to slc-num-oct-850.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine pro-forma non gestibile !                  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-850.
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-acc-tmo-orc-tip    not  = "F"
                     go to slc-num-oct-860.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine 'forecast' non gestibile !                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-oct-900.
       slc-num-oct-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-acc                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-slc-num-oct-sel      .
       slc-num-oct-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-c02      .
           add       5                    to   w-slc-num-oct-c02      .
           divide    6                    into w-slc-num-oct-c02      .
           move      w-slc-num-oct-c02    to   w-slc-num-oct-cpa      .
           subtract  1                    from w-slc-num-oct-c02      .
           multiply  6                    by   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c02      .
           add       5
                     w-slc-num-oct-c02  giving w-slc-num-oct-c03      .
           move      w-slc-num-oct-c03    to   w-slc-num-oct-c04      .
           if        w-slc-num-oct-c03    >    w-slc-num-oct-crb
                     move  w-slc-num-oct-crb
                                          to   w-slc-num-oct-c03      .
           move      10                   to   w-slc-num-oct-c05      .
       slc-num-oct-951.
      *              *-------------------------------------------------*
      *              * Lettura record [oct] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c02)   to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
       slc-num-oct-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-oct-tmo-orc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-oct-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-oct-num-doc       to   w-acc-num-doc          .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-oct-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-oct-dpz-arc       =    spaces
                     go to slc-num-oct-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-oct-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-oct-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-oct-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           if        w-slc-num-oct-c02    not  > w-slc-num-oct-c03
                     go to slc-num-oct-951.
       slc-num-oct-970.
           if        w-slc-num-oct-c02    >    w-slc-num-oct-c04
                     go to slc-num-oct-980.
           if        w-slc-num-oct-crb    not  > 6
                     go to slc-num-oct-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           go to     slc-num-oct-970.
       slc-num-oct-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-lt1      .
           move      w-slc-num-oct-cpb    to   w-slc-num-oct-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-oct-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-989.
           exit.
       slc-num-oct-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zoc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zoc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zoc-cod    =    spaces
                     go to let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOC"             to   f-key                  .
           move      w-let-arc-zoc-cod    to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zoc-400.
       let-arc-zoc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zoc-des-toc       to   w-let-arc-zoc-des      .
           move      rf-zoc-vld-dpz       to   w-let-arc-zoc-vld      .
           move      rf-zoc-cod-dpz       to   w-let-arc-zoc-dpz      .
           move      rf-zoc-org-doc       to   w-let-arc-zoc-ord      .
           move      rf-zoc-prv-doc       to   w-let-arc-zoc-prd      .
           move      rf-zoc-sgl-num       to   w-let-arc-zoc-sgl      .
           move      rf-zoc-tip-ord       to   w-let-arc-zoc-tip      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zoc-999.
       let-arc-zoc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zoc-flg      .
           move      all   "."            to   w-let-arc-zoc-des      .
           go to     let-arc-zoc-600.
       let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-des      .
       let-arc-zoc-600.
           move      spaces               to   w-let-arc-zoc-des      .
           move      zero                 to   w-let-arc-zoc-vld      .
           move      zero                 to   w-let-arc-zoc-dpz      .
           move      zero                 to   w-let-arc-zoc-ord      .
           move      zero                 to   w-let-arc-zoc-prd      .
           move      spaces               to   w-let-arc-zoc-sgl      .
           move      spaces               to   w-let-arc-zoc-tip      .
       let-arc-zoc-999.
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
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
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
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-stc      .
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
           go to     let-arc-cli-520.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-520.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      zero                 to   w-let-arc-cli-stc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
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
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
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
           move      rf-dcc-cod-vlt       to   w-let-arc-dcc-vlt      .
           move      rf-dcc-cod-lng       to   w-let-arc-dcc-lng      .
           move      rf-dcc-tas-ivc       to   w-let-arc-dcc-tai      .
           move      rf-dcc-fco-blo       to   w-let-arc-dcc-blo      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-520.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-520.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      spaces               to   w-let-arc-dcc-vlt      .
           move      spaces               to   w-let-arc-dcc-lng      .
           move      zero                 to   w-let-arc-dcc-tai      .
           move      spaces               to   w-let-arc-dcc-blo      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
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
           go to     let-arc-zvf-999.
       let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-des      .
       let-arc-zvf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zls]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zcs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zcs-cod    =    zero
                     go to let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      w-let-arc-zcs-tip    to   rf-zcs-tip-csc         .
           move      w-let-arc-zcs-cod    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcs-400.
       let-arc-zcs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcs-des-csc       to   w-let-arc-zcs-des      .
           move      rf-zcs-per-sco (1)   to   w-let-arc-zcs-per (1)  .
           move      rf-zcs-per-sco (2)   to   w-let-arc-zcs-per (2)  .
           move      rf-zcs-per-sco (3)   to   w-let-arc-zcs-per (3)  .
           move      rf-zcs-per-sco (4)   to   w-let-arc-zcs-per (4)  .
           move      rf-zcs-per-sco (5)   to   w-let-arc-zcs-per (5)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcs-999.
       let-arc-zcs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcs-flg      .
           move      all   "."            to   w-let-arc-zcs-des      .
           go to     let-arc-zcs-520.
       let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-des      .
       let-arc-zcs-520.
           move      zero                 to   w-let-arc-zcs-per (1)  .
           move      zero                 to   w-let-arc-zcs-per (2)  .
           move      zero                 to   w-let-arc-zcs-per (3)  .
           move      zero                 to   w-let-arc-zcs-per (4)  .
           move      zero                 to   w-let-arc-zcs-per (5)  .
       let-arc-zcs-999.
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
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
           move      rf-age-sup-age       to   w-let-arc-age-spa      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-rag      .
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
       let-arc-age-600.
           move      zero                 to   w-let-arc-age-spa      .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfp]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-zfp-cod    =    zero
                     go to let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-let-arc-zfp-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfp-400.
       let-arc-zfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfp-des-fop       to   w-let-arc-zfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfp-999.
       let-arc-zfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfp-flg      .
           move      all   "."            to   w-let-arc-zfp-des      .
           go to     let-arc-zfp-999.
       let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-des      .
       let-arc-zfp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-999.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice banca a zero                     *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB    "         to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-999.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-999.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zin]                         *
      *    *-----------------------------------------------------------*
       let-arc-zin-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zin-cod    =    spaces
                     go to let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-let-arc-zin-cod    to   rf-zin-cod-spi         .
           move      w-let-arc-zin-tpg    to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zin-400.
       let-arc-zin-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zin-des-spi       to   w-let-arc-zin-des      .
           move      rf-zin-civ-spi       to   w-let-arc-zin-coi      .
           move      rf-zin-ccp-spi       to   w-let-arc-zin-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zin-999.
       let-arc-zin-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zin-flg      .
           move      all   "."            to   w-let-arc-zin-des      .
           go to     let-arc-zin-520.
       let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-des      .
       let-arc-zin-520.
           move      zero                 to   w-let-arc-zin-coi      .
           move      zero                 to   w-let-arc-zin-ccp      .
       let-arc-zin-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zbo]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbo-cod    =    spaces
                     go to let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-let-arc-zbo-cod    to   rf-zbo-cod-spb         .
           move      w-let-arc-zbo-tpg    to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbo-400.
       let-arc-zbo-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbo-des-spb       to   w-let-arc-zbo-des      .
           move      rf-zbo-civ-spb       to   w-let-arc-zbo-coi      .
           move      rf-zbo-ccp-spb       to   w-let-arc-zbo-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbo-999.
       let-arc-zbo-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbo-flg      .
           move      all   "."            to   w-let-arc-zbo-des      .
           go to     let-arc-zbo-520.
       let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-des      .
       let-arc-zbo-520.
           move      zero                 to   w-let-arc-zbo-coi      .
           move      zero                 to   w-let-arc-zbo-ccp      .
       let-arc-zbo-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [ocx]                         *
      *    *-----------------------------------------------------------*
       let-arc-ocx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ocx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-ocx-prt    =    zero   or
                     w-let-arc-ocx-prg    =    zero   or
                     w-let-arc-ocx-trc    =    zero
                     go to let-arc-ocx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-ocx-prt    to   rf-ocx-num-prt         .
           move      w-let-arc-ocx-prg    to   rf-ocx-num-prg         .
           move      w-let-arc-ocx-trc    to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ocx-400.
       let-arc-ocx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ocx-des-400       to   w-let-arc-ocx-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ocx-999.
       let-arc-ocx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ocx-flg      .
           move      spaces               to   w-let-arc-ocx-des      .
           move      all   "."            to   w-let-arc-ocx-drg (1)  .
           go to     let-arc-ocx-999.
       let-arc-ocx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ocx-des      .
       let-arc-ocx-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [vet]                         *
      *    *-----------------------------------------------------------*
       let-arc-vet-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-vet-cod    =    zero
                     go to let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET"             to   f-key                  .
           move      w-let-arc-vet-cod    to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-vet-400.
       let-arc-vet-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-vet-rag-soc       to   w-let-arc-vet-rag      .
           move      rf-vet-via-vet       to   w-let-arc-vet-via      .
           move      rf-vet-loc-vet       to   w-let-arc-vet-loc      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-vet-999.
       let-arc-vet-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-vet-flg      .
           move      all   "."            to   w-let-arc-vet-rag      .
           go to     let-arc-vet-600.
       let-arc-vet-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-vet-rag      .
       let-arc-vet-600.
           move      spaces               to   w-let-arc-vet-via      .
           move      spaces               to   w-let-arc-vet-loc      .
       let-arc-vet-999.
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
       let-arc-zac-999.
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
       let-arc-dcp-210.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione della descrizione       *
      *                      *-----------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Test se codice lingua diverso da "I  "  *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-lng    not  = "I  "
                     go to let-arc-dcp-350.
       let-arc-dcp-215.
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
           if        rf-dcp-des-pdx       =    0
                     go to let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-220.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-220.
       let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-350.
      *                  *---------------------------------------------*
      *                  * Se lingua diversa da "I  "                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in lingua                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-370.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      02                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      w-let-arc-dcp-lng    to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-370.
       let-arc-dcp-380.
      *                      *-----------------------------------------*
      *                      * Test in uscita                          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-des    =    spaces
                     move  rf-dcp-des-pro to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      all   "."            to   w-let-arc-dcp-drg (1)  .
           move      all   "."            to   w-let-arc-dcp-dui      .
           go to     let-arc-dcp-520.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-dui      .
       let-arc-dcp-520.
           move      zero                 to   w-let-arc-dcp-tpr      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-dat-doc        to   rf-oct-dat-doc         .
           move      w-acc-cod-dpz        to   rf-oct-cod-dpz         .
           move      w-acc-num-doc        to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [oct], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  = w-acc-dat-doc or
                     rf-oct-cod-dpz       not  = w-acc-cod-dpz or
                     rf-oct-num-doc       not  = w-acc-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo movimento letto   *
      *              *-------------------------------------------------*
           if        rf-oct-tmo-orc       =    w-acc-tmo-orc
                     go to det-doc-ges-300
           else      go to det-doc-ges-400.
       det-doc-ges-300.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto pari al tipo movimento  *
      *              * da cercare                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
       det-doc-ges-400.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto diverso dal tipo movi-  *
      *              * mento da cercare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sigla numerazione per il tipo movimento  *
      *                  * letto diversa da sigla numerazione del ti-  *
      *                  * po movimento cercato : si ignora il docu-   *
      *                  * mento e si passa ad esaminare il documento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        rf-oct-sgl-num       not  = w-acc-tmo-orc-sgl
                     go to det-doc-ges-100.
      *                  *---------------------------------------------*
      *                  * Se invece la sigla numerazione e' la stessa *
      *                  * si memorizza il codice tipo movimento letto *
      *                  * per il caso in cui la determinazione doves- *
      *                  * se dare esito pari a 'X'                    *
      *                  *---------------------------------------------*
           move      rf-oct-tmo-orc       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Quindi si ricicla per esaminare eventuali   *
      *                  * documenti successivi                        *
      *                  *---------------------------------------------*
           go to     det-doc-ges-100.
       det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Determinazioni finali per il caso in cui il do- *
      *              * cumento non sia stato sicuramente trovato       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * documento con la data cercata, il numero    *
      *                  * cercato, e la sigla numerazione cercata :   *
      *                  * si esce con 'X', e con il codice del tipo   *
      *                  * movimento trovato gia' preparato; altri-    *
      *                  * menti si esce per documento non trovato     *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-tmt    not  = spaces
                     move  "X"            to   w-det-doc-ges-snx
                     go to det-doc-ges-999
           else      go to det-doc-ges-900.
       det-doc-ges-900.
      *              *-------------------------------------------------*
      *              * Uscita per documento non trovato                *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-doc-ges-snx      .
       det-doc-ges-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * principale                                                *
      *    *-----------------------------------------------------------*
       det-vlt-dcc-000.
      *              *-------------------------------------------------*
      *              * Si/No gestione valuta per il prezzo ed eventua- *
      *              * le momento di applicazione cambio               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-010.
      *                  *---------------------------------------------*
      *                  * Se No gestione valuta per il prezzo : oltre *
      *                  *---------------------------------------------*
           if        w-prs-ges-vpp        =    00
                     go to det-vlt-dcc-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-mom-acv       =    01
                     go to det-vlt-dcc-002
           else if   rf-dcc-mom-acv       =    11
                     go to det-vlt-dcc-004
           else if   rf-dcc-mom-acv       =    21
                     go to det-vlt-dcc-006
           else      go to det-vlt-dcc-002.
       det-vlt-dcc-002.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : Standard           *
      *                  *---------------------------------------------*
           if        w-prs-ges-vpp        =    11
                     move  11             to   w-tes-mac-vpp (1)
           else if   w-prs-ges-vpp        =    21
                     move  21             to   w-tes-mac-vpp (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-004.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in odsla           *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-mac-vpp (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-006.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in fattura         *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-mac-vpp (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-010.
      *              *-------------------------------------------------*
      *              * Si/No gestione legame valutario ed eventuale    *
      *              * momento di applicazione cambio                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-015.
      *                  *---------------------------------------------*
      *                  * Se No gestione legame valutario : oltre     *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    00
                     go to det-vlt-dcc-015.
      *                  *---------------------------------------------*
      *                  * Se no rapporti con legame valutario con il  *
      *                  * cliente : oltre                             *
      *                  *---------------------------------------------*
           if        rf-dcc-snx-rlv        not  = "S"
                     go to det-vlt-dcc-015.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-mom-alv       =    01
                     go to det-vlt-dcc-011
           else if   rf-dcc-mom-alv       =    11
                     go to det-vlt-dcc-012
           else if   rf-dcc-mom-alv       =    21
                     go to det-vlt-dcc-013
           else      go to det-vlt-dcc-011.
       det-vlt-dcc-011.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : Standard           *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    11
                     move  11             to   w-tes-mac-lvl (1)
           else if   w-prs-ges-lvl        =    21
                     move  21             to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-015.
       det-vlt-dcc-012.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in odsla           *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-015.
       det-vlt-dcc-013.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in fattura         *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-015.
       det-vlt-dcc-015.
      *              *-------------------------------------------------*
      *              * Tipo esposizione prezzi e sconti in riga docu-  *
      *              * mento                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-020.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-epz-pes       =    01
                     go to det-vlt-dcc-016
           else if   rf-dcc-epz-pes       =    11
                     go to det-vlt-dcc-017
           else if   rf-dcc-epz-pes       =    21
                     go to det-vlt-dcc-018
           else      go to det-vlt-dcc-016.
       det-vlt-dcc-016.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione : Standard              *
      *                  *---------------------------------------------*
           if        w-prs-epz-pes        =    11
                     move  11             to   w-tes-epz-pes (1)
           else if   w-prs-epz-pes        =    21
                     move  21             to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-020.
       det-vlt-dcc-017.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione : Prezzo e sconti       *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-020.
       det-vlt-dcc-018.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione: Prezzo netto           *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-020.
       det-vlt-dcc-020.
      *              *-------------------------------------------------*
      *              * Fatturazione separata per il documento          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-025.
      *                  *---------------------------------------------*
      *                  * Se valore contenuto nel [dcc] pari al valo- *
      *                  * re 'Una fattura per ogni ordine' : si pone  *
      *                  * il valore a 'O'                             *
      *                  *---------------------------------------------*
           if        rf-dcc-rag-bft       =    11
                     move  "O"            to   w-tes-fat-sep (1)      .
       det-vlt-dcc-025.
      *              *-------------------------------------------------*
      *              * Indirizzo sede del cliente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione : rimane quanto e' stato determi- *
      *                  * nato per il Cliente Commerciale             *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"
                     go to det-vlt-dcc-030.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-rag-soc       to   w-tes-rag-sed (1)      .
           move      rf-dcc-via-dcc       to   w-tes-via-sed (1)      .
           move      rf-dcc-loc-dcc       to   w-tes-loc-sed (1)      .
       det-vlt-dcc-030.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-cli-den (1)  .
       det-vlt-dcc-035.
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-cli (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-cli-den (1)  .
       det-vlt-dcc-040.
      *              *-------------------------------------------------*
      *              * C/c appoggio del cliente                        *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-cli (1)      .
       det-vlt-dcc-050.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostra banca per bonifico  *
      *              * associati al cliente                            *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ban       to   w-tes-nsb-aac (1)      .
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nsb-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nsb-aac-des (1)  .
       det-vlt-dcc-060.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostro c/c postale per pa- *
      *              * gamento associati al cliente                    *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ccp       to   w-tes-ncp-aac (1)      .
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-ncp-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-ncp-aac-des (1)  .
       det-vlt-dcc-070.
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato al clien- *
      *              * te                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aac (1)    not  = zero
                     go to det-vlt-dcc-080.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aac (1)      .
       det-vlt-dcc-080.
      *              *-------------------------------------------------*
      *              * Trasporto a cura associato al cliente           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione : rimane quanto e' stato determi- *
      *                  * nato per il Cliente Commerciale             *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"
                     go to det-vlt-dcc-085.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           if        rf-dcc-dtt-acu       =    10 or
                     rf-dcc-dtt-acu       =    11 or
                     rf-dcc-dtt-acu       =    12 or
                     rf-dcc-dtt-acu       =    13 or
                     rf-dcc-dtt-acu       =    14
                     move  10             to   w-pie-tcu-aac (1)
           else if   rf-dcc-dtt-acu       =    20
                     move  20             to   w-pie-tcu-aac (1)
           else if   rf-dcc-dtt-acu       =    30 or
                     rf-dcc-dtt-acu       =    31 or
                     rf-dcc-dtt-acu       =    32 or
                     rf-dcc-dtt-acu       =    33 or
                     rf-dcc-dtt-acu       =    34
                     move  30             to   w-pie-tcu-aac (1)
           else      move  zero           to   w-pie-tcu-aac (1)      .
       det-vlt-dcc-085.
      *              *-------------------------------------------------*
      *              * Codice vettore associato al cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione : rimane quanto e' stato determi- *
      *                  * nato per il Cliente Commerciale             *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"
                     go to det-vlt-dcc-090.
      *                  *---------------------------------------------*
      *                  * Primo vettore                               *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vet       to   w-pie-vet-aac (1)      .
           move      w-pie-vet-aac (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vet-aac-des (1)  .
      *                  *---------------------------------------------*
      *                  * Secondo vettore                             *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vt2       to   w-pie-vt2-aac (1)      .
           move      w-pie-vt2-aac (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vt2-aac-des (1)  .
      *                  *---------------------------------------------*
      *                  * Terzo vettore                               *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vt3       to   w-pie-vt3-aac (1)      .
           move      w-pie-vt3-aac (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vt3-aac-des (1)  .
       det-vlt-dcc-090.
      *              *-------------------------------------------------*
      *              * Altri valori                                    *
      *              *-------------------------------------------------*
           move      rf-dcc-tip-esm       to   w-tes-tip-esm (1)      .
           move      rf-dcc-ggg-alt       to   w-tes-ggg-alt (1)      .
           move      rf-dcc-mmm-e01       to   w-tes-mmm-e01 (1)      .
           move      rf-dcc-mmm-e02       to   w-tes-mmm-e02 (1)      .
       det-vlt-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione funzionamento spese in fattura             *
      *    *-----------------------------------------------------------*
       det-fun-spe-000.
      *              *-------------------------------------------------*
      *              * Se tipo determinazione per cliente per fattura- *
      *              * zione e tipo fornitura tramite Leasing : rimane *
      *              * quanto e' stato determinato per il Cliente Com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           if        w-det-fun-spe-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella personalizzazioni *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-fun-spe-ctr      .
       det-fun-spe-100.
           add       1                    to   w-det-fun-spe-ctr      .
           if        w-det-fun-spe-ctr    >    w-prs-spe-fat-nst
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Preparazione indice per tabella                 *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-det-fun-spe-ctr)   to   w-det-fun-spe-inx      .
      *              *-------------------------------------------------*
      *              * Si/No addebito                                  *
      *              *-------------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    zero or
                     rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    01
                     move  0              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)
           else      move  1              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              * derivato dalle personalizzazioni                *
      *              *-------------------------------------------------*
           if        w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    01
                     go to det-fun-spe-110
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    02
                     go to det-fun-spe-120
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    03
                     go to det-fun-spe-130
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    04
                     go to det-fun-spe-140
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    05
                     go to det-fun-spe-150.
       det-fun-spe-110.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % fissa                    *
      *              *-------------------------------------------------*
           move      1                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-per
                    (w-det-fun-spe-ctr)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-120.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % variabile                *
      *              *-------------------------------------------------*
           move      2                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-per-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-130.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo fisso              *
      *              *-------------------------------------------------*
           move      3                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-imp
                    (w-det-fun-spe-ctr)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-140.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo variabile          *
      *              *-------------------------------------------------*
           move      4                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-imp-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-150.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : secondo [dcc]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo funziona-   *
      *                  * mento espresso in [dcc]                     *
      *                  *---------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    03
                     go to det-fun-spe-151
           else if   rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    04
                     go to det-fun-spe-152.
       det-fun-spe-151.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : a %  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : % variabile         *
      *                      *-----------------------------------------*
           go to     det-fun-spe-120.
       det-fun-spe-152.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : ad   *
      *                  * importo                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : importo variabile   *
      *                      *-----------------------------------------*
           go to     det-fun-spe-140.
       det-fun-spe-200.
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella personalizzazioni  *
      *              *-------------------------------------------------*
           go to     det-fun-spe-100.
       det-fun-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * relativo alla dipendenza del cliente                      *
      *    *-----------------------------------------------------------*
       det-vlt-dcd-000.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza a spazi : uscita           *
      *              *-------------------------------------------------*
           if        w-det-vlt-dcd-dpz    =    spaces
                     go to det-vlt-dcd-999.
       det-vlt-dcd-010.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-dpz-den (1)  .
       det-vlt-dcd-020.
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-dpz (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-dpz-den (1)  .
       det-vlt-dcd-030.
      *              *-------------------------------------------------*
      *              * C/c appoggio della dipendenza                   *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-dpz (1)      .
       det-vlt-dcd-040.
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato alla      *
      *              * dipendenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcd-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aad (1)    not  = zero
                     go to det-vlt-dcd-050.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aad (1)      .
       det-vlt-dcd-050.
      *              *-------------------------------------------------*
      *              * Trasporto a cura associato alla dipendnza del   *
      *              * cliente                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione : rimane quanto e' stato determi- *
      *                  * nato per il Cliente Commerciale             *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"
                     go to det-vlt-dcd-060.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           if        rf-dcc-dtt-acu       =    10 or
                     rf-dcc-dtt-acu       =    11 or
                     rf-dcc-dtt-acu       =    12 or
                     rf-dcc-dtt-acu       =    13 or
                     rf-dcc-dtt-acu       =    14
                     move  10             to   w-pie-tcu-aad (1)
           else if   rf-dcc-dtt-acu       =    20
                     move  20             to   w-pie-tcu-aad (1)
           else if   rf-dcc-dtt-acu       =    30 or
                     rf-dcc-dtt-acu       =    31 or
                     rf-dcc-dtt-acu       =    32 or
                     rf-dcc-dtt-acu       =    33 or
                     rf-dcc-dtt-acu       =    34
                     move  30             to   w-pie-tcu-aad (1)
           else      move  zero           to   w-pie-tcu-aad (1)      .
       det-vlt-dcd-060.
      *              *-------------------------------------------------*
      *              * Codice vettore associato alla dipendenza cli-   *
      *              * ente                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione : rimane quanto e' stato determi- *
      *                  * nato per il Cliente Commerciale             *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"
                     go to det-vlt-dcc-070.
      *                  *---------------------------------------------*
      *                  * Primo vettore                               *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vet       to   w-pie-vet-aad (1)      .
           move      w-pie-vet-aad (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vet-aad-des (1)  .
      *                  *---------------------------------------------*
      *                  * Secondo vettore                             *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vt2       to   w-pie-vt2-aad (1)      .
           move      w-pie-vt2-aad (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vt2-aad-des (1)  .
      *                  *---------------------------------------------*
      *                  * Terzo vettore                               *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-vt3       to   w-pie-vt3-aad (1)      .
           move      w-pie-vt3-aad (1)    to   w-let-arc-vet-cod      .
           perform   let-arc-vet-000      thru let-arc-vet-999        .
           move      w-let-arc-vet-rag    to   w-pie-vt3-aad-des (1)  .
       det-vlt-dcd-070.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-vlt-dcd-999.
       det-vlt-dcd-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori piede relativi al vettore           *
      *    *-----------------------------------------------------------*
       det-vlp-vet-000.
      *              *-------------------------------------------------*
      *              * Trasporto a cura                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore gia' assegnato : oltre            *
      *                  *---------------------------------------------*
           if        w-pie-tra-cur (1)    not  = zero
                     go to det-vlp-vet-100.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        w-tes-dpz-arc (1)    =    spaces
                     move  w-pie-tcu-aac (1)
                                          to   w-pie-tra-cur (1)
           else      move  w-pie-tcu-aad (1)
                                          to   w-pie-tra-cur (1)      .
       det-vlp-vet-100.
      *              *-------------------------------------------------*
      *              * Vettore                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se Traporto a cura non del Vettore : uscita *
      *                  *---------------------------------------------*
           if        w-pie-tra-cur (1)    not  = 30
                     go to det-vlp-vet-999.
      *                  *---------------------------------------------*
      *                  * Se vettore gia' assegnato : uscita          *
      *                  *---------------------------------------------*
           if        w-pie-cod-vet (1)    not  = zero or
                     w-pie-cod-vt2 (1)    not  = zero or
                     w-pie-cod-vt3 (1)    not  = zero
                     go to det-vlp-vet-999.
      *                  *---------------------------------------------*
      *                  * Se dipendenza esistente : si assumono quel- *
      *                  * li della dipendenza, purche' i relativi co- *
      *                  * dici vettore non siano zero                 *
      *                  *---------------------------------------------*
           if        w-tes-dpz-arc (1)    =    spaces
                     go to det-vlp-vet-500.
           if        w-pie-vet-aad (1)    =    zero and
                     w-pie-vt2-aad (1)    =    zero and
                     w-pie-vt3-aad (1)    =    zero
                     go to det-vlp-vet-500.
      *                  *---------------------------------------------*
      *                  * Primo vettore                               *
      *                  *---------------------------------------------*
           move      w-pie-vet-aad (1)    to   w-pie-cod-vet (1)      .
           move      w-pie-vet-aad-des (1)
                                          to   w-pie-cod-vet-des (1)  .
      *                  *---------------------------------------------*
      *                  * Secondo vettore                             *
      *                  *---------------------------------------------*
           move      w-pie-vt2-aad (1)    to   w-pie-cod-vt2 (1)      .
           move      w-pie-vt2-aad-des (1)
                                          to   w-pie-cod-vt2-des (1)  .
      *                  *---------------------------------------------*
      *                  * Terzo vettore                               *
      *                  *---------------------------------------------*
           move      w-pie-vt3-aad (1)    to   w-pie-cod-vt3 (1)      .
           move      w-pie-vt3-aad-des (1)
                                          to   w-pie-cod-vt3-des (1)  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-vlp-vet-999.
       det-vlp-vet-500.
      *              *-------------------------------------------------*
      *              * Se dipendenza non esistente si assumono i valo- *
      *              * ri relativi al [dcc] principale                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Primo vettore                               *
      *                  *---------------------------------------------*
           move      w-pie-vet-aac (1)    to   w-pie-cod-vet (1)      .
           move      w-pie-vet-aac-des (1)
                                          to   w-pie-cod-vet-des (1)  .
      *                  *---------------------------------------------*
      *                  * Secondo vettore                             *
      *                  *---------------------------------------------*
           move      w-pie-vt2-aac (1)    to   w-pie-cod-vt2 (1)      .
           move      w-pie-vt2-aac-des (1)
                                          to   w-pie-cod-vt2-des (1)  .
      *                  *---------------------------------------------*
      *                  * Terzo vettore                               *
      *                  *---------------------------------------------*
           move      w-pie-vt3-aac (1)    to   w-pie-cod-vt3 (1)      .
           move      w-pie-vt3-aac-des (1)
                                          to   w-pie-cod-vt3-des (1)  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-vlp-vet-999.
       det-vlp-vet-999.
           exit.

      *    *===========================================================*
      *    * Determinazione quantita' secondaria per la vendita        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Editing di una quantita' da incolonnare                   *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-rig-qta-ven (1)    to   d-imp-ven-qta-ven      .
           move      w-rig-snx-2qt (1)    to   d-imp-ven-snx-2qt      .
           move      w-rig-qta-a02 (1)    to   d-imp-ven-qta-a02      .
           move      w-rig-snx-3qt (1)    to   d-imp-ven-snx-3qt      .
           move      w-rig-qta-a03 (1)    to   d-imp-ven-qta-a03      .
           move      w-rig-prz-net (1)    to   d-imp-ven-prz-uni      .
           move      w-rig-prz-ven (1)    to   d-imp-ven-prz-unl      .
           move      w-rig-per-scr (1, 1) to   d-imp-ven-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-imp-ven-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-imp-ven-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-imp-ven-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-imp-ven-per-scr (5)  .
           move      w-rig-dec-prz (1)    to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   w-rig-imp-rig (1)      .
       det-imp-rig-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-rig-sgl-vpp (1)    =    w-tes-sgl-vpf (1)
                     go to det-imp-rig-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           move      w-rig-imp-rig (1)    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   w-rig-imp-rig (1)      .
       det-imp-rig-999.
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
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

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
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      17                   to   v-lin                  .
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
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      19                   to   v-lto                  .
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
           move      17                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
           move      18                   to   v-lin                  .
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
      *    * Ricerca su buffer documenti movimentati                   *
      *    *-----------------------------------------------------------*
       buf-doc-mvm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-doc-mvm-fds      .
           move      spaces               to   w-buf-doc-mvm-tds      .
           move      zero                 to   w-buf-doc-mvm-nds      .
           move      zero                 to   w-buf-doc-mvm-dds      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-doc-mvm-crb      .
       buf-doc-mvm-100.
      *              *-------------------------------------------------*
      *              * Start su [oct]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATSYS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-buf-doc-mvm-drc    to   rf-oct-ide-dat         .
           move      zero                 to   rf-oct-dat-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to  buf-doc-mvm-999.
       buf-doc-mvm-200.
      *              *-------------------------------------------------*
      *              * Lettura [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-doc-mvm-500.
       buf-doc-mvm-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oct-ide-dat       not  = w-buf-doc-mvm-drc
                     go to buf-doc-mvm-500.
       buf-doc-mvm-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [oct]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice utente                       *
      *                  *---------------------------------------------*
           if        rf-oct-ide-ute       not  = w-buf-doc-mvm-ute
                     go to buf-doc-mvm-200.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-oct-cod-dpz       not  = w-buf-doc-mvm-dpz
                     go to buf-doc-mvm-200.
       buf-doc-mvm-410.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-crb    >    w-buf-doc-mvm-max
                     go to buf-doc-mvm-500.
       buf-doc-mvm-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      rf-oct-tmo-orc       to   w-buf-doc-mvm-tmo
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      rf-oct-num-doc       to   w-buf-doc-mvm-num
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      rf-oct-dat-doc       to   w-buf-doc-mvm-dat
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura [cli]                           *
      *                      *-----------------------------------------*
           move      rf-oct-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-buf-doc-mvm-rsa
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-200.
       buf-doc-mvm-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-buf-doc-mvm-crb    =    zero
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to buf-doc-mvm-999.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-crb    to   w-buf-doc-mvm-cpb      .
           subtract  1                    from w-buf-doc-mvm-cpb      .
           divide    6                    into w-buf-doc-mvm-cpb      .
           add       1                    to   w-buf-doc-mvm-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-doc-mvm-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Editing data                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-drc    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione fincatura                  *
      *                      *-----------------------------------------*
           move      60                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "DOCUMENTI INSERITI O MODIFICATI"
                                          to   w-all-str-cat (1)      .
      *
           move      w-buf-doc-mvm-drc    to   s-dat                  .
           if        s-gio                =    01 or
                     s-gio                =    08 or
                     s-gio                =    11
                     move  "L'"           to   w-all-str-cat (2)
           else      move  "IL"           to   w-all-str-cat (2)      .
      *
           move      v-edt                to   w-all-str-cat (3)      .
           move      "DA ["               to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-buf-doc-mvm-ute    to   w-all-str-cat (2)      .
           move      "]"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-555.
           if        w-buf-doc-mvm-nli    >    6
                     subtract  6          from w-buf-doc-mvm-nli
                     go to buf-doc-mvm-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       06                   to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       buf-doc-mvm-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-doc-mvm-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-doc-mvm-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-doc-mvm-cpa    <    w-buf-doc-mvm-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-doc-mvm-nli    to   v-lin                  .
           move      11                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to buf-doc-mvm-582
           else if   v-key                =    "UP  "
                     go to buf-doc-mvm-584
           else if   v-key                =    "DOWN"
                     go to buf-doc-mvm-586
           else if   v-key                =    "EXIT"
                     go to buf-doc-mvm-598
           else if   v-key                =    "NXSC"
                     go to buf-doc-mvm-592
           else if   v-key                =    "PRSC"
                     go to buf-doc-mvm-594
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori selezionati           *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-tds      .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-nds      .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-dds      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-c01      .
           if        w-buf-doc-mvm-nli    =    07
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     add   1              to   w-buf-doc-mvm-c01
                     go to buf-doc-mvm-588
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-588.
           if        w-buf-doc-mvm-nli    =    12
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-590.
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
           go to     buf-doc-mvm-550.
       buf-doc-mvm-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-cpa      .
           go to     buf-doc-mvm-596.
       buf-doc-mvm-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-cpa      .
       buf-doc-mvm-596.
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-c01      .
           multiply  6                    by   w-buf-doc-mvm-c01      .
           subtract  5                    from w-buf-doc-mvm-c01      .
           go to     buf-doc-mvm-590.
       buf-doc-mvm-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-doc-mvm-fds      .
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-c02      .
           add       5                    to   w-buf-doc-mvm-c02      .
           divide    6                    into w-buf-doc-mvm-c02      .
           move      w-buf-doc-mvm-c02    to   w-buf-doc-mvm-cpa      .
           subtract  1                    from w-buf-doc-mvm-c02      .
           multiply  6                    by   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       5
                     w-buf-doc-mvm-c02  giving w-buf-doc-mvm-c03      .
           move      w-buf-doc-mvm-c03    to   w-buf-doc-mvm-c04      .
           if        w-buf-doc-mvm-c03    >    w-buf-doc-mvm-crb
                     move  w-buf-doc-mvm-crb
                                          to   w-buf-doc-mvm-c03      .
           move      07                   to   w-buf-doc-mvm-c05      .
       buf-doc-mvm-951.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c02)
                    (6 : 6)               to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c02)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale intestatario documento      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-doc-mvm-rsa
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           if        w-buf-doc-mvm-c02    not  > w-buf-doc-mvm-c03
                     go to buf-doc-mvm-951.
       buf-doc-mvm-952.
           if        w-buf-doc-mvm-c02    >    w-buf-doc-mvm-c04
                     go to buf-doc-mvm-955.
           if        w-buf-doc-mvm-crb    not  > 6
                     go to buf-doc-mvm-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           go to     buf-doc-mvm-952.
       buf-doc-mvm-955.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-lt1      .
           move      w-buf-doc-mvm-cpb    to   w-buf-doc-mvm-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-doc-mvm-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-959.
           exit.
       buf-doc-mvm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione ubicazione di magazzino    *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dprmubi0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

