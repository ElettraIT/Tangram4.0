       Identification Division.
       Program-Id.                                 page4000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    mat                 *
      *                                   Fase:    age400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/04/94    *
      *                       Ultima revisione:    NdK del 30/10/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Generazione automatica delle maturazioni di *
      *                    provvigione a fronte dei conteggi provvi-   *
      *                    gionali.                                    *
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
      *    * File Control [sqz]                                        *
      *    *-----------------------------------------------------------*
           select  sqz       assign to input-output         f-sqz-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-sqz-sts .

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
      *            * Subchiave 1 : Per agente                          *
      *            *---------------------------------------------------*
               10  srt-k01.
      *                *-----------------------------------------------*
      *                * Flag di codice diverso da zero ma non trovato *
      *                * in archivio agenti                            *
      *                *                                               *
      *                *   - 1 : Esistente                             *
      *                *   - 5 : Non esistente                         *
      *                *   - 9 : Codice a zero                         *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per nominativo                              *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k01-flg-eon    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Nominativo per l'agente                       *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per nominativo                              *
      *                *-----------------------------------------------*
                   15  srt-k01-ron-age    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Mnemonico per l'agente                        *
      *                *                                               *
      *                * Solo per tipo ordinamento agente :            *
      *                *                                               *
      *                * - Per mnemonico                               *
      *                *-----------------------------------------------*
                   15  srt-k01-mne-age    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Flag di codice a zero o diverso da zero       *
      *                *                                               *
      *                *   - 1 : Codice diverso da zero                *
      *                *   - 9 : Codice a zero                         *
      *                *                                               *
      *                * Solo per tipo ordinamento agenti :            *
      *                *                                               *
      *                * - Per codice                                  *
      *                *-----------------------------------------------*
                   15  srt-k01-flg-zod    pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Codice agente                                 *
      *                *                                               *
      *                * Per tutti i tipi ordinamento agente           *
      *                *-----------------------------------------------*
                   15  srt-k01-cod-age    pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Subchiave 2 : Per documento relativo al conteggio *
      *            *               provvigionale                       *
      *            *---------------------------------------------------*
               10  srt-k02.
      *                *-----------------------------------------------*
      *                * Data documento del conteggio                  *
      *                *-----------------------------------------------*
                   15  srt-k02-dat-doc    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero documento del conteggio                *
      *                *-----------------------------------------------*
                   15  srt-k02-num-doc    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Numero conteggio                              *
      *                *-----------------------------------------------*
                   15  srt-k02-num-ctg    pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Conteggio provvigionale                           *
      *            *---------------------------------------------------*
               10  srt-dat-gpc.
      *                *-----------------------------------------------*
      *                * Numero conteggio                              *
      *                *-----------------------------------------------*
                   15  srt-gpc-num-ctg    pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Numero protocollo movimento di fatture clien- *
      *                * ti.                                           *
      *                *                                               *
      *                * Solo se conteggio generato automaticamente    *
      *                * dalla procedura di fatturazione.              *
      *                *-----------------------------------------------*
                   15  srt-gpc-prt-fcl    pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Tipo di conteggio                             *
      *                *                                               *
      *                *  - 01 : Conteggio a fronte fattura            *
      *                *  - 02 : Conteggio a fronte nota di addebito   *
      *                *  - 03 : Conteggio a fronte nota di accredito  *
      *                *  - 11 : Conteggio a fronte fattura, ma in de- *
      *                *         trazione                              *
      *                *-----------------------------------------------*
                   15  srt-gpc-tip-ctg    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Tipo di vendita per l'agente                  *
      *                *                                               *
      *                *  - 01 : Vendita diretta                       *
      *                *  - 02 : Vendita indiretta                     *
      *                *-----------------------------------------------*
                   15  srt-gpc-tip-vpa    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Data documento del conteggio                  *
      *                *-----------------------------------------------*
                   15  srt-gpc-dat-doc    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero documento del conteggio                *
      *                *-----------------------------------------------*
                   15  srt-gpc-num-doc    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Codice agente                                 *
      *                *-----------------------------------------------*
                   15  srt-gpc-cod-age    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente di fatturazione                *
      *                *-----------------------------------------------*
                   15  srt-gpc-cod-cli    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza del cliente di fatturazione *
      *                *-----------------------------------------------*
                   15  srt-gpc-dpz-cli    pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Imponibile provvigionale                      *
      *                *-----------------------------------------------*
                   15  srt-gpc-ibl-pvg    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * % di provvigione                              *
      *                *-----------------------------------------------*
                   15  srt-gpc-per-pvg    pic  9(02)v9(01)            .
      *                *-----------------------------------------------*
      *                * Ammontare provvigione conteggiata sul docu-   *
      *                * mento                                         *
      *                *-----------------------------------------------*
                   15  srt-gpc-amm-pvg    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * Importo totale del documento cui si riferisce *
      *                * il conteggio                                  *
      *                *-----------------------------------------------*
                   15  srt-gpc-imp-doc    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * Importo totale di eventuali acconti gia' fat- *
      *                * turati assorbiti nel documento cui si rife-   *
      *                * risce il conteggio                            *
      *                *-----------------------------------------------*
                   15  srt-gpc-imp-agf    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * Annotazioni sul conteggio provvigionale       *
      *                *-----------------------------------------------*
                   15  srt-gpc-not-ctg    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Tipo di maturazione prevista per la provvi-   *
      *                * gione conteggiata                             *
      *                *                                               *
      *                *  - 01 : Maturazione immediata                 *
      *                *  - 02 : Maturazione su incassi a fronte do-   *
      *                *         cumento                               *
      *                *                                               *
      *                * In caso di conteggio a fronte di una nota di  *
      *                * accredito la maturazione e' sempre immediata  *
      *                *-----------------------------------------------*
                   15  srt-gpc-tip-mat    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Segnale di provvigione con maturazione bloc-  *
      *                * cata                                          *
      *                *                                               *
      *                *  - 01 : Maturazione della provvigione non     *
      *                *         bloccata                              *
      *                *  - 02 : Maturazione della provvigione bloc-   *
      *                *         cata                                  *
      *                *                                               *
      *                * In caso di conteggio a fronte di una nota di  *
      *                * accredito la maturazione non e' mai bloccata  *
      *                *-----------------------------------------------*
                   15  srt-gpc-mat-blo    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Data di maturazione minima per la provvigione *
      *                *                                               *
      *                * In caso di conteggio a fronte di una nota di  *
      *                * accredito la data minima di maturazione e'    *
      *                * sempre a zero                                 *
      *                *-----------------------------------------------*
                   15  srt-gpc-ddm-min    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice agente subordinato                     *
      *                *                                               *
      *                * Solo se provvigione di un super-agente        *
      *                *-----------------------------------------------*
                   15  srt-gpc-cod-ags    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice del cliente per la fatturazione        *
      *                *                                               *
      *                * Se a zero significa che coincide con il cli-  *
      *                * ente di vendita                               *
      *                *-----------------------------------------------*
                   15  srt-gpc-cod-plf    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza del cliente per la fattura- *
      *                * zione                                         *
      *                *                                               *
      *                * Se il codice del cliente per la fatturazzione *
      *                * e' a zero questo valore e' sicuramente a spa- *
      *                * ces                                           *
      *                *-----------------------------------------------*
                   15  srt-gpc-dpz-plf    pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *---------------------------------------------------*
               10  srt-dat-age.
      *                *-----------------------------------------------*
      *                * Codice agente, mnemonico                      *
      *                *-----------------------------------------------*
                   15  srt-age-mne-age    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Codice agente, nominativo                     *
      *                *-----------------------------------------------*
                   15  srt-age-nom-age    pic  x(20)                  .
      *                *-----------------------------------------------*
      *                * Codice agente, ragione sociale                *
      *                *-----------------------------------------------*
                   15  srt-age-rag-age    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  srt-dat-cli.
      *                *-----------------------------------------------*
      *                * Codice cliente, ragione sociale               *
      *                *-----------------------------------------------*
                   15  srt-cli-rag-cli    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, via                           *
      *                *-----------------------------------------------*
                   15  srt-cli-via-cli    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, localita'                     *
      *                *-----------------------------------------------*
                   15  srt-cli-loc-cli    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice agente subordinato                         *
      *            *---------------------------------------------------*
               10  srt-dat-ags.
      *                *-----------------------------------------------*
      *                * Codice agente subordinato, mnemonico          *
      *                *-----------------------------------------------*
                   15  srt-ags-mne-ags    pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Codice agente subordinato, nominativo         *
      *                *-----------------------------------------------*
                   15  srt-ags-nom-ags    pic  x(20)                  .
      *                *-----------------------------------------------*
      *                * Codice agente subordinato, ragione sociale    *
      *                *-----------------------------------------------*
                   15  srt-ags-rag-ags    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente per la fatturazione                *
      *            *---------------------------------------------------*
               10  srt-dat-plf.
      *                *-----------------------------------------------*
      *                * Codice cliente, ragione sociale               *
      *                *-----------------------------------------------*
                   15  srt-plf-rag-plf    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, via                           *
      *                *-----------------------------------------------*
                   15  srt-plf-via-plf    pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Codice cliente, localita'                     *
      *                *-----------------------------------------------*
                   15  srt-plf-loc-plf    pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Maturazione                                       *
      *            *---------------------------------------------------*
               10  srt-dat-gpm.
      *                *-----------------------------------------------*
      *                * Flag di uscita dalla routine di determinazio- *
      *                * ne della nuova maturazione                    *
      *                *                                               *
      *                * Vedere il commento per il parametro in uscita *
      *                * corrispondente della routine                  *
      *                *-----------------------------------------------*
                   15  srt-gpm-flg-det    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Si/No maturazione effettiva                   *
      *                *                                               *
      *                * - S : Si                                      *
      *                * - N : No                                      *
      *                *-----------------------------------------------*
                   15  srt-gpm-snx-mef    pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Numero maturazione                            *
      *                *                                               *
      *                * Se simulazione : a zero                       *
      *                *-----------------------------------------------*
                   15  srt-gpm-num-mtz    pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Data di riferimento per la maturazione        *
      *                *-----------------------------------------------*
                   15  srt-gpm-dat-mtz    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Tipo di maturazione effettuata                *
      *                *                                               *
      *                *  - 00 : Non significativa, in quanto non si   *
      *                *         tratta di una maturazione effettiva   *
      *                *  - 01 : Maturazione immediata di provvigione  *
      *                *  - 02 : Maturazione di provvigione a fronte   *
      *                *         incasso                               *
      *                *  - 51 : Storno di provvigione                 *
      *                *                                               *
      *                * In caso di conteggio a fronte di una nota di  *
      *                * accredito la maturazione e' sempre immediata  *
      *                *                                               *
      *                * Nota : Il tipo maturazione 51 non e' mai ge-  *
      *                *        nerato automaticamente, bensi' puo'    *
      *                *        solo essere impostato manualmente.     *
      *                *                                               *
      *                *        Non deve essere confuso con una matu-  *
      *                *        razione immediata e di tipo positivo a *
      *                *        fronte di una nota di accredito.       *
      *                *                                               *
      *                *        Infatti lo storno puo' riferirsi sia   *
      *                *        ad un conteggio positivo sia ad un     *
      *                *        conteggio negativo. In entrambi i ca-  *
      *                *        si non modifica l'importo maturato to- *
      *                *        tale per l'agente, bensi' serve solo   *
      *                *        per chiudere il conteggio.             *
      *                *-----------------------------------------------*
                   15  srt-gpm-tip-mtz    pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Importo incassato, che ha fatto scattare la   *
      *                * maturazione.                                  *
      *                *                                               *
      *                * Solo se tipo di maturazione effettuata pari   *
      *                * a 02                                          *
      *                *-----------------------------------------------*
                   15  srt-gpm-imp-inc    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * Importo di provvigione maturato               *
      *                *-----------------------------------------------*
                   15  srt-gpm-imp-mtz    pic s9(11)       sign is
                                                           trailing
                                                           separate
                                                           character  .
      *                *-----------------------------------------------*
      *                * Annotazioni sulla maturazione                 *
      *                *-----------------------------------------------*
                   15  srt-gpm-not-mtz    pic  x(40)                  .

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
                     "age"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "mat"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "age400"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page4000"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     " GENERAZIONE MATURAZIONI SU PROVVIGIONI "       .

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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

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
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
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
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
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
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
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
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [gpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpm"                          .
      *        *-------------------------------------------------------*
      *        * [gpc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfgpc"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [rsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsc"                          .
      *        *-------------------------------------------------------*
      *        * [rnnumgpm]                                            *
      *        *-------------------------------------------------------*
           copy      "pgm/age/num/rec/rnnumgpm"                       .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Tipo di esecuzione                                    *
      *        *                                                       *
      *        *  - 01 : Solo simulazione e stampa                     *
      *        *  - 02 : Solo generazione effettiva delle maturazioni  *
      *        *  - 03 : Generazione effettiva delle maturazioni e an- *
      *        *         che stampa                                    *
      *        *-------------------------------------------------------*
           05  rr-tip-exe                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente                                         *
      *        *  - Se codice zero : Tutti gli agenti                  *
      *        *-------------------------------------------------------*
           05  rr-cod-age                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, nominativo                             *
      *        *  - Se codice zero : 'Tutti gli agenti'                *
      *        *-------------------------------------------------------*
           05  rr-cod-age-nom             pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, ragione sociale                        *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-rag             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, via                                    *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-via             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice agente, localita'                              *
      *        *  - Se codice zero : spaces                            *
      *        *-------------------------------------------------------*
           05  rr-cod-age-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di ordinamento per agenti                        *
      *        *                                                       *
      *        *  - 01 : Ordinamento per nominativo agente             *
      *        *  - 02 : Ordinamento per codice agente                 *
      *        *  - 03 : Ordinamento per mnemonico agente              *
      *        *                                                       *
      *        * Non significativo se selezionato un solo agente       *
      *        *-------------------------------------------------------*
           05  rr-tor-age                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data minima                                           *
      *        *-------------------------------------------------------*
           05  rr-dat-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data massima dei documenti relativi ai conteggi prov- *
      *        * vigionali su cui eseguire le nuove maturazioni        *
      *        *-------------------------------------------------------*
           05  rr-dat-max                 pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni generali                      *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Data di maturazione                                   *
      *        *-------------------------------------------------------*
           05  w-prs-dat-mtz              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/age[cod-age]'             *
      *    *-----------------------------------------------------------*
       01  w-prs-age-age.
      *        *-------------------------------------------------------*
      *        * Tipo codice agente da esporre                         *
      *        *   - C : Il codice numerico                            *
      *        *   - M : Il codice mnemonico                           *
      *        *-------------------------------------------------------*
           05  w-prs-age-age-tco          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo nominativo da esporre per l'agente               *
      *        *   - N : Il nominativo di 20 caratteri                 *
      *        *   - R : La ragione sociale di 40 caratteri            *
      *        *-------------------------------------------------------*
           05  w-prs-age-age-ron          pic  x(01)                  .

      *    *===========================================================*
      *    * Work per personalizzazione 'pgm/age[cod-cli]'             *
      *    *-----------------------------------------------------------*
       01  w-prs-age-cli.
      *        *-------------------------------------------------------*
      *        * Tipo codice cliente da esporre                        *
      *        *   - C : Il codice numerico                            *
      *        *   - M : Il codice mnemonico                           *
      *        *   - N : Nessuno                                       *
      *        *-------------------------------------------------------*
           05  w-prs-age-cli-tco          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ragione sociale da esporre                       *
      *        *   - C : Completa di via e localita'                   *
      *        *   - R : Solo la ragione sociale                       *
      *        *-------------------------------------------------------*
           05  w-prs-age-cli-trs          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo di esecuzione                         *
      *        *-------------------------------------------------------*
           05  w-exp-tip-exe.
               10  w-exp-tip-exe-num      pic  9(02)       value 03   .
               10  w-exp-tip-exe-lun      pic  9(02)       value 50   .
               10  w-exp-tip-exe-tbl.
                   15  filler             pic  x(50) value
                  "Solo simulazione e stampa                         ".
                   15  filler             pic  x(50) value
                  "Solo generazione effettiva delle maturazioni      ".
                   15  filler             pic  x(50) value
                  "Generazione effettiva delle maturazioni e stampa  ".
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ordinamento agenti                    *
      *        *-------------------------------------------------------*
           05  w-exp-tor-age.
               10  w-exp-tor-age-num      pic  9(02)       value 03   .
               10  w-exp-tor-age-lun      pic  9(02)       value 50   .
               10  w-exp-tor-age-tbl.
                   15  filler             pic  x(50) value
                  "Ordinamento per nominativo agente                 ".
                   15  filler             pic  x(50) value
                  "Ordinamento per codice agente                     ".
                   15  filler             pic  x(50) value
                  "Ordinamento per mnemonico agente                  ".

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica agenti            *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)     value spaces .
               10  w-let-arc-age-cod      pic  9(07)     value zero   .
               10  w-let-arc-age-nom      pic  x(20)     value spaces .
               10  w-let-arc-age-rag      pic  x(40)     value spaces .
               10  w-let-arc-age-via      pic  x(40)     value spaces .
               10  w-let-arc-age-loc      pic  x(40)     value spaces .
               10  w-let-arc-age-mne      pic  x(10)     value spaces .
               10  w-let-arc-age-exc      pic  9(07)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica agenti subordi-   *
      *        * nati                                                  *
      *        *-------------------------------------------------------*
           05  w-let-arc-ags.
               10  w-let-arc-ags-flg      pic  x(01)     value spaces .
               10  w-let-arc-ags-cod      pic  9(07)     value zero   .
               10  w-let-arc-ags-nom      pic  x(20)     value spaces .
               10  w-let-arc-ags-rag      pic  x(40)     value spaces .
               10  w-let-arc-ags-via      pic  x(40)     value spaces .
               10  w-let-arc-ags-loc      pic  x(40)     value spaces .
               10  w-let-arc-ags-mne      pic  x(10)     value spaces .
               10  w-let-arc-ags-exc      pic  9(07)     value zero   .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica clienti di fattu- *
      *        * razione                                               *
      *        *-------------------------------------------------------*
           05  w-let-cli-dcc.
               10  w-let-cli-dcc-flg      pic  x(01)     value spaces .
               10  w-let-cli-dcc-cli      pic  9(07)     value zero   .
               10  w-let-cli-dcc-dpz      pic  x(04)     value spaces .
               10  w-let-cli-dcc-rag      pic  x(40)     value spaces .
               10  w-let-cli-dcc-via      pic  x(40)     value spaces .
               10  w-let-cli-dcc-loc      pic  x(40)     value spaces .
               10  w-let-cli-dcc-exc      pic  9(07)     value zero   .
               10  w-let-cli-dcc-exd      pic  x(04)     value spaces .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio anagrafica clienti per la    *
      *        * fatturazione                                          *
      *        *-------------------------------------------------------*
           05  w-let-plf-dcc.
               10  w-let-plf-dcc-flg      pic  x(01)     value spaces .
               10  w-let-plf-dcc-cli      pic  9(07)     value zero   .
               10  w-let-plf-dcc-dpz      pic  x(04)     value spaces .
               10  w-let-plf-dcc-rag      pic  x(40)     value spaces .
               10  w-let-plf-dcc-via      pic  x(40)     value spaces .
               10  w-let-plf-dcc-loc      pic  x(40)     value spaces .
               10  w-let-plf-dcc-exc      pic  9(07)     value zero   .
               10  w-let-plf-dcc-exd      pic  x(04)     value spaces .

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

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 1 : Valori in input ed in output                    *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-001.
      *        *-------------------------------------------------------*
      *        * Input                                                 *
      *        *-------------------------------------------------------*
           05  w-det-nmp-inp.
      *            *---------------------------------------------------*
      *            * Si/No esecuzione lettura del record relativo al   *
      *            * conteggio di riferimento                          *
      *            *                                                   *
      *            * - S : Si, lettura da eseguire                     *
      *            * - N : No, lettura da non eseguire in quanto il    *
      *            *           record e' gia' presente in 'rf-gpc'     *
      *            *---------------------------------------------------*
               10  w-det-nmp-let-ctg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero conteggio di riferimento                   *
      *            *---------------------------------------------------*
               10  w-det-nmp-num-ctg      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Data di maturazione di riferimento                *
      *            *---------------------------------------------------*
               10  w-det-nmp-dat-mtz      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Output                                                *
      *        *-------------------------------------------------------*
           05  w-det-nmp-out.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalla routine                      *
      *            *                                                   *
      *            * - 01 : Maturazione eseguita, di tipo immediato    *
      *            * - 02 : Maturazione eseguita, a fronte incasso     *
      *            * - 11 : Determinazione eseguita senza anomalie, ma *
      *            *        importo nuova provvigione a zero in quanto *
      *            *        le maturazioni pregresse coprono gia' com- *
      *            *        pletamente l'importo del conteggio provvi- *
      *            *        gionale                                    *
      *            * - 12 : Determinazione eseguita senza anomalie, ma *
      *            *        importo nuova provvigione a zero in quanto *
      *            *        non esistono nuovi incassi che fanno matu- *
      *            *        rare provvigione a fronte incassi          *
      *            * - 21 : Determinazione eseguita senza anomalie, ma *
      *            *        importo nuova provvigione a zero in quanto *
      *            *        il conteggio provvigionale risulta attual- *
      *            *        mente bloccato                             *
      *            * - 22 : Determinazione eseguita senza anomalie, ma *
      *            *        importo nuova provvigione a zero in quanto *
      *            *        il conteggio provvigionale indica una data *
      *            *        di maturazione minima superiore alla data  *
      *            *        di riferimento per la maturazione          *
      *            * - 51 : Determinazione non eseguita in quanto non  *
      *            *        esiste piu' in archivio il record relati-  *
      *            *        vo al conteggio provvigionale              *
      *            *---------------------------------------------------*
               10  w-det-nmp-flg-det      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo di conteggio di riferimento                  *
      *            *                                                   *
      *            *  - 00 : Non significativo, in quanto conteggio    *
      *            *         non esistente in archivio                 *
      *            *  - 01 : Conteggio a fronte fattura                *
      *            *  - 02 : Conteggio a fronte nota di addebito       *
      *            *  - 03 : Conteggio a fronte nota di accredito      *
      *            *  - 11 : Conteggio a fronte fattura, ma in detra-  *
      *            *         zione                                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-tip-ctg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Importo incassato che fa' scattare la nuova matu- *
      *            * razione, solo in caso di maturazione a fronte in- *
      *            * casso                                             *
      *            *---------------------------------------------------*
               10  w-det-nmp-imp-inc      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo della nuova maturazione determinato       *
      *            *---------------------------------------------------*
               10  w-det-nmp-imp-mtz      pic s9(11)                  .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 2 : Area di comodo comune a tutte le subroutines    *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-002.
      *        *-------------------------------------------------------*
      *        * Area locale per le singole subroutines                *
      *        *-------------------------------------------------------*
           05  w-det-nmp-sub.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalle subroutines                  *
      *            * - 00 : Nessuna anomalia                           *
      *            * - nn : Flag indicatore dell'anomalia              *
      *            *---------------------------------------------------*
               10  w-det-nmp-sub-flg      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 3 : Area relativa al conteggio provvigionale        *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-003.
      *        *-------------------------------------------------------*
      *        * Area locale per la bufferizzazione di valori prove-   *
      *        * nienti dal record del conteggio provvigionale con-    *
      *        * tenuto in 'rf-gpc'                                    *
      *        *-------------------------------------------------------*
           05  w-det-nmp-gpc.
      *            *---------------------------------------------------*
      *            * Numero protocollo movimento di fatture clienti    *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-pfc      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Tipo di conteggio                                 *
      *            *                                                   *
      *            *  - 01 : Conteggio a fronte fattura                *
      *            *  - 02 : Conteggio a fronte nota di addebito       *
      *            *  - 03 : Conteggio a fronte nota di accredito      *
      *            *  - 11 : Conteggio a fronte fattura, ma in detra-  *
      *            *         zione                                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-tdc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data documento cui si riferisce il conteggio      *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-ddo      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento cui si riferisce il conteggio    *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-ndo      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-age      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del cliente                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-dcl      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Imponibile provvigionale                          *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-ibl      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * % di provvigione                                  *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-per      pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * Ammontare provvigione conteggiata sul documento   *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-pro      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale del documento cui si riferisce il  *
      *            * conteggio                                         *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-tdo      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale di eventuali acconti gia' fattura- *
      *            * ti assorbiti nel documento cui si riferisce il    *
      *            * conteggio                                         *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-taf      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Annotazioni sul conteggio provvigionale           *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-not      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Tipo di maturazione prevista per la provvigione   *
      *            *                                                   *
      *            * - 01 : Maturazione immediata                      *
      *            * - 02 : Maturazione su incassi a fronte documento  *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-tdm      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Segnale di provvigione con maturazione bloccata   *
      *            *                                                   *
      *            * - 01 : Maturazione della provvigione non bloccata *
      *            * - 02 : Maturazione della provvigione bloccata     *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-blo      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data di maturazione minima per la provvigione     *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-dmm      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice agente subordinato                         *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-ags      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente per la fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-ccf      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del cliente per la fatturazione *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpc-dcf      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 4 : Area relativa alle maturazioni preesistenti     *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-004.
      *        *-------------------------------------------------------*
      *        * Area locale per la bufferizzazione di valori prove-   *
      *        * nienti dai records di maturazione preesistenti con-   *
      *        * tenuti in 'rf-gpm'                                    *
      *        *-------------------------------------------------------*
           05  w-det-nmp-gpm.
      *            *---------------------------------------------------*
      *            * Numero records di maturazione preesistenti        *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpm-num      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Totale incassi preesistenti                       *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpm-ipr      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Totale maturazioni preesistenti                   *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpm-pre      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Totale maturazione residua da assegnare           *
      *            *---------------------------------------------------*
               10  w-det-nmp-gpm-res      pic s9(11)                  .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 5 : Area relativa alla nuova maturazione            *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-005.
      *        *-------------------------------------------------------*
      *        * Area locale per la determinazione effettiva della     *
      *        * nuova maturazione                                     *
      *        *-------------------------------------------------------*
           05  w-det-nmp-det.
      *            *---------------------------------------------------*
      *            * Totale documento presunto, pari al totale docu-   *
      *            * mento reale, aumentato degli eventuali acconti    *
      *            * gia' fatturati                                    *
      *            *---------------------------------------------------*
               10  w-det-nmp-det-tdp      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Incasso totale presunto, pari all'incasso totale  *
      *            * da analisi del portafoglio, aumentato degli even- *
      *            * tuali acconti gia' fatturati                      *
      *            *---------------------------------------------------*
               10  w-det-nmp-det-tip      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Totale provvigione maturata, calcolata proporzio- *
      *            * nalmente all'incassato                            *
      *            *---------------------------------------------------*
               10  w-det-nmp-det-tpm      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Nuova provvigione maturata, calcolata come diffe- *
      *            * renza tra l'importo totale maturato proporzional- *
      *            * mente agli incassi, e le maturazioni preesistenti *
      *            *---------------------------------------------------*
               10  w-det-nmp-det-npm      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Work per calcolo                                  *
      *            *---------------------------------------------------*
               10  w-det-nmp-det-w11      pic s9(11)                  .
               10  w-det-nmp-det-w18      pic s9(18)                  .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 6 : Area relativa all'analisi di portafoglio        *
      *    *                                                           *
      *    *           Sub-area 1 : Dati generali e buffer scadenze    *
      *    *                        originali emesse a fronte della    *
      *    *                        fattura                            *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-006.
      *        *-------------------------------------------------------*
      *        * Area locale per la determinazione del totale incassa- *
      *        * to reale per analisi del portafoglio                  *
      *        *-------------------------------------------------------*
           05  w-det-nmp-iap-001.
      *            *---------------------------------------------------*
      *            * Data di riferimento per l'analisi di portafoglio  *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ddr      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero protocollo movimento di fatture clienti    *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-pfc      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Totale della fattura cui devono fare riscontro le *
      *            * scadenze nel portafoglio crediti                  *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-tft      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Incasso totale determinato per analisi del porta- *
      *            * foglio crediti                                    *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-inc      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Incasso parziale determinato per analisi del por- *
      *            * tafoglio crediti relativo all'incasso presunto    *
      *            * su differenza tra totale fattura e scadenze emes- *
      *            * se in origine                                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ipd      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Incasso parziale determinato per analisi del por- *
      *            * tafoglio crediti relativo all'incasso reale       *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-irl      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Numero di scadenze emesse in origine a fronte del *
      *            * movimento di fatture clienti, memorizzate nel re- *
      *            * lativo buffer                                     *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-box      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Totale importi scadenze emesse in origine a fron- *
      *            * te del movimento di fatture clienti               *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-bot      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Indice per la scansione del buffer scadenze emes- *
      *            * se in origine a fronte del movimento di fatture   *
      *            * clienti                                           *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-boy      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Buffer scadenze emesse in origine a fronte del    *
      *            * movimento di fatture clienti                      *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-bob occurs 96.
      *                *-----------------------------------------------*
      *                * Numero scadenza originale                     *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-bon  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Numero riscossione, o pagamento, o compensa-  *
      *                * zione, cui la scadenza originale e' stata     *
      *                * sottoposta                                    *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-bop  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Importo scadenza originale, in valuta base    *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-boi  pic s9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Importo scadenza riscosso, in valuta base     *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-bor  pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 7 : Area relativa all'analisi di portafoglio        *
      *    *                                                           *
      *    *           Sub-area 2 : Sub-buffer per la determinazione   *
      *    *                        dell'importo riscosso per ogni     *
      *    *                        singola scadenza                   *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-007.
      *        *-------------------------------------------------------*
      *        * Area locale per la determinazione                     *
      *        *-------------------------------------------------------*
           05  w-det-nmp-iap-002.
      *            *---------------------------------------------------*
      *            * Numero di scadenze memorizzate nel sub-buffer per *
      *            * la determinazione del riscosso per la scadenza e- *
      *            * emessa in origine in esame                        *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-sbx      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Totale importo riscosso per la singola scadenza   *
      *            * emessa in origine in esame                        *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-sbt      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Indice per la scansione del sub-buffer per la de- *
      *            * terminazione del riscosso per la scadenza emessa  *
      *            * in origine in esame                               *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-sby      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Sub-buffer scadenze per la determinazione del ri- *
      *            * scosso per la scadenza emessa in origine in esame *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-sbb occurs 2000.
      *                *-----------------------------------------------*
      *                * Numero scadenza                               *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-sbn  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Numero riscossione, o pagamento, o compensa-  *
      *                * zione, cui la scadenza e' stata sottoposta    *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-sbp  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Flag di scadenza gia' trattata                *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-sbf  pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Importo scadenza, in valuta base              *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-sbi  pic s9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Coefficiente per la scadenza, che non puo'    *
      *                * mai assumere valori maggiori di 1,00000000    *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-sbc  pic  9(01)v9(07)            .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 8 : Area relativa all'analisi di portafoglio        *
      *    *                                                           *
      *    *           Sub-area 3 : Sub-sub-buffer per la memorizza-   *
      *    *                        zione delle scadenze riemesse a    *
      *    *                        fronte delle singole scadenze co-  *
      *    *                        involte in un'operazione           *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-008.
      *        *-------------------------------------------------------*
      *        * Area locale per la memorizzazione                     *
      *        *-------------------------------------------------------*
           05  w-det-nmp-iap-003.
      *            *---------------------------------------------------*
      *            * Numero di scadenze memorizzate nel sub-sub-buffer *
      *            * per la memorizzazione delle scadenze riemesse a   *
      *            * fronte di una operazione di riscossione, o di pa- *
      *            * gamento, o di compensazione                       *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ubx      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Indice per la scansione del sub-sub-buffer per la *
      *            * memorizzazione delle scadenze riemesse a fronte   *
      *            * di una operazione di riscossione, o di pagamento, *
      *            * o di compensazione                                *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-uby      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ubm      pic  9(05)       value 2000 .
      *            *---------------------------------------------------*
      *            * Scadenza di comodo                                *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ubw      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Sub-sub-buffer per la memorizzazione delle sca-   *
      *            * denze riemesse a fronte di una operazione di ri-  *
      *            * scossione, o di pagamento, o di compensazione     *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-ubb occurs 2000.
      *                *-----------------------------------------------*
      *                * Numero scadenza                               *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-ubn  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Numero riscossione, o pagamento, o compensa-  *
      *                * zione, cui la scadenza e' stata sottoposta    *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-ubp  pic  9(11)       comp-3     .
      *                *-----------------------------------------------*
      *                * Importo scadenza                              *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-ubi  pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work per determinazione nuove maturazioni di provvigione  *
      *    *                                                           *
      *    * Parte 9 : Area relativa all'analisi di portafoglio        *
      *    *                                                           *
      *    *           Sub-area 4 : Area di work comune                *
      *    *-----------------------------------------------------------*
       01  w-det-nmp-009.
      *        *-------------------------------------------------------*
      *        * Area di work                                          *
      *        *-------------------------------------------------------*
           05  w-det-nmp-iap-004.
      *            *---------------------------------------------------*
      *            * Work per calcoli intermedi                        *
      *            *---------------------------------------------------*
               10  w-det-nmp-iap-war.
      *                *-----------------------------------------------*
      *                * Per calcolo ammontare riscosso moltiplicato   *
      *                * per il coefficiente                           *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w01  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Per calcolo coefficiente per divisione tra    *
      *                * due importi                                   *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w02  pic  9(11)v9(07)            .
      *                *-----------------------------------------------*
      *                * Per memorizzazione importo di differenza tra  *
      *                * importo scadenza e importo scadenza riemessa  *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w03  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Per determinazione del totale degli importi   *
      *                * delle scadenze, di tipo positivo, coinvolte   *
      *                * in una operazione di riscossione, o di paga-  *
      *                * mento, o di compensazione                     *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w11  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Per determinazione dell'incidenza dell'impor- *
      *                * to della scadenza in esame nel sub-buffer sul *
      *                * totale degli importi delle scadenze, di tipo  *
      *                * positivo, coinvolte in una operazione di ri-  *
      *                * scossione, o di pagamento, o di compensazione *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w12  pic  9(01)v9(07)            .
      *                *-----------------------------------------------*
      *                * Per determinazione del totale degli importi   *
      *                * delle scadenze, di tipo positivo, riemesse a  *
      *                * fronte delle scadenze coinvolte in una opera- *
      *                * zione di riscossione, o di pagamento, o di    *
      *                * compensazione                                 *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w13  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Per determinazione della differenza tra il    *
      *                * totale degli importi delle scadenze positive  *
      *                * coinvolte in una operazione di riscossione, o *
      *                * di pagamento, o di compensazione, ed il tota- *
      *                * le degli importi delle scadenze positive rie- *
      *                * messe a fronte di tutte le scadenze coinvolte *
      *                * nell'operazione                               *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-w14  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Comodi di scansione file [rsc]                *
      *                *-----------------------------------------------*
                   15  w-det-nmp-iap-rc1  pic  9(05)                  .
                   15  w-det-nmp-iap-rs1  pic  9(11)                  .

      *    *===========================================================*
      *    * Work per la determinazione dell'ultima operazione esegui- *
      *    * ta su di una scadenza, contenuta in 'rf-sdb', con riferi- *
      *    * mento ad una certa data.                                  *
      *    *                                                           *
      *    * Senza analisi dettagliata sulle operazioni attinenti la   *
      *    * distinta di presentazione                                 *
      *    *-----------------------------------------------------------*
       01  w-sts-sdb.
      *        *-------------------------------------------------------*
      *        * Input                                                 *
      *        *-------------------------------------------------------*
           05  w-sts-sdb-inp.
      *            *---------------------------------------------------*
      *            * Tipo di determinazione                            *
      *            *                                                   *
      *            *  - 01 : Completa                                  *
      *            *  - 02 : Senza considerare le distinte di presen-  *
      *            *         tazione, ovvero equiparando una scadenza  *
      *            *         in circolazione ad una scadenza solo in   *
      *            *         stato di emissione                        *
      *            *---------------------------------------------------*
               10  w-sts-sdb-tip-det      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data di riferimento per la determinazione dello   *
      *            * status della scadenza                             *
      *            *---------------------------------------------------*
               10  w-sts-sdb-dat-rif      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Output                                                *
      *        *-------------------------------------------------------*
           05  w-sts-sdb-out.
      *            *---------------------------------------------------*
      *            * Ultima operazione eseguita sulla scadenza         *
      *            *                                                   *
      *            * - 000 : Nessuna operazione                        *
      *            *                                                   *
      *            *         La scadenza non risulta ancora emessa al- *
      *            *         la data di determinazione, o comunque lo  *
      *            *         status della scadenza non e' determinabi- *
      *            *         le.                                       *
      *            *                                                   *
      *            * - 100 : Emissione                                 *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata solo emessa, senza   *
      *            *         alcuna ulteriore operazione successiva    *
      *            *         a quella di emissione.                    *
      *            *                                                   *
      *            * - 200 : Storno                                    *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         stornata.                                 *
      *            *                                                   *
      *            * - 300 : Riscossione                               *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di riscossio-   *
      *            *         ne.                                       *
      *            *                                                   *
      *            * - 320 : Pagamento                                 *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di pagamento    *
      *            *         a clienti.                                *
      *            *                                                   *
      *            * - 350 : Compensazione                             *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di compensa-    *
      *            *         zione.                                    *
      *            *                                                   *
      *            * - 400 : Inclusione in distinta                    *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta.                              *
      *            *                                                   *
      *            * - 500 : Presentazione distinta                    *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta, successivamente presentata   *
      *            *         in banca.                                 *
      *            *                                                   *
      *            * - 540 : Accettazione distinta                     *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta, successivamente presentata   *
      *            *         in banca, ed accettata dalla stessa.      *
      *            *                                                   *
      *            * - 560 : Accredito distinta                        *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta, successivamente presentata   *
      *            *         in banca, ed accettata dalla stessa, ed   *
      *            *         infine accreditata in conto.              *
      *            *                                                   *
      *            * - 600 : Insoluto                                  *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta successivamente presentata.   *
      *            *         Dopo di cio' la scadenza e' ritornata     *
      *            *         insoluta.                                 *
      *            *                                                   *
      *            * - 620 : Richiamo                                  *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta successivamente presentata.   *
      *            *         Dopo di cio' la scadenza e' stata richia- *
      *            *         mata dalla presentazione.                 *
      *            *                                                   *
      *            * - 700 : Accredito al dopo incasso                 *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta al dopo incasso, successiva-  *
      *            *         mente presentata. Dopo di che la banca    *
      *            *         ha accreditato la scadenza in conto cor-  *
      *            *         rente.                                    *
      *            *                                                   *
      *            * - 720 : Notizia di buon esito                     *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta non di tipo al dopo incasso,  *
      *            *         successivamente presentata. Dopo di cio'  *
      *            *         la banca ha comunicato un sicuro buon e-  *
      *            *         sito per la scadenza.                     *
      *            *                                                   *
      *            * - 740 : Presunto buon esito                       *
      *            *                                                   *
      *            *         Alla data di determinazione la scadenza   *
      *            *         risulta essere stata prima emessa e poi   *
      *            *         inclusa in una operazione di composizio-  *
      *            *         ne distinta non di tipo al dopo incasso,  *
      *            *         successivamente presentata. Dopo di cio'  *
      *            *         si e' deciso di presumer un buon esito    *
      *            *         per la scadenza.                          *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-sts-sdb-ult-ope      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Data di rilevamento dello status della scadenza   *
      *            *---------------------------------------------------*
               10  w-sts-sdb-dat-ril      pic  9(07)                  .

      *    *===========================================================*
      *    * Work per subroutines di attribuzione numero maturazione   *
      *    * per la gestione provvigioni agenti                        *
      *    *-----------------------------------------------------------*
       01  w-num-gpm.
           05  w-num-gpm-num-gpm          pic  9(09)                  .
           05  w-num-gpm-val-pre          pic  9(09)                  .
           05  w-num-gpm-val-pos          pic  9(09)                  .

      *    *===========================================================*
      *    * Work per generazione file sequenziale di appoggio e per   *
      *    * generazione delle maturazioni definitive                  *
      *    *-----------------------------------------------------------*
       01  w-sqz-gpm.
      *        *-------------------------------------------------------*
      *        * Numero di records scritti su [sqz]                    *
      *        *-------------------------------------------------------*
           05  w-sqz-gpm-ctr-sqz          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero di records scritti su [gpm], o che sarebbero   *
      *        * stati scritti in caso di esecuzione definitiva        *
      *        *-------------------------------------------------------*
           05  w-sqz-gpm-ctr-gpm          pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *---------------------------------------------------*
               10  w-rot-l01-cod-age      pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per livelli di rottura                          *
      *    *-----------------------------------------------------------*
       01  w-liv.
      *        *-------------------------------------------------------*
      *        * Agente                                                *
      *        *-------------------------------------------------------*
           05  w-liv-age.
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *---------------------------------------------------*
               10  w-liv-age-cod-age      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Nominativo agente                                 *
      *            *---------------------------------------------------*
               10  w-liv-age-nom-age      pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale agente                            *
      *            *---------------------------------------------------*
               10  w-liv-age-rag-age      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Mnemonico agente                                  *
      *            *---------------------------------------------------*
               10  w-liv-age-mne-age      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Totale importo maturato                           *
      *            *---------------------------------------------------*
               10  w-liv-age-tot-mat      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Numero maturazioni trattate per l'agente          *
      *            *---------------------------------------------------*
               10  w-liv-age-num-mat      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Generale                                              *
      *        *-------------------------------------------------------*
           05  w-liv-gen.
      *            *---------------------------------------------------*
      *            * Totale importo maturato                           *
      *            *---------------------------------------------------*
               10  w-liv-gen-tot-mat      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Numero agenti trattati in totale                  *
      *            *---------------------------------------------------*
               10  w-liv-gen-num-age      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Flag di fine ciclo in esecuzione                  *
      *            *   - Spaces : No                                   *
      *            *   - #      : Si                                   *
      *            *---------------------------------------------------*
               10  w-liv-gen-flg-end      pic  x(01)                  .

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
      *            * Titolo per lo stampato allineato a sinistra       *
      *            *---------------------------------------------------*
               10  w-stp-int-tit-stp      pic  x(80)                  .
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
      *            * Sub-titolo per lo stampato allineato a sinistra   *
      *            *---------------------------------------------------*
               10  w-stp-int-sbt-stp      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza del sub-titolo per lo stampato, alli-   *
      *            * neato a sinistra, senza considerare gli spazi in  *
      *            * coda                                              *
      *            *---------------------------------------------------*
               10  w-stp-int-sbt-stl      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Colonna di stampa per il sub-titolo dello stampa- *
      *            * to                                                *
      *            *---------------------------------------------------*
               10  w-stp-int-sbt-stc      pic  9(03)                  .
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
               10  w-stp-int-wed-dt0      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per intestazione agente                      *
      *        *-------------------------------------------------------*
           05  w-stp-age.
      *            *---------------------------------------------------*
      *            * Numero totale di intestazioni agente stampate     *
      *            *---------------------------------------------------*
               10  w-stp-age-num-int      pic  9(05)       value zero .
      *            *---------------------------------------------------*
      *            * Area da stampare, centrata sulla pagina, allinea- *
      *            * ta a sinistra                                     *
      *            *---------------------------------------------------*
               10  w-stp-age-ads-asx      pic  x(72)                  .
      *            *---------------------------------------------------*
      *            * Area da stampare, centrata sulla pagina, allinea- *
      *            * ta al centro                                      *
      *            *---------------------------------------------------*
               10  w-stp-age-ads-sub      pic  x(72)                  .
      *            *---------------------------------------------------*
      *            * Lunghezza area da stampare                        *
      *            *---------------------------------------------------*
               10  w-stp-age-ads-lng      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Posizione di stampa iniziale, centrata sulla pa-  *
      *            * gina                                              *
      *            *---------------------------------------------------*
               10  w-stp-age-pos-dsi      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Literal per intestazione                          *
      *            *---------------------------------------------------*
               10  w-stp-age-lit-int      pic  x(30)                  .
      *            *---------------------------------------------------*
      *            * Tipo di intestazione                              *
      *            *   - C : Relativa ad un codice agente              *
      *            *   - T : Relativa al totale generale               *
      *            *---------------------------------------------------*
               10  w-stp-age-tip-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di codice per intestazione                   *
      *            *   - N : Numerico                                  *
      *            *   - A : Alfanumerico                              *
      *            *---------------------------------------------------*
               10  w-stp-age-tip-cod      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore codice numerico                            *
      *            *---------------------------------------------------*
               10  w-stp-age-cod-num      pic  9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore codice alfanumerico                        *
      *            *---------------------------------------------------*
               10  w-stp-age-cod-alf      pic  x(14)                  .
      *            *---------------------------------------------------*
      *            * Valore descrizione                                *
      *            *---------------------------------------------------*
               10  w-stp-age-cod-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Valore codice numerico editato                    *
      *            *---------------------------------------------------*
               10  w-stp-age-cod-ned      pic  x(13)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-age-wct-c01      pic  9(03)                  .
               10  w-stp-age-wct-c02      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per righe di dettaglio                       *
      *        *-------------------------------------------------------*
           05  w-stp-det.
      *            *---------------------------------------------------*
      *            * Area di stampa completa che compone il dettaglio  *
      *            *---------------------------------------------------*
               10  w-stp-det-ads.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono l'area di stampa *
      *                * di dettaglio                                  *
      *                *-----------------------------------------------*
                   15  w-stp-det-rig occurs 20.
      *                    *-------------------------------------------*
      *                    * Colonna 1                                 *
      *                    *-------------------------------------------*
                       20  w-stp-det-col-001
                                          pic  x(11)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 2                                 *
      *                    *-------------------------------------------*
                       20  w-stp-det-col-002
                                          pic  x(40)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 3                                 *
      *                    *-------------------------------------------*
                       20  w-stp-det-col-003
                                          pic  x(15)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 4                                 *
      *                    *-------------------------------------------*
                       20  w-stp-det-col-004
                                          pic  x(15)                  .
      *                    *-------------------------------------------*
      *                    * Colonna 5                                 *
      *                    *-------------------------------------------*
                       20  w-stp-det-col-005
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatori relativi alle colonne che funzionano a  *
      *            * riempimento a fisarmonica                         *
      *            *---------------------------------------------------*
               10  w-stp-det-cco.
      *                *-----------------------------------------------*
      *                * Per la colonna 1                              *
      *                *-----------------------------------------------*
                  15  w-stp-det-cco-001   pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Per la colonna 2                              *
      *                *-----------------------------------------------*
                  15  w-stp-det-cco-002   pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Per la colonna 5                              *
      *                *-----------------------------------------------*
                  15  w-stp-det-cco-005   pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Indici e contatori per la scansione e la stampa   *
      *            * delle singole righe di dettaglio                  *
      *            *---------------------------------------------------*
               10  w-stp-det-iec.
      *                *-----------------------------------------------*
      *                * Contatore delle righe effettive da stampare   *
      *                *-----------------------------------------------*
                   15  w-stp-det-ctr      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Indice per la scansione delle righe effettive *
      *                *-----------------------------------------------*
                   15  w-stp-det-inx      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per l'assestamento di valori relativi alla gestione  *
      *    * provvigioni ed agenti                                     *
      *    *-----------------------------------------------------------*
       01  w-ass-age.
      *        *-------------------------------------------------------*
      *        * Valori relativi al conteggio provvigionale            *
      *        *-------------------------------------------------------*
           05  w-ass-ctg.
      *            *---------------------------------------------------*
      *            * Tipo di conteggio                                 *
      *            *---------------------------------------------------*
               10  w-ass-ctg-tip-ctg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Imponibile provvigionale                          *
      *            *---------------------------------------------------*
               10  w-ass-ctg-ibl-pvg      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Ammontare provvigione conteggiata                 *
      *            *---------------------------------------------------*
               10  w-ass-ctg-amm-pvg      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale del documento cui si riferisce il  *
      *            * conteggio provvigionale                           *
      *            *---------------------------------------------------*
               10  w-ass-ctg-imp-doc      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale di eventuali acconti gia' fattura- *
      *            * ti assorbiti nel documento cui si riferisce il    *
      *            * conteggio provvigionale                           *
      *            *---------------------------------------------------*
               10  w-ass-ctg-imp-agf      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Segnale se importo conteggio influente o no sulle *
      *            * totalizzazioni di conteggi                        *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-ass-ctg-inf-som      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi alla maturazione                      *
      *        *-------------------------------------------------------*
           05  w-ass-mtz.
      *            *---------------------------------------------------*
      *            * Tipo di conteggio                                 *
      *            *---------------------------------------------------*
               10  w-ass-mtz-tip-ctg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo di maturazione                               *
      *            *---------------------------------------------------*
               10  w-ass-mtz-tip-mtz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Importo della maturazione                         *
      *            *---------------------------------------------------*
               10  w-ass-mtz-imp-mtz      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo incassato, che ha fatto scattare la matu- *
      *            * razione                                           *
      *            *---------------------------------------------------*
               10  w-ass-mtz-imp-inc      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Segnale se importo maturazione influente o no     *
      *            * sulle totalizzazioni di maturazione               *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-ass-mtz-inf-som      pic  x(01)                  .

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
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
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
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
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
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800
           else      go to main-250.
       main-800.
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
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa       *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-pgm-frg-900.
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
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
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
       pre-exe-pgm-010.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-100.
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
       pre-exe-pgm-100.
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
       pre-exe-pgm-200.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data maturazione                            *
      *                  *---------------------------------------------*
           perform   prs-dat-mtz-000      thru prs-dat-mtz-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento del codice agente nella gestio- *
      *                  * ne provvigioni ed agenti                    *
      *                  *---------------------------------------------*
           perform   prs-age-age-000      thru prs-age-age-999        .
      *                  *---------------------------------------------*
      *                  * Trattamento del codice cliente nella ges-   *
      *                  * tione provvigioni ed agenti                 *
      *                  *---------------------------------------------*
           perform   prs-age-cli-000      thru prs-age-cli-999        .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative alla data di     *
      *    * maturazione                                               *
      *    *-----------------------------------------------------------*
       prs-dat-mtz-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age/age400[dat-mtz]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dat-mtz
           else      move  spaces         to   w-prs-dat-mtz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-dat-mtz        not  = "S" and
                     w-prs-dat-mtz        not  = "X"
                     move  "M"            to   w-prs-dat-mtz          .
       prs-dat-mtz-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per il trattamento del    *
      *    * codice agente nella gestione provvigioni ed agenti        *
      *    *-----------------------------------------------------------*
       prs-age-age-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/age[cod-age]"
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
                     go to prs-age-age-150.
       prs-age-age-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-age-age-500.
       prs-age-age-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-age-age          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-age-700.
       prs-age-age-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[cod-age]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-age-age-650.
       prs-age-age-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-age-age          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-age-700.
       prs-age-age-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-age-age          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-age-700.
       prs-age-age-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo codice agente da esporre           'C' *
      *                  *---------------------------------------------*
           if        w-prs-age-age-tco    not  = "M"
                     move  "C"            to   w-prs-age-age-tco      .
      *                  *---------------------------------------------*
      *                  * Tipo nominativo da esporre              'N' *
      *                  *---------------------------------------------*
           if        w-prs-age-age-ron    not  = "R"
                     move  "N"            to   w-prs-age-age-ron      .
       prs-age-age-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-age-age-999.
       prs-age-age-999.
           exit.

      *    *===========================================================*
      *    * Lettura della personalizzazione per il trattamento del    *
      *    * codice cliente nella gestione provvigioni ed agenti       *
      *    *-----------------------------------------------------------*
       prs-age-cli-000.
      *              *-------------------------------------------------*
      *              * Personalizzazione per la specifica fase gestio- *
      *              * nale                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      spaces               to   s-alf                  .
           string    "pgm/age[cod-cli]"
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
                     go to prs-age-cli-150.
       prs-age-cli-100.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A lettura personalizzazione valida per  *
      *                      * tutte le fasi gestionali                *
      *                      *-----------------------------------------*
           go to     prs-age-cli-500.
       prs-age-cli-150.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-age-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-cli-700.
       prs-age-cli-500.
      *              *-------------------------------------------------*
      *              * Personalizzazione per tutte le fasi gestionali  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura della personalizzazione             *
      *                  *---------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/age[cod-cli]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della let-  *
      *                  * tura                                        *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-age-cli-650.
       prs-age-cli-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione non esistente          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione parametri in area di    *
      *                      * lavoro                                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-prs-age-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-cli-700.
       prs-age-cli-650.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri letti in area di lavoro       *
      *                      *-----------------------------------------*
           move      s-alf                to   w-prs-age-cli          .
      *                      *-----------------------------------------*
      *                      * A regolarizzazione parametri            *
      *                      *-----------------------------------------*
           go to     prs-age-cli-700.
       prs-age-cli-700.
      *              *-------------------------------------------------*
      *              * Regolarizzazione parametri                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo codice cliente da esporre          'C' *
      *                  *---------------------------------------------*
           if        w-prs-age-cli-tco    not  = "M" and
                     w-prs-age-cli-tco    not  = "N"
                     move  "C"            to   w-prs-age-cli-tco      .
      *                  *---------------------------------------------*
      *                  * Tipo ragione sociale da esporre         'C' *
      *                  *---------------------------------------------*
           if        w-prs-age-cli-trs    not  = "R"
                     move  "C"            to   w-prs-age-cli-trs      .
       prs-age-cli-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-age-cli-999.
       prs-age-cli-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente : Si                  *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico stampa : No         *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *                                                 *
      *              * Provvisoriamente posto a 'Si', per poi essere   *
      *              * riassestato, a seconda del tipo di esecuzione   *
      *              * prescelto, in fase di regolarizzazione dei pa-  *
      *              * rametri di selezione                            *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
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
      *              * [gpm]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *              *-------------------------------------------------*
      *              * [gpc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
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
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * [rsc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *              *-------------------------------------------------*
      *              * [numgpm]                                        *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
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
      *              * [gpm]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *              *-------------------------------------------------*
      *              * [gpc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
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
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * [rsc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *              *-------------------------------------------------*
      *              * [numgpm]                                        *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
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
      *                  * Tipo di esecuzione                          *
      *                  *---------------------------------------------*
           perform   acc-tip-exe-000      thru acc-tip-exe-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           perform   acc-cod-age-000      thru acc-cod-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento per agenti              *
      *                  *---------------------------------------------*
           perform   acc-tor-age-000      thru acc-tor-age-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Data min                                    *
      *                  *---------------------------------------------*
           perform   acc-dat-min-000      thru acc-dat-min-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
       acc-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Data max                                    *
      *                  *---------------------------------------------*
           perform   acc-dat-max-000      thru acc-dat-max-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-400.
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
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo di esecuzione                              *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tip-exe             .
      *              *-------------------------------------------------*
      *              * Codice agente e dati correlati                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-cod-age             .
           move      spaces               to   rr-cod-age-nom         .
           move      spaces               to   rr-cod-age-rag         .
           move      spaces               to   rr-cod-age-via         .
           move      spaces               to   rr-cod-age-loc         .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           move      zero                 to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Data min                                        *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-min             .
      *              *-------------------------------------------------*
      *              * Data max                                        *
      *              *-------------------------------------------------*
           move      zero                 to   rr-dat-max             .
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo di esecuzione                              *
      *              *-------------------------------------------------*
           perform   pmt-tip-exe-000      thru pmt-tip-exe-999        .
      *              *-------------------------------------------------*
      *              * Codice agente                                   *
      *              *-------------------------------------------------*
           perform   pmt-cod-age-000      thru pmt-cod-age-999        .
      *              *-------------------------------------------------*
      *              * Tipo di ordinamento per agenti                  *
      *              *-------------------------------------------------*
           perform   pmt-tor-age-000      thru pmt-tor-age-999        .
      *              *-------------------------------------------------*
      *              * Data min                                        *
      *              *-------------------------------------------------*
           perform   pmt-dat-min-000      thru pmt-dat-min-999        .
      *              *-------------------------------------------------*
      *              * Data max                                        *
      *              *-------------------------------------------------*
           perform   pmt-dat-max-000      thru pmt-dat-max-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di esecuzione                               *
      *    *-----------------------------------------------------------*
       pmt-tip-exe-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di esecuzione         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-exe-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Codice agente                                    *
      *    *-----------------------------------------------------------*
       pmt-cod-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice agente              :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Tipo di ordinamento per agenti                   *
      *    *-----------------------------------------------------------*
       pmt-tor-age-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Ordinamento agenti         :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data min                                         *
      *    *-----------------------------------------------------------*
       pmt-dat-min-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data documenti minima      :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Prompt : Data massima dei documenti                       *
      *    *-----------------------------------------------------------*
       pmt-dat-max-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data documenti massima dei :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " conteggi provvigionali su  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "           cui eseguire la  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "         generazione delle  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               maturazioni  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-max-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo di esecuzione                         *
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
           move      spaces               to   v-edm                  .
           move      w-exp-tip-exe-tbl    to   v-txt                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      05                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-exe           to   v-num                  .
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
       acc-tip-exe-410.
      *                  *---------------------------------------------*
      *                  * Che sia un valore previsto                  *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 01 and
                     rr-tip-exe           not  = 02 and
                     rr-tip-exe           not  = 03
                     go to acc-tip-exe-100.
       acc-tip-exe-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se si tratta di sola generazione si norma-  *
      *                  * lizza il tipo ordinamento agenti            *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 02
                     go to acc-tip-exe-675.
           move      zero                 to   rr-tor-age             .
           perform   vis-tor-age-000      thru vis-tor-age-999        .
       acc-tip-exe-675.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze da impostazione             *
      *                  *---------------------------------------------*
           go to     acc-tip-exe-800.
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
      *    * Accettazione : Codice agente                              *
      *    *-----------------------------------------------------------*
       acc-cod-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-age-ope      .
           move      rr-cod-age           to   w-cod-mne-age-cod      .
           move      08                   to   w-cod-mne-age-lin      .
           move      30                   to   w-cod-mne-age-pos      .
           move      08                   to   w-cod-mne-age-nln      .
           move      41                   to   w-cod-mne-age-nps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
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
      *                  *---------------------------------------------*
      *                  * Lettura archivio [age]                      *
      *                  *---------------------------------------------*
           move      rr-cod-age           to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione                  *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     move  "Tutti gli agenti                        "
                                          to   rr-cod-age-nom
                     move  spaces         to   rr-cod-age-rag
                     move  spaces         to   rr-cod-age-via
                     move  spaces         to   rr-cod-age-loc
           else      move  w-let-arc-age-nom
                                          to   rr-cod-age-nom
                     move  w-let-arc-age-rag
                                          to   rr-cod-age-rag
                     move  w-let-arc-age-via
                                          to   rr-cod-age-via
                     move  w-let-arc-age-loc
                                          to   rr-cod-age-loc         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione nominativo agente           *
      *                  *---------------------------------------------*
           perform   vis-cod-age-nom-000  thru vis-cod-age-nom-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-age-flg    not  = spaces
                     go to acc-cod-age-100.
       acc-cod-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-cod-age-625.
      *                  *---------------------------------------------*
      *                  * Se impostato un codice diverso da zero si   *
      *                  * normalizza il tipo ordinamento agenti       *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     go to acc-cod-age-650.
           move      zero                 to   rr-tor-age             .
           perform   vis-tor-age-000      thru vis-tor-age-999        .
       acc-cod-age-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze dall'impostazione           *
      *                  *---------------------------------------------*
           go to     acc-cod-age-800.
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
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Nominativo agente                       *
      *    *-----------------------------------------------------------*
       vis-cod-age-nom-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-age-nom       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-age-nom-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Tipo ordinamento per agenti                *
      *    *-----------------------------------------------------------*
       acc-tor-age-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tor-age-025.
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il tipo esecuzione non lo prevede    *
      *                      *-----------------------------------------*
           if        rr-tip-exe           =    02
                     go to acc-tor-age-999.
      *                      *-----------------------------------------*
      *                      * Se selezionato un solo agente non si    *
      *                      * accetta il campo                        *
      *                      *-----------------------------------------*
           if        rr-cod-age           not  = zero
                     go to acc-tor-age-999.
       acc-tor-age-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tor-age           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tor-age-999.
       acc-tor-age-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tor-age             .
       acc-tor-age-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-tor-age-425.
      *                  *---------------------------------------------*
      *                  * Che sia un valore consentito                *
      *                  *---------------------------------------------*
           if        rr-tor-age           not  = 01 and
                     rr-tor-age           not  = 02 and
                     rr-tor-age           not  = 03
                     go to acc-tor-age-100.
       acc-tor-age-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tor-age-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tor-age-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tor-age-100.
       acc-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo ordinamento per agenti             *
      *    *-----------------------------------------------------------*
       vis-tor-age-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tor-age-lun    to   v-car                  .
           move      w-exp-tor-age-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-exp-tor-age-tbl    to   v-txt                  .
           move      rr-tor-age           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tor-age-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data min                                   *
      *    *-----------------------------------------------------------*
       acc-dat-min-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-min-100.
      *              *-------------------------------------------------*
      *              * Note operative eventuali                        *
      *              *-------------------------------------------------*
       acc-dat-min-120.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      13                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-min           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-min-999.
       acc-dat-min-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-min             .
       acc-dat-min-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-min-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-min-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-min-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-min-100.
       acc-dat-min-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data massima documenti                     *
      *    *-----------------------------------------------------------*
       acc-dat-max-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-max-100.
      *              *-------------------------------------------------*
      *              * Note operative eventuali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-dat-mtz        not  = "X"
                     go to acc-dat-max-120.
      *                  *---------------------------------------------*
      *                  * Note operative                              *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      "La data documenti massima indicata vale anche come
      -              " data massima per le scadenze"
                                          to   v-nt1                  .
           move      "relative ai documenti conteggiati                 
      -              "                            "
                                          to   v-nt2                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-dat-max-120.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      15                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-dat-max           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione note operative attuali            *
      *              *-------------------------------------------------*
           move      "NT"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-max-999.
       acc-dat-max-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-max             .
       acc-dat-max-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non accettabile               *
      *                  *---------------------------------------------*
           if        rr-dat-max           not  = zero
                     go to acc-dat-max-600.
           if        v-key                =    "UP  "
                     go to acc-dat-max-600
           else      go to acc-dat-max-100.
       acc-dat-max-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-max-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-max-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-max-100.
       acc-dat-max-999.
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
      *              * Controllo su tipo di esecuzione                 *
      *              *-------------------------------------------------*
       tdo-ric-sel-105.
           if        rr-tip-exe           not  = zero
                     go to tdo-ric-sel-110.
           move      "Manca il tipo di esecuzione !                     
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-110.
           if        rr-tip-exe           =    01 or
                     rr-tip-exe           =    02 or
                     rr-tip-exe           =    03
           go to     tdo-ric-sel-115.
           move      "Tipo di esecuzione errato !                       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-115.
           go to     tdo-ric-sel-300.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Controllo : Tipo di ordinamento per agenti      *
      *              *-------------------------------------------------*
       tdo-ric-sel-305.
           if        rr-tip-exe           =    02
                     go to tdo-ric-sel-315.
           if        rr-cod-age           not  = zero
                     go to tdo-ric-sel-315.
           if        rr-tor-age           not  = zero
                     go to tdo-ric-sel-310.
           move      "Manca il tipo di ordinamento per gli agenti !     
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-310.
           if        rr-tor-age           =    01 or
                     rr-tor-age           =    02 or
                     rr-tor-age           =    03
                     go to tdo-ric-sel-315.
           move      "Tipo di ordinamento per gli agenti errato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-315.
           go to     tdo-ric-sel-400.
       tdo-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Controllo su data massima documenti             *
      *              *-------------------------------------------------*
       tdo-ric-sel-405.
           if        rr-dat-max           not  = zero
                     go to tdo-ric-sel-410.
           move      "Manca la data massima documenti !                 
      -              "               "    to   w-err-box-err-msg      .
           go to     tdo-ric-sel-900.
       tdo-ric-sel-410.
           go to     tdo-ric-sel-500.
       tdo-ric-sel-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
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
      *              * Se non e' previsto alcun tipo di stampa si for- *
      *              * za il tipo ordinamento agenti per codice        *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    02
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Se selezionato un solo agente si forza il tipo  *
      *              * ordinamento agenti per codice                   *
      *              *-------------------------------------------------*
           if        rr-cod-age           not  = zero
                     move  02             to   rr-tor-age             .
      *              *-------------------------------------------------*
      *              * Assestamento del parametro di si/no stampa a    *
      *              * seconda del tipo di esecuzione                  *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    01 or
                     rr-tip-exe           =    03
                     move  "S"            to   w-cnt-fun-snx-stp
           else      move  "N"            to   w-cnt-fun-snx-stp      .
       reg-ric-sel-999.
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
      *                  *---------------------------------------------*
      *                  * Se le richieste prevedono la gerazione ef-  *
      *                  * fettiva e la stampa, viene inibita l'opzio- *
      *                  * ne 'video' (a prova di imbecille)           *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    03
                     move  "#"            to   w-cnt-stp-esp-fut
                                              (98 : 1)                .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
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
      *              * Start su archivio [gpc]                         *
      *              *-------------------------------------------------*
       stp-srt-inp-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se e' stato richiesto  *
      *                  * un solo agente o tutti gli agenti           *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     go to stp-srt-inp-030.
       stp-srt-inp-020.
      *                  *---------------------------------------------*
      *                  * Se richiesto un solo agente                 *
      *                  *---------------------------------------------*
       stp-srt-inp-022.
      *                      *-----------------------------------------*
      *                      * Start per agente e data documento       *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ADNDOC    "         to   f-key                  .
           move      rr-cod-age           to   rf-gpc-cod-age         .
           move      rr-dat-min           to   rf-gpc-dat-doc         .
           move      spaces               to   rf-gpc-num-doc         .
           move      zero                 to   rf-gpc-num-ctg         .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
       stp-srt-inp-024.
      *                      *-----------------------------------------*
      *                      * Se Start errata si va' ad uscita, al-   *
      *                      * trimenti si continua con la lettura     *
      *                      * sequenziale                             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-srt-inp-100
           else      go to stp-srt-inp-950.
       stp-srt-inp-030.
      *                  *---------------------------------------------*
      *                  * Se richiesti tutti gli agenti               *
      *                  *---------------------------------------------*
       stp-srt-inp-032.
      *                      *-----------------------------------------*
      *                      * Start solo per data documento           *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DENDOC    "         to   f-key                  .
           move      rr-dat-min           to   rf-gpc-dat-doc         .
           move      spaces               to   rf-gpc-num-doc         .
           move      zero                 to   rf-gpc-num-ctg         .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
       stp-srt-inp-034.
      *                      *-----------------------------------------*
      *                      * Se Start errata si va' ad uscita, al-   *
      *                      * trimenti si continua con la lettura     *
      *                      * sequenziale                             *
      *                      *-----------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-srt-inp-100
           else      go to stp-srt-inp-950.
       stp-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Read next su archivio [gpc]                     *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
      *              *-------------------------------------------------*
      *              * Se fine file : ad uscita                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-950.
       stp-srt-inp-200.
      *              *-------------------------------------------------*
      *              * Test max su archivio [gpc], e se non superato : *
      *              * ad uscita                                       *
      *              *-------------------------------------------------*
       stp-srt-inp-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se e' stato richiesto  *
      *                  * un solo agente o tutti gli agenti           *
      *                  *---------------------------------------------*
           if        rr-cod-age           =    zero
                     go to stp-srt-inp-230.
       stp-srt-inp-220.
      *                  *---------------------------------------------*
      *                  * Se richiesto un solo agente                 *
      *                  *---------------------------------------------*
       stp-srt-inp-222.
      *                      *-----------------------------------------*
      *                      * Test su codice agente                   *
      *                      *-----------------------------------------*
           if        rf-gpc-cod-age       not  = rr-cod-age
                     go to stp-srt-inp-950.
       stp-srt-inp-224.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-gpc-dat-doc       >    rr-dat-max
                     go to stp-srt-inp-950.
       stp-srt-inp-226.
      *                      *-----------------------------------------*
      *                      * Test max superato                       *
      *                      *-----------------------------------------*
           go to     stp-srt-inp-300.
       stp-srt-inp-230.
      *                  *---------------------------------------------*
      *                  * Se richiesti tutti gli agenti               *
      *                  *---------------------------------------------*
       stp-srt-inp-232.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-gpc-dat-doc       >    rr-dat-max
                     go to stp-srt-inp-950.
       stp-srt-inp-234.
      *                      *-----------------------------------------*
      *                      * Test max superato                       *
      *                      *-----------------------------------------*
           go to     stp-srt-inp-300.
       stp-srt-inp-300.
      *              *-------------------------------------------------*
      *              * Selezione su archivio [gpc], e se non supera-   *
      *              * to : riciclo a record successivo                *
      *              *-------------------------------------------------*
       stp-srt-inp-310.
      *                  *---------------------------------------------*
      *                  * Test se conteggio senza provvigione         *
      *                  *---------------------------------------------*
           if        rf-gpc-per-pvg       =    zero and
                     rf-gpc-amm-pvg       =    zero
                     go to stp-srt-inp-100.
       stp-srt-inp-320.
      *                  *---------------------------------------------*
      *                  * Selezione superata                          *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-400.
       stp-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Richiamo della routine per la determinazione di *
      *              * nuove maturazioni a fronte di un conteggio, e   *
      *              * deviazione a seconda dell'esito della determi-  *
      *              * nazione                                         *
      *              *-------------------------------------------------*
       stp-srt-inp-405.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per il richiamo  *
      *                  * della subroutine                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura del record relativo al conteg-  *
      *                      * gio provvigionale : No, in quanto gia'  *
      *                      * presente in 'rf-gpc'                    *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-nmp-let-ctg      .
      *                      *-----------------------------------------*
      *                      * Numero conteggio per cui calcolare la   *
      *                      * nuova maturazione : pari a quello con-  *
      *                      * tenuto in 'rf-gpc'                      *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   w-det-nmp-num-ctg      .
      *                      *-----------------------------------------*
      *                      * Data di sistema                         *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Data di maturazione di riferimento per  *
      *                      * la nuova maturazione : pari alla data   *
      *                      * massima documento impostata             *
      *                      *                                         *
      *                      * Da personalizzazione                    *
      *                      *-----------------------------------------*
           if        w-prs-dat-mtz        =    "S"
                     move  s-dat          to   w-det-nmp-dat-mtz
           else      move  rr-dat-max     to   w-det-nmp-dat-mtz      .
       stp-srt-inp-410.
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo della subroutine         *
      *                  *---------------------------------------------*
           perform   det-nmp-000          thru det-nmp-999            .
       stp-srt-inp-415.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della rou-  *
      *                  * tine                                        *
      *                  *---------------------------------------------*
           if        w-det-nmp-flg-det    =    11 or
                     w-det-nmp-flg-det    =    51
                     go to stp-srt-inp-500
           else if   w-det-nmp-flg-det    =    01 or
                     w-det-nmp-flg-det    =    02 or
                     w-det-nmp-flg-det    =    12 or
                     w-det-nmp-flg-det    =    21 or
                     w-det-nmp-flg-det    =    22
                     go to stp-srt-inp-600
           else      go to stp-srt-inp-500.
       stp-srt-inp-500.
      *              *-------------------------------------------------*
      *              * Se esito della determinazione pari a :          *
      *              *                                                 *
      *              * - 11 : Maturazione a zero, in quanto le matura- *
      *              *        zioni pregresse coprono gia' completa-   *
      *              *        mente l'importo del conteggio provvigio- *
      *              *        nale                                     *
      *              * - 51 : Record del conteggio provvigionale non   *
      *              *        piu' esistente in archivio.              *
      *              *        Nota : questo status di uscita non puo'  *
      *              *               mai essere ritornato, in quanto   *
      *              *               non e' stata richiesta la lettu-  *
      *              *               ra del record relativo al conteg- *
      *              *               gio provvigionale                 *
      *              * - xx : Altri status non contemplati             *
      *              *-------------------------------------------------*
       stp-srt-inp-510.
      *                  *---------------------------------------------*
      *                  * Nessuna azione, si ignora il record relati- *
      *                  * vo al conteggio provvigionale, e si ricicla *
      *                  * a leggere quello successivo                 *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-100.
       stp-srt-inp-600.
      *              *-------------------------------------------------*
      *              * Se esito della determinazione pari a :          *
      *              *                                                 *
      *              * - 01 : Maturazione eseguita, di tipo immediato  *
      *              * - 02 : Maturazione eseguita, di tipo a fronte   *
      *              *        incasso                                  *
      *              * - 12 : Maturazione a zero, per mancanza di nuo- *
      *              *        vi incassi che facciano scattare nuove   *
      *              *        maturazioni                              *
      *              * - 21 : Maturazione a zero, perche' il conteggio *
      *              *        provvigionale risulta attualmente bloc-  *
      *              *        cato                                     *
      *              * - 22 : Maturazione a zero, perche' il conteggio *
      *              *        provvigionale indica una data di matura- *
      *              *        zione minima superiore alla data di ri-  *
      *              *        ferimento per la maturazione             *
      *              *-------------------------------------------------*
       stp-srt-inp-610.
      *                  *---------------------------------------------*
      *                  * Letture records preliminari                 *
      *                  *---------------------------------------------*
       stp-srt-inp-612.
      *                      *-----------------------------------------*
      *                      * Anagrafica agente                       *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
       stp-srt-inp-614.
      *                      *-----------------------------------------*
      *                      * Anagrafica agente subordinato           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   w-let-arc-ags-cod      .
           perform   let-arc-ags-000      thru let-arc-ags-999        .
       stp-srt-inp-616.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente di fatturazione      *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   w-let-cli-dcc-cli      .
           move      rf-gpc-dpz-cli       to   w-let-cli-dcc-dpz      .
           perform   let-cli-dcc-000      thru let-cli-dcc-999        .
       stp-srt-inp-618.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente per la fatturazione  *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   w-let-plf-dcc-cli      .
           move      rf-gpc-dpz-plf       to   w-let-plf-dcc-dpz      .
           perform   let-plf-dcc-000      thru let-plf-dcc-999        .
       stp-srt-inp-630.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare record si sort  *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
       stp-srt-inp-650.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa alla subchiave 1 : per agente      *
      *                  *---------------------------------------------*
       stp-srt-inp-652.
      *                      *-----------------------------------------*
      *                      * Flag di codice diverso da zero ma non   *
      *                      * trovato in archivio agenti              *
      *                      *                                         *
      *                      * Se ordinamento agenti per codice viene  *
      *                      * forzato a zero                          *
      *                      *                                         *
      *                      * Se ordinamento agenti per nominativo o  *
      *                      * per mnemonico viene posto a seconda del *
      *                      * valore del codice agente e dell'esi-    *
      *                      * stenza o meno dell'anagrafica           *
      *                      *-----------------------------------------*
           if        rr-tor-age           =    02
                     move  zero           to   srt-k01-flg-eon
           else if   rf-gpc-cod-age       =    zero
                     move  9              to   srt-k01-flg-eon
           else if   w-let-arc-age-flg    =    spaces
                     move  1              to   srt-k01-flg-eon
           else      move  5              to   srt-k01-flg-eon        .
       stp-srt-inp-654.
      *                      *-----------------------------------------*
      *                      * Nominativo per l'agente                 *
      *                      *                                         *
      *                      * Se ordinamento agenti per codice o per  *
      *                      * mnemonico viene forzato a spaces        *
      *                      *                                         *
      *                      * Se ordinamento agenti per nominativo    *
      *                      * viene posto al valore proveniente dal-  *
      *                      * l'anagrafica agente, e a seconda della  *
      *                      * personalizzazione che indica se espor-  *
      *                      * re il nome o la ragione sociale         *
      *                      *-----------------------------------------*
           if        rr-tor-age           =    02 or
                     rr-tor-age           =    03
                     move  spaces         to   srt-k01-ron-age
           else if   w-prs-age-age-ron    =    "R"
                     move  w-let-arc-age-rag
                                          to   srt-k01-ron-age
           else      move  w-let-arc-age-nom
                                          to   srt-k01-ron-age        .
       stp-srt-inp-656.
      *                      *-----------------------------------------*
      *                      * Mnemonico per l'agente                  *
      *                      *                                         *
      *                      * Se ordinamento agenti per codice o per  *
      *                      * nominativo viene forzato a spaces       *
      *                      *                                         *
      *                      * Se ordinamento agenti per mnemonico,    *
      *                      * viene posto al valore proveniente dal-  *
      *                      * l'anagrafica agente                     *
      *                      *-----------------------------------------*
           if        rr-tor-age           =    01 or
                     rr-tor-age           =    02
                     move  spaces         to   srt-k01-mne-age
           else      move  w-let-arc-age-mne
                                          to   srt-k01-mne-age        .
       stp-srt-inp-658.
      *                      *-----------------------------------------*
      *                      * Flag di codice a zero o diverso da zero *
      *                      *                                         *
      *                      * Se ordinamento agenti per nominativo o  *
      *                      * per mnemonico viene forzato a zero      *
      *                      *                                         *
      *                      * Se ordinamento agenti per codice, viene *
      *                      * posto a seconda del valore del codice   *
      *                      *-----------------------------------------*
           if        rr-tor-age           =    01 or
                     rr-tor-age           =    03
                     move  zero           to   srt-k01-flg-zod
           else if   rf-gpc-cod-age       =    zero
                     move  9              to   srt-k01-flg-zod
           else      move  zero           to   srt-k01-flg-zod        .
       stp-srt-inp-660.
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   srt-k01-cod-age        .
       stp-srt-inp-670.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa alla subchiave 2 : per documento   *
      *                  * relativo al conteggio provvigionale         *
      *                  *---------------------------------------------*
       stp-srt-inp-672.
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   srt-k02-dat-doc        .
       stp-srt-inp-674.
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   srt-k02-num-doc        .
       stp-srt-inp-676.
      *                      *-----------------------------------------*
      *                      * Numero conteggio                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   srt-k02-num-ctg        .
       stp-srt-inp-690.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al conteg-  *
      *                  * gio provvigionale                           *
      *                  *---------------------------------------------*
       stp-srt-inp-692.
      *                      *-----------------------------------------*
      *                      * Numero conteggio                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   srt-gpc-num-ctg        .
       stp-srt-inp-694.
      *                      *-----------------------------------------*
      *                      * Numero protocollo movimento di fatture  *
      *                      * clienti                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-prt-fcl       to   srt-gpc-prt-fcl        .
       stp-srt-inp-696.
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   srt-gpc-tip-ctg        .
       stp-srt-inp-697.
      *                      *-----------------------------------------*
      *                      * Tipo di vendita per l'agente            *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-vpa       to   srt-gpc-tip-vpa        .
       stp-srt-inp-698.
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   srt-gpc-dat-doc        .
       stp-srt-inp-700.
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   srt-gpc-num-doc        .
       stp-srt-inp-702.
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   srt-gpc-cod-age        .
       stp-srt-inp-704.
      *                      *-----------------------------------------*
      *                      * Codice cliente di fatturazione          *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   srt-gpc-cod-cli        .
       stp-srt-inp-706.
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente di fattu- *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-cli       to   srt-gpc-dpz-cli        .
       stp-srt-inp-708.
      *                      *-----------------------------------------*
      *                      * Imponibile provvigionale                *
      *                      *-----------------------------------------*
           move      rf-gpc-ibl-pvg       to   srt-gpc-ibl-pvg        .
       stp-srt-inp-710.
      *                      *-----------------------------------------*
      *                      * % di provvigione                        *
      *                      *-----------------------------------------*
           move      rf-gpc-per-pvg       to   srt-gpc-per-pvg        .
       stp-srt-inp-712.
      *                      *-----------------------------------------*
      *                      * Ammontare provvigione conteggiata sul   *
      *                      * documento                               *
      *                      *-----------------------------------------*
           move      rf-gpc-amm-pvg       to   srt-gpc-amm-pvg        .
       stp-srt-inp-714.
      *                      *-----------------------------------------*
      *                      * Importo totale del documento cui si ri- *
      *                      * ferisce il conteggio                    *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-doc       to   srt-gpc-imp-doc        .
       stp-srt-inp-716.
      *                      *-----------------------------------------*
      *                      * Importo totale di eventuali acconti gia'*
      *                      * fatturati assorbiti nel documento cui   *
      *                      * si riferisce il conteggio               *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-agf       to   srt-gpc-imp-agf        .

       stp-srt-inp-718.
      *                      *-----------------------------------------*
      *                      * Annotazioni sul conteggio provvigionale *
      *                      *-----------------------------------------*
           move      rf-gpc-not-ctg       to   srt-gpc-not-ctg        .
       stp-srt-inp-720.
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione prevista per la     *
      *                      * provvigione conteggiata                 *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-mat       to   srt-gpc-tip-mat        .
       stp-srt-inp-722.
      *                      *-----------------------------------------*
      *                      * Segnale di provvigione con maturazione  *
      *                      * bloccata                                *
      *                      *-----------------------------------------*
           move      rf-gpc-mat-blo       to   srt-gpc-mat-blo        .
       stp-srt-inp-724.
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima per la prov- *
      *                      * vigione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-ddm-min       to   srt-gpc-ddm-min        .
       stp-srt-inp-726.
      *                      *-----------------------------------------*
      *                      * Codice agente subordinato               *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   srt-gpc-cod-ags        .
       stp-srt-inp-728.
      *                      *-----------------------------------------*
      *                      * Codice del cliente per la fatturazione  *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   srt-gpc-cod-plf        .
       stp-srt-inp-730.
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente per la    *
      *                      * fatturazione                            *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-plf       to   srt-gpc-dpz-plf        .
       stp-srt-inp-750.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa all'agente  *
      *                  *---------------------------------------------*
       stp-srt-inp-752.
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-age-mne    to   srt-age-mne-age        .
       stp-srt-inp-754.
      *                      *-----------------------------------------*
      *                      * Nominativo                              *
      *                      *-----------------------------------------*
           move      w-let-arc-age-nom    to   srt-age-nom-age        .
       stp-srt-inp-756.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-arc-age-rag    to   srt-age-rag-age        .
       stp-srt-inp-770.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al clien-   *
      *                  * te di fatturazione                          *
      *                  *---------------------------------------------*
       stp-srt-inp-772.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rag    to   srt-cli-rag-cli        .
       stp-srt-inp-774.
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-via    to   srt-cli-via-cli        .
       stp-srt-inp-776.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-loc    to   srt-cli-loc-cli        .
       stp-srt-inp-790.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa all'agente  *
      *                  * subordinato                                 *
      *                  *---------------------------------------------*
       stp-srt-inp-792.
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-mne    to   srt-ags-mne-ags        .
       stp-srt-inp-794.
      *                      *-----------------------------------------*
      *                      * Nominativo                              *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-nom    to   srt-ags-nom-ags        .
       stp-srt-inp-796.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-rag    to   srt-ags-rag-ags        .
       stp-srt-inp-810.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al clien-   *
      *                  * te per la fatturazione                      *
      *                  *---------------------------------------------*
       stp-srt-inp-812.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-rag    to   srt-plf-rag-plf        .
       stp-srt-inp-814.
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-via    to   srt-plf-via-plf        .
       stp-srt-inp-816.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-loc    to   srt-plf-loc-plf        .
       stp-srt-inp-830.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa alla matu-  *
      *                  * razione vera e propria                      *
      *                  *---------------------------------------------*
       stp-srt-inp-832.
      *                      *-----------------------------------------*
      *                      * Flag di uscita dalla routine di deter-  *
      *                      * minazione della nuova maturazione       *
      *                      *-----------------------------------------*
           move      w-det-nmp-flg-det    to   srt-gpm-flg-det        .
       stp-srt-inp-834.
      *                      *-----------------------------------------*
      *                      * Si/No maturazione effettiva             *
      *                      *                                         *
      *                      * In funzione dei flags di uscita dalla   *
      *                      * routine di determinazione della nuo-    *
      *                      * va maturazione, che indicano il tipo    *
      *                      * di maturazione determinata, e se posi-  *
      *                      * tiva o negativa in caso di storno       *
      *                      *-----------------------------------------*
           if        w-det-nmp-flg-det    =    01 or
                     w-det-nmp-flg-det    =    02
                     move  "S"            to   srt-gpm-snx-mef
           else      move  "N"            to   srt-gpm-snx-mef        .
       stp-srt-inp-836.
      *                      *-----------------------------------------*
      *                      * Numero maturazione                      *
      *                      *                                         *
      *                      * Solamente se la maturazione e' effetti- *
      *                      * va, e la generazione e' di tipo defini- *
      *                      * tivo                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se la maturazione non e' effettiva  *
      *                          * si forza il numero maturazione a    *
      *                          * zero                                *
      *                          *-------------------------------------*
           if        srt-gpm-snx-mef      not  = "S"
                     move  zero           to   srt-gpm-num-mtz
                     go to stp-srt-inp-838.
      *                          *-------------------------------------*
      *                          * Se la generazione non e' di tipo    *
      *                          * definitivo si forza il numero ma-   *
      *                          * turazione a zero                    *
      *                          *-------------------------------------*
           if        rr-tip-exe           not  = 02 and
                     rr-tip-exe           not  = 03
                     move  zero           to   srt-gpm-num-mtz
                     go to stp-srt-inp-838.
      *                          *-------------------------------------*
      *                          * Richiamo routine di attribuzione di *
      *                          * un nuovo numero di maturazione      *
      *                          *-------------------------------------*
           perform   att-num-gpm-000      thru att-num-gpm-999        .
      *                          *-------------------------------------*
      *                          * Nuovo numero maturazione attribuito *
      *                          * in campo di destinazione            *
      *                          *-------------------------------------*
           move      w-num-gpm-num-gpm    to   srt-gpm-num-mtz        .
       stp-srt-inp-838.
      *                      *-----------------------------------------*
      *                      * Data di riferimento per la maturazione  *
      *                      *                                         *
      *                      * Da personalizzazione                    *
      *                      *-----------------------------------------*
           if        w-prs-dat-mtz        =    "S"
                     move  s-dat          to   srt-gpm-dat-mtz
           else      move  rr-dat-max     to   srt-gpm-dat-mtz        .
       stp-srt-inp-840.
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione effettuata          *
      *                      *                                         *
      *                      * Solo se la maturazione e' effettiva, ed *
      *                      * inoltre in funzione del flag di uscita  *
      *                      * dalla routine di determinazione della   *
      *                      * nuova maturazione per verificare se si  *
      *                      * tratta di una vera maturazione, e di    *
      *                      * quale tipo, o di uno storno di matura-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           if        srt-gpm-snx-mef      not  = "S"
                     move  zero           to   srt-gpm-tip-mtz
                     go to stp-srt-inp-842.
           if        w-det-nmp-flg-det    =    02
                     move  02             to   srt-gpm-tip-mtz
           else      move  01             to   srt-gpm-tip-mtz        .
       stp-srt-inp-842.
      *                      *-----------------------------------------*
      *                      * Importo incassato, che ha fatto scatta- *
      *                      * re la maturazione                       *
      *                      *                                         *
      *                      * Solo se tipo di maturazione effettuata  *
      *                      * pari a 02, altrimenti a zero            *
      *                      *-----------------------------------------*
           if        srt-gpm-tip-mtz      =    02
                     move  w-det-nmp-imp-inc
                                          to   srt-gpm-imp-inc
           else      move  zero           to   srt-gpm-imp-inc        .
       stp-srt-inp-844.
      *                      *-----------------------------------------*
      *                      * Importo di provvigione maturato         *
      *                      *                                         *
      *                      * Solo se tipo di maturazione effettuata  *
      *                      * pari a 01 o 02 , altrimenti a zero      *
      *                      *-----------------------------------------*
           if        srt-gpm-tip-mtz      =    01 or
                     srt-gpm-tip-mtz      =    02
                     move  w-det-nmp-imp-mtz
                                          to   srt-gpm-imp-mtz
           else      move  zero           to   srt-gpm-imp-mtz        .
       stp-srt-inp-846.
      *                      *-----------------------------------------*
      *                      * Annotazioni sulla maturazione           *
      *                      *-----------------------------------------*
           move      spaces               to   srt-gpm-not-mtz        .
       stp-srt-inp-880.
      *                  *---------------------------------------------*
      *                  * Rilascio del record al Sort                 *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-900.
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura del conteggio provvi-  *
      *                  * gionale successivo                          *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-100.
       stp-srt-inp-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-str-ini-100.
      *              *-------------------------------------------------*
      *              * Generazione del file sequenziale di appoggio e  *
      *              * generazione delle maturazioni definitive        *
      *              *-------------------------------------------------*
           perform   gen-sqz-gpm-000      thru gen-sqz-gpm-999        .
       prn-str-ini-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se il tipo di esecuzione   *
      *              * prevede di stampare oppure no                   *
      *              *-------------------------------------------------*
           if        rr-tip-exe           =    02
                     go to prn-str-ini-300
           else      go to prn-str-ini-400.
       prn-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se l'esecuzione non prevede stampa, bensi' solo *
      *              * generazione definitiva di maturazioni           *
      *              *-------------------------------------------------*
       prn-str-ini-310.
      *                  *---------------------------------------------*
      *                  * Se non e' stata eseguita alcuna scrittura   *
      *                  * di maturazione definitiva : si emette un    *
      *                  * messaggio di anomalia                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sqz-gpm-ctr-gpm    not  = zero
                     go to prn-str-ini-320.
      *                      *-----------------------------------------*
      *                      * Emissione del messaggio di anomalia     *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "Nessuna nuova maturazione da generare entro la dat
      -              "a massima impostata !         "
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-str-ini-320.
      *                  *---------------------------------------------*
      *                  * Flag di fine ciclo in On                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-400.
      *              *-------------------------------------------------*
      *              * Se l'esecuzione prevede di stampare             *
      *              *-------------------------------------------------*
       prn-str-ini-410.
      *                  *---------------------------------------------*
      *                  * Se non e' stata incontrata alcuna matura-   *
      *                  * zione definitiva : si emette un messaggio   *
      *                  * di errore                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-sqz-gpm-ctr-gpm    not  = zero
                     go to prn-str-ini-420.
      *                      *-----------------------------------------*
      *                      * Emissione del messaggio di anomalia     *
      *                      *-----------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      "Nessuna nuova maturazione da generare entro la dat
      -              "a massima impostata !         "
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-str-ini-420.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del numero di records  *
      *                  * generati su file sequenziale di appoggio    *
      *                  * [sqz]                                       *
      *                  *---------------------------------------------*
           if        w-sqz-gpm-ctr-sqz    =    zero
                     go to prn-str-ini-500
           else      go to prn-str-ini-600.
       prn-str-ini-500.
      *                  *---------------------------------------------*
      *                  * Se non e' stato generato alcun record sul   *
      *                  * file sequenziale di appoggio [sqz]          *
      *                  *---------------------------------------------*
       prn-str-ini-510.
      *                      *-----------------------------------------*
      *                      * Delete del file di appoggio sequenziale *
      *                      * [sqz]                                   *
      *                      *-----------------------------------------*
           perform   del-fil-sqz-000      thru del-fil-sqz-999        .
       prn-str-ini-520.
      *                      *-----------------------------------------*
      *                      * Flag di fine ciclo in On                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-600.
      *                  *---------------------------------------------*
      *                  * Se e' stato generato almeno un record sul   *
      *                  * file sequenziale di appoggio [sqz]          *
      *                  *---------------------------------------------*
       prn-str-ini-610.
      *                      *-----------------------------------------*
      *                      * Riapertura in input del file di appog-  *
      *                      * gio sequenziale                         *
      *                      *-----------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
       prn-str-ini-620.
      *                      *-----------------------------------------*
      *                      * Uscita per continuare il ciclo          *
      *                      *-----------------------------------------*
           go to     prn-str-ini-999.
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Generazione del file sequenziale di appoggio da records   *
      *    * usciti dal sort, e anche, se necessario, scrittura del-   *
      *    * le maturazioni definitive                                 *
      *    *-----------------------------------------------------------*
       gen-sqz-gpm-000.
      *              *-------------------------------------------------*
      *              * Numero di records scritti sul file di appoggio  *
      *              * sequenziale [sqz] : a zero                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sqz-gpm-ctr-sqz      .
      *              *-------------------------------------------------*
      *              * Numero di maturazioni definitive scritte sul    *
      *              * file [gpm] : a zero                             *
      *              *-------------------------------------------------*
           move      zero                 to   w-sqz-gpm-ctr-gpm      .
       gen-sqz-gpm-050.
      *              *-------------------------------------------------*
      *              * Apertura file di appoggio sequenziale [sqz]     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo di esecuzione non prevede la     *
      *                  * stampa : non si esegue l'apertura           *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    02
                     go to gen-sqz-gpm-100.
      *                  *---------------------------------------------*
      *                  * Open [sqz]                                  *
      *                  *---------------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
       gen-sqz-gpm-100.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt], se  *
      *              * fine file : a chiusura file di appoggio [sqz]   *
      *              *-------------------------------------------------*
           return    srt    at end
                            go to gen-sqz-gpm-900.
       gen-sqz-gpm-200.
      *              *-------------------------------------------------*
      *              * Scrittura record file sequenziale [sqz]         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo di esecuzione non prevede la     *
      *                  * stampa : non si esegue la scrittura         *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    02
                     go to gen-sqz-gpm-300.
      *                  *---------------------------------------------*
      *                  * Da area record [srt] ad area record file    *
      *                  * sequenziale [sqz]                           *
      *                  *---------------------------------------------*
           move      srt-rec              to   sqz-rec                .
      *                  *---------------------------------------------*
      *                  * Write [sqz]                                 *
      *                  *---------------------------------------------*
           perform   wrt-fil-sqz-000      thru wrt-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Incremento numero records scritti su [sqz]  *
      *                  *---------------------------------------------*
           add       1                    to   w-sqz-gpm-ctr-sqz      .
       gen-sqz-gpm-300.
      *              *-------------------------------------------------*
      *              * Se si tratta di una maturazione effettiva si    *
      *              * incrementa il numero di records nuove matura-   *
      *              * zioni scritte su [gpm], anche se non si tratta  *
      *              * di una esecuzione di tipo definitivo            *
      *              *-------------------------------------------------*
           if        srt-gpm-snx-mef      =    "S"
                     add   1              to   w-sqz-gpm-ctr-gpm      .
       gen-sqz-gpm-350.
      *              *-------------------------------------------------*
      *              * Se la generazione delle maturazioni non e' di   *
      *              * tipo definitivo : nessun'altra azione, si ri-   *
      *              * cicla alla lettura del file sortato             *
      *              *-------------------------------------------------*
           if        rr-tip-exe           not  = 02 and
                     rr-tip-exe           not  = 03
                     go to gen-sqz-gpm-800.
       gen-sqz-gpm-400.
      *              *-------------------------------------------------*
      *              * Se la generazione e' di tipo definitivo : si    *
      *              * scrive il record relativo alla nuova matura-    *
      *              * zione, se necessario                            *
      *              *-------------------------------------------------*
       gen-sqz-gpm-425.
      *                  *---------------------------------------------*
      *                  * Se non si tratta di una maturazione defini- *
      *                  * tiva, bensi' solo di una segnalazione : non *
      *                  * si eseguue nessuna scrittura su [gpm]       *
      *                  *---------------------------------------------*
           if        srt-gpm-snx-mef      not  = "S"
                     go to gen-sqz-gpm-800.
       gen-sqz-gpm-450.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [gpm]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
       gen-sqz-gpm-500.
      *                  *---------------------------------------------*
      *                  * Composizione record [gpm] da area record    *
      *                  * sortato [srt]                               *
      *                  *---------------------------------------------*
       gen-sqz-gpm-505.
      *                      *-----------------------------------------*
      *                      * Data di sistema di ultimo inserimento o *
      *                      * modifica, utente, fase                  *
      *                      *-----------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-gpm-ide-dat         .
           move      s-ute                to   rf-gpm-ide-ute         .
           move      s-fas                to   rf-gpm-ide-fas         .
       gen-sqz-gpm-510.
      *                      *-----------------------------------------*
      *                      * Numero maturazione                      *
      *                      *-----------------------------------------*
           move      srt-gpm-num-mtz      to   rf-gpm-num-mtz         .
       gen-sqz-gpm-515.
      *                      *-----------------------------------------*
      *                      * Numero conteggio provvigionale di rife- *
      *                      * rimento                                 *
      *                      *-----------------------------------------*
           move      srt-gpc-num-ctg      to   rf-gpm-num-ctg         .
       gen-sqz-gpm-520.
      *                      *-----------------------------------------*
      *                      * Data di riferimento per la maturazione  *
      *                      *-----------------------------------------*
           move      srt-gpm-dat-mtz      to   rf-gpm-dat-mtz         .
       gen-sqz-gpm-525.
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione                     *
      *                      *-----------------------------------------*
           move      srt-gpm-tip-mtz      to   rf-gpm-tip-mtz         .
       gen-sqz-gpm-530.
      *                      *-----------------------------------------*
      *                      * Importo incassato, che fa' scattare la  *
      *                      * maturazione proporzionalmente all'im-   *
      *                      * porto totale del documento, in caso di  *
      *                      * provvigione a fronte incasso            *
      *                      *-----------------------------------------*
           move      srt-gpm-imp-inc      to   rf-gpm-imp-inc         .
       gen-sqz-gpm-535.
      *                      *-----------------------------------------*
      *                      * Importo di provvigione maturato         *
      *                      *-----------------------------------------*
           move      srt-gpm-imp-mtz      to   rf-gpm-imp-mtz         .
       gen-sqz-gpm-540.
      *                      *-----------------------------------------*
      *                      * Annotazioni sulla maturazione           *
      *                      *-----------------------------------------*
           move      srt-gpm-not-mtz      to   rf-gpm-not-mtz         .
       gen-sqz-gpm-545.
      *                      *-----------------------------------------*
      *                      * Flags di elaborazione                   *
      *                      *-----------------------------------------*
           move      spaces               to   rf-gpm-flg-ela         .
       gen-sqz-gpm-550.
      *                      *-----------------------------------------*
      *                      * Flag di sottoponibilita' a pulizia      *
      *                      *-----------------------------------------*
           move      spaces               to   rf-gpm-flg-pul         .
       gen-sqz-gpm-555.
      *                      *-----------------------------------------*
      *                      * Area libera per espansioni speciali     *
      *                      *-----------------------------------------*
           move      spaces               to   rf-gpm-alx-exp         .
       gen-sqz-gpm-600.
      *                  *---------------------------------------------*
      *                  * Put record [gpm]                            *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
       gen-sqz-gpm-800.
      *              *-------------------------------------------------*
      *              * Riciclo a lettura record successivo da [srt]    *
      *              *-------------------------------------------------*
           go to     gen-sqz-gpm-100.
       gen-sqz-gpm-900.
      *              *-------------------------------------------------*
      *              * Chiusura file di appoggio sequenziale [sqz]     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo di esecuzione non prevede la     *
      *                  * stampa : non si esegue la chiusura          *
      *                  *---------------------------------------------*
           if        rr-tip-exe           =    02
                     go to gen-sqz-gpm-950.
      *                  *---------------------------------------------*
      *                  * Close [sqz]                                 *
      *                  *---------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
       gen-sqz-gpm-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     gen-sqz-gpm-999.
       gen-sqz-gpm-999.
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
      *              * Se il pathname per il file [sqz] non e' ancora  *
      *              * stato determinato : nessuna azione              *
      *              *-------------------------------------------------*
           if        f-sqz-pat            =    spaces
                     go to cls-fil-sqz-999.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     sqz                                              .
       cls-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Delete file di appoggio sequenziale [sqz]                 *
      *    *-----------------------------------------------------------*
       del-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [sqz] non e' ancora  *
      *              * stato determinato : nessuna azione              *
      *              *-------------------------------------------------*
           if        f-sqz-pat            =    spaces
                     go to del-fil-sqz-999.
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
      *    * Ciclo Report Program : Messaggio per nessuna registrazio- *
      *    *                        ne                                 *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
      *              *-------------------------------------------------*
      *              * Nessun messaggio in ogni caso, in quanto gia'   *
      *              * trattati alla routine di Start                  *
      *              *-------------------------------------------------*
           go to     prn-nes-ela-999.
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
       prn-let-seq-100.
      *              *-------------------------------------------------*
      *              * Lettura del file sequenziale di appoggio        *
      *              *-------------------------------------------------*
           perform   rea-fil-sqz-000      thru rea-fil-sqz-999        .
      *              *-------------------------------------------------*
      *              * Se fine file : si pone in On il flag di uscita  *
      *              * e si esce                                       *
      *              *-------------------------------------------------*
           if        f-sqz-sts            not  = e-not-err
                     move  "#"            to   w-cnt-prn-flg-sub
                     go to prn-let-seq-999.
       prn-let-seq-200.
      *              *-------------------------------------------------*
      *              * Da area record sequenziale [sqz] ad area record *
      *              * di sort [srt] per ridefinizione                 *
      *              *-------------------------------------------------*
           move      sqz-rec              to   srt-rec                .
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
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      srt-gpc-cod-age      to   w-rot-l01-cod-age      .
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
       prn-ini-cic-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione work area per livello di rot-  *
      *              * tura generale                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale importo maturato                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-tot-mat      .
      *                  *---------------------------------------------*
      *                  * Numero di agenti trattati in totale         *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-gen-num-age      .
      *                  *---------------------------------------------*
      *                  * Flag di fine ciclo in esecuzione : No       *
      *                  *---------------------------------------------*
           move      spaces               to   w-liv-gen-flg-end      .
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Flag di fine ciclo in esecuzione : Si           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-liv-gen-flg-end      .
       prn-fin-cic-200.
      *              *-------------------------------------------------*
      *              * Close file di appoggio sequenziale [sqz]        *
      *              *-------------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
      *              *-------------------------------------------------*
      *              * Delete del file di appoggio sequenziale [sqz]   *
      *              *-------------------------------------------------*
           perform   del-fil-sqz-000      thru del-fil-sqz-999        .
       prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Se non sono stati trattati almeno due agenti :  *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           if        w-liv-gen-num-age    not  > 1
                     go to prn-fin-cic-999.
       prn-fin-cic-400.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero pagina da stampare a zero            *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-num-pag      .
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-500.
      *              *-------------------------------------------------*
      *              * Intestazione agente per totale generale         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo intestazione : per il totale gene- *
      *                      * rale                                    *
      *                      *-----------------------------------------*
           move      "T"                  to   w-stp-age-tip-int      .
      *                      *-----------------------------------------*
      *                      * Valore numerico per il codice agente :  *
      *                      * non significativo                       *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-age-cod-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico per il codice agen- *
      *                      * te : non significativo                  *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-age-cod-alf      .
      *                      *-----------------------------------------*
      *                      * Descrizione per il codice agente : non  *
      *                      * significativa                           *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-age-cod-des      .
      *                  *---------------------------------------------*
      *                  * Richiamo subroutine                         *
      *                  *---------------------------------------------*
           perform   stp-int-age-000      thru stp-int-age-999        .
       prn-fin-cic-600.
      *              *-------------------------------------------------*
      *              * Stampa dei totali generali                      *
      *              *-------------------------------------------------*
           perform   stp-tot-gen-000      thru stp-tot-gen-999        .
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
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
      *              * tura per codice agente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente                               *
      *                  *---------------------------------------------*
           move      srt-gpc-cod-age      to   w-liv-age-cod-age      .
      *                  *---------------------------------------------*
      *                  * Nominativo per l'agente                     *
      *                  *---------------------------------------------*
           move      srt-age-nom-age      to   w-liv-age-nom-age      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale per l'agente                *
      *                  *---------------------------------------------*
           move      srt-age-rag-age      to   w-liv-age-rag-age      .
      *                  *---------------------------------------------*
      *                  * Mnemonico per l'agente                      *
      *                  *---------------------------------------------*
           move      srt-age-mne-age      to   w-liv-age-mne-age      .
      *                  *---------------------------------------------*
      *                  * Totale importo maturato                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-age-tot-mat      .
      *                  *---------------------------------------------*
      *                  * Numero maturazioni trattate per l'agente    *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-age-num-mat      .
      *                  *---------------------------------------------*
      *                  * Numero di agenti trattati                   *
      *                  *---------------------------------------------*
           add       1                    to   w-liv-gen-num-age      .
       prn-ini-lr1-200.
      *              *-------------------------------------------------*
      *              * Numero pagina da stampare a zero                *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-int-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata: uscita         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-ini-lr1-999.
       prn-ini-lr1-300.
      *              *-------------------------------------------------*
      *              * Intestazione agente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo intestazione : per un codice agen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           move      "C"                  to   w-stp-age-tip-int      .
      *                      *-----------------------------------------*
      *                      * Valore numerico per il codice agente    *
      *                      *-----------------------------------------*
           move      w-liv-age-cod-age    to   w-stp-age-cod-num      .
      *                      *-----------------------------------------*
      *                      * Valore alfanumerico per il codice agen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           move      w-liv-age-mne-age    to   w-stp-age-cod-alf      .
      *                      *-----------------------------------------*
      *                      * Descrizione per il codice agente        *
      *                      *-----------------------------------------*
           if        w-prs-age-age-ron    =    "R"
                     move  w-liv-age-rag-age
                                          to   w-stp-age-cod-des
           else      move  w-liv-age-nom-age
                                          to   w-stp-age-cod-des      .
      *                  *---------------------------------------------*
      *                  * Richiamo subroutine                         *
      *                  *---------------------------------------------*
           perform   stp-int-age-000      thru stp-int-age-999        .
       prn-ini-lr1-400.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa dei ti- *
      *              * toli relativi alla fincatura verticale della    *
      *              * pagina                                          *
      *              *-------------------------------------------------*
           perform   stp-tfv-vrt-000      thru stp-tfv-vrt-999        .
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa della   *
      *              * linea di sottolineatura relativa alla fincatu-  *
      *              * ra verticale della pagina                       *
      *              *-------------------------------------------------*
           perform   stp-lds-fvp-000      thru stp-lds-fvp-999        .
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
       prn-fin-lr1-200.
      *              *-------------------------------------------------*
      *              * Test se linee residue sufficienti               *
      *              *-------------------------------------------------*
           if        p-res                >    3
                     go to prn-fin-lr1-300.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se flag di interruzione forzata: uscita         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-999.
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa dei ti- *
      *              * toli relativi alla fincatura verticale della    *
      *              * pagina                                          *
      *              *-------------------------------------------------*
           perform   stp-tfv-vrt-000      thru stp-tfv-vrt-999        .
       prn-fin-lr1-300.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Richiamo della subroutine per la stampa della   *
      *              * linea di sottolineatura relativa alla finca-    *
      *              * tura verticale della pagina                     *
      *              *-------------------------------------------------*
           perform   stp-lds-fvp-000      thru stp-lds-fvp-999        .
       prn-fin-lr1-400.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-500.
      *              *-------------------------------------------------*
      *              * Stampa totale agente                            *
      *              *-------------------------------------------------*
       prn-fin-lr1-520.
      *                  *---------------------------------------------*
      *                  * Dicitura per il totale                      *
      *                  *---------------------------------------------*
           move      43                   to   w-all-str-lun          .
           move      "Totale:"            to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      43                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      014                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-540.
      *                  *---------------------------------------------*
      *                  * Totale provvigioni maturate                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-liv-age-tot-mat    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
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
       prn-liv-det-025.
      *              *-------------------------------------------------*
      *              * Numero di maturazioni trattate per l'agente     *
      *              *-------------------------------------------------*
           add       1                    to   w-liv-age-num-mat      .
       prn-liv-det-050.
      *              *-------------------------------------------------*
      *              * Assestamento preliminare, in aree di comodo,    *
      *              * degli importi relativi al conteggio e alla ma-  *
      *              * turazione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Conteggio                                   *
      *                  *---------------------------------------------*
           move      srt-gpc-tip-ctg      to   w-ass-ctg-tip-ctg      .
           move      srt-gpc-ibl-pvg      to   w-ass-ctg-ibl-pvg      .
           move      srt-gpc-amm-pvg      to   w-ass-ctg-amm-pvg      .
           move      srt-gpc-imp-doc      to   w-ass-ctg-imp-doc      .
           move      srt-gpc-imp-agf      to   w-ass-ctg-imp-agf      .
           perform   ass-age-ctg-000      thru ass-age-ctg-999        .
      *                  *---------------------------------------------*
      *                  * Maturazione                                 *
      *                  *---------------------------------------------*
           move      srt-gpc-tip-ctg      to   w-ass-mtz-tip-ctg      .
           move      srt-gpm-tip-mtz      to   w-ass-mtz-tip-mtz      .
           move      srt-gpm-imp-mtz      to   w-ass-mtz-imp-mtz      .
           move      srt-gpm-imp-inc      to   w-ass-mtz-imp-inc      .
           perform   ass-age-mtz-000      thru ass-age-mtz-999        .
       prn-liv-det-075.
      *              *-------------------------------------------------*
      *              * Accumulo importo maturato su totali:            *
      *              *  - Totale agente                                *
      *              *  - Totale generale                              *
      *              * solo se l'importo e' influente sui totali       *
      *              *-------------------------------------------------*
           if        w-ass-mtz-inf-som    =    "S"
                     add   w-ass-mtz-imp-mtz
                                          to   w-liv-age-tot-mat
                     add   w-ass-mtz-imp-mtz
                                          to   w-liv-gen-tot-mat      .
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Preparazione righe da stampare gia' editate     *
      *              *-------------------------------------------------*
       prn-liv-det-150.
      *                  *---------------------------------------------*
      *                  * Inizializzazione area di stampa completa    *
      *                  * che compone il dettaglio                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-det-ads          .
       prn-liv-det-200.
      *                  *---------------------------------------------*
      *                  * Colonna 1                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore per le righe *
      *                      * effettive presenti nella colonna 1      *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-det-cco-001      .
       prn-liv-det-210.
      *                      *-----------------------------------------*
      *                      * Numero conteggio, editato tra parentesi *
      *                      * tonde o quadre, a seconda se a fronte   *
      *                      * incasso oppure no                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento contatore per la se-  *
      *                          * parazione dalla riga precedente     *
      *                          *-------------------------------------*
           add       1                    to   w-stp-det-cco-001      .
      *                          *-------------------------------------*
      *                          * Assemblaggio                        *
      *                          *-------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           if        srt-gpc-tip-mat      =    02
                     move  "["            to   w-all-str-cat (01)
           else      move  "("            to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      9                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      srt-gpc-num-ctg      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (02)     .
      *
           if        srt-gpc-tip-mat      =    02
                     move  "]"            to   w-all-str-cat (03)
           else      move  ")"            to   w-all-str-cat (03)     .
      *
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                          *-------------------------------------*
      *                          * Allineamento a sinistra             *
      *                          *-------------------------------------*
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   w-stp-det-col-001
                                              (w-stp-det-cco-001)     .
       prn-liv-det-220.
      *                      *-----------------------------------------*
      *                      * Codice del cliente concatenato even-    *
      *                      * tualmente con il codice dipendenza      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se codice a zero : no stampa        *
      *                          *-------------------------------------*
           if        srt-gpc-cod-cli      =    zero
                     go to prn-liv-det-250.
      *                          *-------------------------------------*
      *                          * Incremento contatore colonna 001    *
      *                          *-------------------------------------*
           add       1                    to   w-stp-det-cco-001      .
      *                          *-------------------------------------*
      *                          * Editing codice del cliente          *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      7                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      srt-gpc-cod-cli      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           move      11                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      p-edt                to   w-all-str-cat (01)     .
           if        srt-gpc-dpz-cli      =    spaces
                     move  spaces         to   w-all-str-cat (02)
           else      move  "-"            to   w-all-str-cat (02)     .
           if        srt-gpc-cod-cli      >    999999 and
                     srt-gpc-dpz-cli (4:1)
                                          not  = spaces
                     move  spaces         to   w-all-str-cat (02)     .
           move      srt-gpc-dpz-cli      to   w-all-str-cat (03)     .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                          *-------------------------------------*
      *                          * Allineamento a destra               *
      *                          *-------------------------------------*
           perform   all-str-adx-000      thru all-str-adx-999        .
      *                          *-------------------------------------*
      *                          * In area di stampa                   *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-001
                                              (w-stp-det-cco-001)     .
      *                      *-----------------------------------------*
      *                      * Fine colonna 1                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-250.
       prn-liv-det-250.
      *                  *---------------------------------------------*
      *                  * Colonna 2                                   *
      *                  *---------------------------------------------*
       prn-liv-det-260.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore per le righe *
      *                      * effettive presenti nella colonna 2      *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-det-cco-002      .
       prn-liv-det-270.
      *                      *-----------------------------------------*
      *                      * Riferimenti alla fattura                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento contatore per la se-  *
      *                          * parazione dalla riga precedente     *
      *                          *-------------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                          *-------------------------------------*
      *                          * Editing e concatenazione            *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      06                   to   w-all-str-num          .
      *
           if        srt-gpc-tip-ctg      =    01
                     move  "FT"           to   w-all-str-cat (01)
           else if   srt-gpc-tip-ctg      =    02
                     move  "ND"           to   w-all-str-cat (01)
           else if   srt-gpc-tip-ctg      =    03
                     move  "NC"           to   w-all-str-cat (01)
           else if   srt-gpc-tip-ctg      =    11
                     move  "F-"           to   w-all-str-cat (01)
           else      move  "Documento"    to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      srt-gpc-dat-doc      to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-edt                =    spaces
                     move  spaces         to   w-all-str-cat (02)
                     move  spaces         to   w-all-str-cat (03)
           else      move  "del"          to   w-all-str-cat (02)
                     move  p-edt          to   w-all-str-cat (03)     .
      *
           if        srt-gpc-num-doc      =    spaces
                     move  spaces         to   w-all-str-cat (04)
                     move  spaces         to   w-all-str-cat (05)
           else      move  "nr."          to   w-all-str-cat (04)
                     move  srt-gpc-num-doc
                                          to   w-all-str-cat (05)     .
      *
           if        srt-gpc-tip-vpa      =    01
                     move  spaces         to   w-all-str-cat (06)
           else if   srt-gpc-tip-vpa      =    02
                     move  "Indiretta"    to   w-all-str-cat (06)
           else      move  spaces         to   w-all-str-cat (06)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * In campo di destinazione            *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-280.
      *                      *-----------------------------------------*
      *                      * Cliente                                 *
      *                      *-----------------------------------------*
       prn-liv-det-281.
      *                          *-------------------------------------*
      *                          * Se codice a zero : no stampa        *
      *                          *-------------------------------------*
           if        srt-gpc-cod-cli      =    zero
                     go to prn-liv-det-290.
       prn-liv-det-282.
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * Editing e concatenazione per il *
      *                              * caso di cliente non esistente   *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           move      "Cliente codice:"    to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      7                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      srt-gpc-cod-cli      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (02)     .
      *
           move      srt-gpc-dpz-cli      to   w-all-str-cat (03)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           if        srt-cli-rag-cli      =    spaces
                     move  w-all-str-alf  to   w-stp-det-col-002
                                              (w-stp-det-cco-002)
           else      move  srt-cli-rag-cli
                                          to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-283.
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se da stampare solo la ragione  *
      *                              * sociale : no stampa             *
      *                              *---------------------------------*
           if        w-prs-age-cli-trs    =    "R"
                     go to prn-liv-det-284.
      *                              *---------------------------------*
      *                              * Se a spaces : no stampa         *
      *                              *---------------------------------*
           if        srt-cli-via-cli      =    spaces
                     go to prn-liv-det-284.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-cli-via-cli      to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-284.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se da stampare solo la ragione  *
      *                              * sociale : no stampa             *
      *                              *---------------------------------*
           if        w-prs-age-cli-trs    =    "R"
                     go to prn-liv-det-290.
      *                              *---------------------------------*
      *                              * Se a spaces : no stampa         *
      *                              *---------------------------------*
           if        srt-cli-loc-cli      =    spaces
                     go to prn-liv-det-290.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-cli-loc-cli      to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-290.
      *                      *-----------------------------------------*
      *                      * Cliente per la fatturazione             *
      *                      *-----------------------------------------*
       prn-liv-det-291.
      *                          *-------------------------------------*
      *                          * Se pari al cliente commerciale : no *
      *                          * stampa                              *
      *                          *-------------------------------------*
           if        srt-gpc-cod-plf      =    srt-gpc-cod-cli and
                     srt-gpc-dpz-plf      =    srt-gpc-dpz-cli
                     go to prn-liv-det-300.
       prn-liv-det-292.
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * Editing e concatenazione per il *
      *                              * caso di cliente non esistente   *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
      *
           move      "Cliente di fattura codice:"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      7                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      srt-gpc-cod-plf      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (02)     .
      *
           move      srt-gpc-dpz-plf      to   w-all-str-cat (03)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           if        srt-plf-rag-plf      =    spaces
                     move  w-all-str-alf  to   w-stp-det-col-002
                                              (w-stp-det-cco-002)
           else      move  srt-plf-rag-plf
                                          to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-293.
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se da stampare solo la ragione  *
      *                              * sociale : no stampa             *
      *                              *---------------------------------*
           if        w-prs-age-cli-trs    =    "R"
                     go to prn-liv-det-294.
      *                              *---------------------------------*
      *                              * Se a spaces : no stampa         *
      *                              *---------------------------------*
           if        srt-plf-via-plf      =    spaces
                     go to prn-liv-det-294.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-plf-via-plf      to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-294.
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se da stampare solo la ragione  *
      *                              * sociale : no stampa             *
      *                              *---------------------------------*
           if        w-prs-age-cli-trs    =    "R"
                     go to prn-liv-det-300.
      *                              *---------------------------------*
      *                              * Se a spaces : no stampa         *
      *                              *---------------------------------*
           if        srt-plf-loc-plf      =    spaces
                     go to prn-liv-det-300.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-plf-loc-plf      to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-300.
      *                      *-----------------------------------------*
      *                      * Subagente                               *
      *                      *-----------------------------------------*
       prn-liv-det-301.
      *                          *-------------------------------------*
      *                          * Se codice a zero : no stampa        *
      *                          *-------------------------------------*
           if        srt-gpc-cod-ags      =    zero
                     go to prn-liv-det-310.
       prn-liv-det-302.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * Editing e concatenazione        *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           move      "Subagente:"         to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      7                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      srt-gpc-cod-ags      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           if        srt-ags-nom-ags      =    spaces
                     move  p-edt          to   w-all-str-cat (02)
           else      move  srt-ags-nom-ags
                                          to   w-all-str-cat (02)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-310.
      *                      *-----------------------------------------*
      *                      * Annotazioni                             *
      *                      *-----------------------------------------*
       prn-liv-det-311.
      *                          *-------------------------------------*
      *                          * Se a spaces : no stampa             *
      *                          *-------------------------------------*
           if        srt-gpc-not-ctg      =    spaces
                     go to prn-liv-det-320.
       prn-liv-det-312.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-gpc-not-ctg      to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-320.
      *                      *-----------------------------------------*
      *                      * Segnale di maturazione bloccata         *
      *                      *-----------------------------------------*
       prn-liv-det-321.
      *                          *-------------------------------------*
      *                          * Se non bloccata : no stampa         *
      *                          *-------------------------------------*
           if        srt-gpc-mat-blo      not  = 02
                     go to prn-liv-det-330.
       prn-liv-det-322.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      "** Provvigione bloccata                 "
                                          to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-330.
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima              *
      *                      *-----------------------------------------*
       prn-liv-det-331.
      *                          *-------------------------------------*
      *                          * Se data di maturazione a zero o pa- *
      *                          * ri alla data documento : no stampa  *
      *                          *-------------------------------------*
           if        srt-gpc-ddm-min      =    zero         or
                     srt-gpc-ddm-min      =    srt-gpc-dat-doc
                     go to prn-liv-det-340.
       prn-liv-det-332.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-002      .
      *                              *---------------------------------*
      *                              * Editing e concatenazione        *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           move      "** Data di maturazione minima:"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      srt-gpc-ddm-min      to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      p-edt                to   w-all-str-cat (02)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-002
                                              (w-stp-det-cco-002)     .
       prn-liv-det-340.
      *                      *-----------------------------------------*
      *                      * Fine colonna 2                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-400.
       prn-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Colonna 3                                   *
      *                  *---------------------------------------------*
       prn-liv-det-410.
      *                      *-----------------------------------------*
      *                      * Provvigione conteggiata, assestata come *
      *                      * segno algebrico                         *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-ass-ctg-amm-pvg    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-det-col-003 (1)  .
       prn-liv-det-450.
      *                  *---------------------------------------------*
      *                  * Colonna 4                                   *
      *                  *---------------------------------------------*
       prn-liv-det-460.
      *                      *-----------------------------------------*
      *                      * Provvigione maturata, assestata come    *
      *                      * segno algebrico                         *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-ass-mtz-imp-mtz    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-det-col-004 (1)  .
       prn-liv-det-500.
      *                  *---------------------------------------------*
      *                  * Colonna 5                                   *
      *                  *---------------------------------------------*
       prn-liv-det-510.
      *                      *-----------------------------------------*
      *                      * Inizializzazione contatore per le righe *
      *                      * effettive presenti nella colonna 5      *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-det-cco-005      .
       prn-liv-det-520.
      *                      *-----------------------------------------*
      *                      * Riferimenti alla maturazione            *
      *                      *-----------------------------------------*
       prn-liv-det-521.
      *                          *-------------------------------------*
      *                          * Se numero maturazione a zero : no   *
      *                          * stampa                              *
      *                          *-------------------------------------*
           if        srt-gpm-num-mtz      =    zero
                     go to prn-liv-det-530.
       prn-liv-det-522.
      *                          *-------------------------------------*
      *                          * Aggiornamento contatore per la se-  *
      *                          * parazione dalla riga precedente     *
      *                          *-------------------------------------*
           add       1                    to   w-stp-det-cco-005      .
      *                          *-------------------------------------*
      *                          * Editing e concatenazione            *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
      *
           move      "Maturazione"        to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      srt-gpm-dat-mtz      to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-edt                =    spaces
                     move  spaces         to   w-all-str-cat (02)
                     move  spaces         to   w-all-str-cat (03)
           else      move  "del"          to   w-all-str-cat (02)
                     move  p-edt          to   w-all-str-cat (03)     .
      *
           move      "nr."                to   w-all-str-cat (04)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      9                    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      spaces               to   p-edm                  .
           move      srt-gpm-num-mtz      to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (05)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * In campo di destinazione            *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-005
                                              (w-stp-det-cco-005)     .
       prn-liv-det-530.
      *                      *-----------------------------------------*
      *                      * Importo incassato                       *
      *                      *-----------------------------------------*
       prn-liv-det-531.
      *                          *-------------------------------------*
      *                          * Se la maturazione non e' a fronte   *
      *                          * di un incasso : no stampa           *
      *                          *-------------------------------------*
           if        w-ass-mtz-tip-mtz    not  = 02
                     go to prn-liv-det-540.
      *                          *-------------------------------------*
      *                          * Se a zero : no stampa               *
      *                          *-------------------------------------*
           if        w-ass-mtz-imp-inc    =    zero
                     go to prn-liv-det-540.
       prn-liv-det-532.
      *                          *-------------------------------------*
      *                          * Aggiornamento contatore per la se-  *
      *                          * parazione dalla riga precedente     *
      *                          *-------------------------------------*
           add       1                    to   w-stp-det-cco-005      .
      *                          *-------------------------------------*
      *                          * Editing e concatenazione            *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           move      "Importo incassato :"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-ass-mtz-imp-inc    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (02)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * In campo di destinazione            *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-005
                                              (w-stp-det-cco-005)     .
       prn-liv-det-540.
      *                      *-----------------------------------------*
      *                      * Annotazioni                             *
      *                      *-----------------------------------------*
       prn-liv-det-541.
      *                          *-------------------------------------*
      *                          * Se a spaces : no stampa             *
      *                          *-------------------------------------*
           if        srt-gpm-not-mtz      =    spaces
                     go to prn-liv-det-550.
       prn-liv-det-542.
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-005      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      srt-gpm-not-mtz      to   w-stp-det-col-005
                                              (w-stp-det-cco-005)     .
       prn-liv-det-550.
      *                      *-----------------------------------------*
      *                      * Castelletto scadenze, se previsto da    *
      *                      * personalizzazione                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su personalizzazione           *
      *                          *-------------------------------------*
           if        w-prs-dat-mtz        not  = "X"
                     go to prn-liv-det-590.
      *                          *-------------------------------------*
      *                          * Test su protocollo documento        *
      *                          *-------------------------------------*
           if        srt-gpc-prt-fcl      =    zero
                     go to prn-liv-det-590.
      *                          *-------------------------------------*
      *                          * Start su archivio scadenze emesse a *
      *                          * fronte del movimento di fatture     *
      *                          * clienti                             *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PFCNRS    "         to   f-key                  .
           move      srt-gpc-prt-fcl      to   rf-sdb-prt-fcl         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se errore di Start : oltre      *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-590.
       prn-liv-det-552.
      *                          *-------------------------------------*
      *                          * Next su archivio scadenze           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se fine file : oltre            *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-590.
       prn-liv-det-553.
      *                          *-------------------------------------*
      *                          * Test max su archivio scadenze       *
      *                          *-------------------------------------*
           if        rf-sdb-prt-fcl       not  = srt-gpc-prt-fcl
                     go to prn-liv-det-590.
       prn-liv-det-554.
      *                          *-------------------------------------*
      *                          * Assemblaggio elementi               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Preparazione tipo scadenza      *
      *                              *---------------------------------*
           if        rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-dlc       =    "S"
                     move  "DC  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-cts       =    "S"
                     move  "CT  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    01    and
                     rf-sdb-snx-dlc       not  = "S" and
                     rf-sdb-snx-cts       not  = "S"
                     move  "RD  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    02
                     move  "IE  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    03
                     move  "RIBA"         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    04
                     move  "CDO "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    05
                     move  "MAV "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    06
                     move  "RD  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    07
                     move  "BB  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    08
                     move  "CCP "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    09
                     move  "RB  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    10
                     move  "TR  "         to   w-all-str-cat (02)
           else if   rf-sdb-tip-sdb       =    11
                     move  "PC  "         to   w-all-str-cat (02)
           else      move  "??  "         to   w-all-str-cat (02)     .
      *                              *---------------------------------*
      *                              * Editing data scadenza           *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      "<B"                 to   p-edm                  .
           move      rf-sdb-dts-sdb       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (04)     .
      *                              *---------------------------------*
      *                              * Editing importo scadenza        *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      rf-sdb-imp-sdb       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (06)     .
      *                              *---------------------------------*
      *                              * Composizione stringa            *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "["                  to   w-all-str-cat (01)     .
           move      "al"                 to   w-all-str-cat (03)     .
           move      "-"                  to   w-all-str-cat (05)     .
           move      "]"                  to   w-all-str-cat (07)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * Aggiornamento colonna               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-005      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-005
                                              (w-stp-det-cco-005)     .
       prn-liv-det-570.
      *                          *-------------------------------------*
      *                          * Eventuale riga aggiuntiva con dati  *
      *                          * rilevati dallo status scadenza      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Status scadenza                 *
      *                              *---------------------------------*
           move      02                   to   w-sts-sdb-tip-det      .
           move      9991231              to   w-sts-sdb-dat-rif      .
           perform   det-sts-sdb-000      thru det-sts-sdb-999        .
           if        w-sts-sdb-ult-ope    =    000
                     go to prn-liv-det-580.
      *                              *---------------------------------*
      *                              * Editing ultima operazione su    *
      *                              * scadenza                        *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      w-sts-sdb-ult-ope    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                              *---------------------------------*
      *                              * Decodifica ultima operazione su *
      *                              * scadenza                        *
      *                              *---------------------------------*
           if        w-sts-sdb-ult-ope    =    000
                     move  "(n.d.)"       to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    100
                     move  "Emessa"       to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    200
                     move  "Stornata"     to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    300
                     move  "Riscossa"     to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    320
                     move  "Pagata"       to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    350
                     move  "Compensata"   to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    400
                     move  "In distinta"  to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    500
                     move  "Presentata"   to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    540
                     move  "Accettata"    to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    560
                     move  "Accreditata"  to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    600
                     move  "INSOLUTA !"   to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    620
                     move  "Richiamata"   to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    700
                     move  "Accreditata"  to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    720
                     move  "Not. buon es."
                                          to   w-all-str-cat (02)
           else if   w-sts-sdb-ult-ope    =    740
                     move  "Pres. buon es."
                                          to   w-all-str-cat (02)
           else      move  p-edt          to   w-all-str-cat (02)     .
      *                              *---------------------------------*
      *                              * Editing data rilevazione status *
      *                              * scadenza                        *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      "<B"                 to   p-edm                  .
           move      w-sts-sdb-dat-ril    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (04)     .
      *                              *---------------------------------*
      *                              * Composizione stringa            *
      *                              *---------------------------------*
           move      40                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "["                  to   w-all-str-cat (01)     .
           move      "-"                  to   w-all-str-cat (03)     .
           move      "]"                  to   w-all-str-cat (05)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * Aggiornamento colonna               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Aggiornamento contatore per la  *
      *                              * separazione dalla riga prece-   *
      *                              * dente                           *
      *                              *---------------------------------*
           add       1                    to   w-stp-det-cco-005      .
      *                              *---------------------------------*
      *                              * In campo di destinazione        *
      *                              *---------------------------------*
           move      w-all-str-alf        to   w-stp-det-col-005
                                              (w-stp-det-cco-005)     .
       prn-liv-det-580.
      *                          *-------------------------------------*
      *                          * Riciclo su scadenza successiva      *
      *                          *-------------------------------------*
           go to     prn-liv-det-552.
       prn-liv-det-590.
      *                      *-----------------------------------------*
      *                      * Fine colonna 5                          *
      *                      *-----------------------------------------*
           go to     prn-liv-det-700.
       prn-liv-det-700.
      *              *-------------------------------------------------*
      *              * Stampa delle righe editate                      *
      *              *-------------------------------------------------*
       prn-liv-det-710.
      *                  *---------------------------------------------*
      *                  * Determinazione del numero di righe effetti- *
      *                  * ve da stampare                              *
      *                  *---------------------------------------------*
       prn-liv-det-711.
           move      20                   to   w-stp-det-ctr          .
       prn-liv-det-712.
           if        w-stp-det-rig
                    (w-stp-det-ctr)       not  = spaces
                     go to prn-liv-det-720.
           subtract  1                    from w-stp-det-ctr          .
           if        w-stp-det-ctr        >    zero
                     go to prn-liv-det-712.
       prn-liv-det-720.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice per la scansione    *
      *                  * delle righe effettive da stampare           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-det-inx          .
       prn-liv-det-730.
      *                  *---------------------------------------------*
      *                  * Incremento indice per la scansione delle    *
      *                  * righe effettive da stampare                 *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-det-inx          .
      *                  *---------------------------------------------*
      *                  * Se oltre il numero di righe di effettive da *
      *                  * stampare : ad uscita                        *
      *                  *---------------------------------------------*
           if        w-stp-det-inx        >    w-stp-det-ctr
                     go to prn-liv-det-900.
       prn-liv-det-740.
      *                  *---------------------------------------------*
      *                  * Test se linee di stampa residue sufficienti *
      *                  *---------------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-745.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *                  *---------------------------------------------*
      *                  * Se flag di interruzione forzata: uscita     *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
      *                  *---------------------------------------------*
      *                  * Richiamo della subroutine per la stampa dei *
      *                  * titoli relativi alla fincatura verticale    *
      *                  * della pagina                                *
      *                  *---------------------------------------------*
           perform   stp-tfv-vrt-000      thru stp-tfv-vrt-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo della subroutine per la stampa     *
      *                  * della linea di sottolineatura relativa alla *
      *                  * fincatura verticale della pagina            *
      *                  *---------------------------------------------*
           perform   stp-lds-fvp-000      thru stp-lds-fvp-999        .
      *                  *---------------------------------------------*
      *                  * A stampa interlinea iniziale                *
      *                  *---------------------------------------------*
           go to     prn-liv-det-746.
       prn-liv-det-745.
      *                  *---------------------------------------------*
      *                  * Test se si tratta della prima riga          *
      *                  *---------------------------------------------*
           if        w-stp-det-inx        >    1
                     go to prn-liv-det-750.
       prn-liv-det-746.
      *                  *---------------------------------------------*
      *                  * Interlinea iniziale di separazione          *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-750.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-760.
      *                  *---------------------------------------------*
      *                  * Stampa delle colonne che compongono la sin- *
      *                  * gola riga di dettaglio                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Colonna 1                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-det-col-001
                    (w-stp-det-inx)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Colonna 2                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           if        w-stp-det-inx        =    01
                     move  014            to   p-pos
           else      move  017            to   p-pos                  .
           move      w-stp-det-col-002
                    (w-stp-det-inx)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Colonna 3                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      059                  to   p-pos                  .
           move      w-stp-det-col-003
                    (w-stp-det-inx)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Colonna 4                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      076                  to   p-pos                  .
           move      w-stp-det-col-004
                    (w-stp-det-inx)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Colonna 5                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      093                  to   p-pos                  .
           move      w-stp-det-col-005
                    (w-stp-det-inx)       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-770.
      *                  *---------------------------------------------*
      *                  * Riciclo a riga effettiva da stampare suc-   *
      *                  * cessiva                                     *
      *                  *---------------------------------------------*
           go to     prn-liv-det-730.
       prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'intestazione della pagina                *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio assoluto                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' l'inizio in assoluto : nessuna a- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-stp-int-num-int    >    zero
                     go to stp-int-pag-100.
      *                  *---------------------------------------------*
      *                  * Preparazione del titolo stampato, allineato *
      *                  * a sinistra                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing data massima dei documenti re-  *
      *                      * lativi ai conteggi provvigionali su cui *
      *                      * eseguire le nuove maturazioni           *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-dat-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt0      .
      *                      *-----------------------------------------*
      *                      * Preparazione del titolo completo        *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-int-tit-stp      .
           string    "GENERAZIONE AUTOMATICA PROVVIGIONI MATURATE A FRON
      -              "TE DOCUMENTI FINO AL "
                                delimited by   size
                     w-stp-int-wed-dt0
                                delimited by   size
                                          into w-stp-int-tit-stp      .
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
      *                  *---------------------------------------------*
      *                  * Preparazione del sub-titolo                 *
      *                  *---------------------------------------------*
           move      "Simulazione"        to   w-stp-int-sbt-stp      .
      *                  *---------------------------------------------*
      *                  * Determinazione della lunghezza del sub-ti-  *
      *                  * tolo dello stampato allineato a sinistra,   *
      *                  * senza considerare gli spazi in coda         *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-int-wct-c01      .
           inspect   w-stp-int-sbt-stp
                                      tallying w-stp-int-wct-c01
                                  for trailing spaces                 .
           move      80                   to   w-stp-int-sbt-stl      .
           subtract  w-stp-int-wct-c01    from w-stp-int-sbt-stl      .
      *                  *---------------------------------------------*
      *                  * Determinazione della posizione di stampa    *
      *                  * per il sub-titolo dello stampato            *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-int-sbt-stc      .
           subtract  w-stp-int-sbt-stl    from w-stp-int-sbt-stc      .
           divide    2                    into w-stp-int-sbt-stc      .
           add       1                    to   w-stp-int-sbt-stc      .
       stp-int-pag-100.
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
       stp-int-pag-200.
      *              *-------------------------------------------------*
      *              * Esecuzione intestazione pagina                  *
      *              *-------------------------------------------------*
       stp-int-pag-250.
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
                     go to stp-int-pag-999.
       stp-int-pag-300.
      *                  *---------------------------------------------*
      *                  * Intestazione vera e propria                 *
      *                  *---------------------------------------------*
       stp-int-pag-325.
      *                      *-----------------------------------------*
      *                      * Linea di '=' iniziale                   *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-350.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-375.
      *                      *-----------------------------------------*
      *                      * Ragione sociale azienda                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-int-rag-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal 'Data :'                        *
      *                      *-----------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Data di stampa                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      p-sel-als-sel        to   p-pos                  .
           subtract  18                   from p-pos                  .
           add       w-stp-int-ncs-dep    to   p-pos                  .
           move      w-stp-int-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Literal 'Pag.'                          *
      *                      *-----------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Numero pagina                           *
      *                      *-----------------------------------------*
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
      *                      *-----------------------------------------*
      *                      * Se stampa su una linea si passa diret-  *
      *                      * tamente a stampare il titolo centrale   *
      *                      *-----------------------------------------*
           if        w-stp-int-tit-uod    =    "U"
                     go to stp-int-pag-425.
       stp-int-pag-400.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-425.
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
       stp-int-pag-450.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-475.
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
       stp-int-pag-600.
      *              *-------------------------------------------------*
      *              * Esecuzione sub-intestazione pagina              *
      *              *-------------------------------------------------*
       stp-int-pag-625.
      *                  *---------------------------------------------*
      *                  * Se il numero pagina da stampare non e' pari *
      *                  * a 1 : no sub-intestazione                   *
      *                  *---------------------------------------------*
           if        w-stp-int-num-pag    not  = 1
                     go to stp-int-pag-900.
      *                  *---------------------------------------------*
      *                  * Se sub-titolo a spaces : no sub-intestazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-stp-int-sbt-stp    =    spaces
                     go to stp-int-pag-900.
      *                  *---------------------------------------------*
      *                  * Se non si e' in simulazione : no sub-inte-  *
      *                  * stazione                                    *
      *                  *---------------------------------------------*
           if        rr-tip-exe           not  = 01
                     go to stp-int-pag-900.
       stp-int-pag-650.
      *                  *---------------------------------------------*
      *                  * Sub-intestazione                            *
      *                  *---------------------------------------------*
       stp-int-pag-675.
      *                      *-----------------------------------------*
      *                      * Posizionamento verticale per lasciare   *
      *                      * una interlinea di separazione           *
      *                      *-----------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-700.
      *                      *-----------------------------------------*
      *                      * Sub-titolo centrale                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-int-sbt-stl    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-int-sbt-stc    to   p-pos                  .
           move      w-stp-int-sbt-stp    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-pag-900.
      *              *-------------------------------------------------*
      *              * Post-intestazione                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero totale di intestazioni e- *
      *                  * seguite                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-int-num-int      .
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'intestazione agente                      *
      *    *                                                           *
      *    * - Parametri in input                                      *
      *    *                                                           *
      *    *   - w-stp-age-cod-num : Valore codice, se numerico        *
      *    *                                                           *
      *    *   - w-stp-age-cod-alf : Valore codice, se alfanumerico    *
      *    *                                                           *
      *    *   - w-stp-age-cod-des : Descrizione per il codice         *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       stp-int-age-000.
      *              *-------------------------------------------------*
      *              * Preliminari di inizio assoluto                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' l'inizio in assoluto : nessuna a- *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-stp-age-num-int    >    zero
                     go to stp-int-age-200.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di intestazione      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione Literal                     *
      *                      *-----------------------------------------*
           move      "Agente :                      "
                                          to   w-stp-age-lit-int      .
      *                      *-----------------------------------------*
      *                      * Tipo di codice per sub-intestazione     *
      *                      *-----------------------------------------*
           if        w-prs-age-age-tco    =    "M"
                     move  "A"            to   w-stp-age-tip-cod
           else      move  "N"            to   w-stp-age-tip-cod      .
       stp-int-age-030.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area di intesta-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-age-ads-asx      .
           move      zero                 to   w-stp-age-ads-lng      .
           inspect   w-stp-age-ads-asx
                                      tallying w-stp-age-ads-lng
                                          for  all   spaces           .
       stp-int-age-040.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione di stampa iniziale *
      *                  * per l'area di sub-intestazione              *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-stp-age-pos-dsi      .
           subtract  w-stp-age-ads-lng    from w-stp-age-pos-dsi      .
           divide    2                    into w-stp-age-pos-dsi      .
           add       1                    to   w-stp-age-pos-dsi      .
       stp-int-age-200.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale per lasciare una in-   *
      *              * terlinea di separazione                         *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-age-300.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-age-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-age-pos-dsi    to   p-pos                  .
           move      all   "="            to   w-stp-age-ads-sub      .
           move      w-stp-age-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-age-400.
      *              *-------------------------------------------------*
      *              * Linea centrale di intestazione agente           *
      *              *-------------------------------------------------*
       stp-int-age-410.
      *                  *---------------------------------------------*
      *                  * Se stampa relativa al totale generale si    *
      *                  * prepara la dicitura per il totale e si o-   *
      *                  * mette l'editing complesso                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-stp-age-tip-int    not  = "T"
                     go to stp-int-age-420.
      *                      *-----------------------------------------*
      *                      * Literal fisso                           *
      *                      *-----------------------------------------*
           move      "TOTALE GENERALE                                   
      -              "                      "
                                          to   w-stp-age-ads-asx      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-int-age-500.
       stp-int-age-420.
      *                  *---------------------------------------------*
      *                  * Se codice numerico a zero e codice alfanu-  *
      *                  * merico a spaces si prepara la dicitura di   *
      *                  * non classificazione e si omette l'editing   *
      *                  * complesso                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-stp-age-cod-num    not  = zero   or
                     w-stp-age-cod-alf    not  = spaces
                     go to stp-int-age-450.
      *                      *-----------------------------------------*
      *                      * Literal fisso                           *
      *                      *-----------------------------------------*
           move      "Conteggi provvigionali non attribuiti ad alcun age
      -              "nte                   "
                                          to   w-stp-age-ads-asx      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     stp-int-age-500.
       stp-int-age-450.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di intestazione, alli- *
      *                  * neata a sinistra                            *
      *                  *---------------------------------------------*
       stp-int-age-455.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           move      w-stp-age-lit-int    to   w-stp-age-ads-asx      .
      *                          *-------------------------------------*
      *                          * Primo non-blank utile in coda       *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-age-wct-c02      .
           inspect   w-stp-age-ads-asx
                                      tallying w-stp-age-wct-c02
                                  for trailing spaces                 .
           move      w-stp-age-ads-lng    to   w-stp-age-wct-c01      .
           subtract  w-stp-age-wct-c02    from w-stp-age-wct-c01      .
           add       1                    to   w-stp-age-wct-c01      .
       stp-int-age-460.
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
       stp-int-age-465.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo di  *
      *                          * codice                              *
      *                          *-------------------------------------*
           if        w-stp-age-tip-cod    =    "A"
                     go to stp-int-age-475.
       stp-int-age-470.
      *                          *-------------------------------------*
      *                          * Se tipo di codice : numerico        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing                         *
      *                              *---------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-age-cod-num    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-age-cod-ned      .
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           add       1                    to   w-stp-age-wct-c01      .
           move      w-stp-age-cod-ned    to   w-stp-age-ads-asx
                                              (w-stp-age-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-age-wct-c02      .
           inspect   w-stp-age-ads-asx
                                      tallying w-stp-age-wct-c02
                                  for trailing spaces                 .
           move      w-stp-age-ads-lng    to   w-stp-age-wct-c01      .
           subtract  w-stp-age-wct-c02    from w-stp-age-wct-c01      .
           add       1                    to   w-stp-age-wct-c01      .
      *                              *---------------------------------*
      *                              * A trattamento descrizione       *
      *                              *---------------------------------*
           go to     stp-int-age-480.
       stp-int-age-475.
      *                          *-------------------------------------*
      *                          * Se tipo di codice : alfanumerico    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se valore a spaces : a tratta-  *
      *                              * mento per codice numerico       *
      *                              *---------------------------------*
           if        w-stp-age-cod-alf    =    spaces
                     go to stp-int-age-470.
      *                              *---------------------------------*
      *                              * Concatenamento                  *
      *                              *---------------------------------*
           if        w-stp-age-tip-cod    not  = "+"
                     add   1              to   w-stp-age-wct-c01      .
           move      w-stp-age-cod-alf    to   w-stp-age-ads-asx
                                              (w-stp-age-wct-c01 :)   .
      *                              *---------------------------------*
      *                              * Primo non-blank utile in coda   *
      *                              *---------------------------------*
           move      zero                 to   w-stp-age-wct-c02      .
           inspect   w-stp-age-ads-asx
                                      tallying w-stp-age-wct-c02
                                  for trailing spaces                 .
           move      w-stp-age-ads-lng    to   w-stp-age-wct-c01      .
           subtract  w-stp-age-wct-c02    from w-stp-age-wct-c01      .
           add       1                    to   w-stp-age-wct-c01      .
      *                              *---------------------------------*
      *                              * A trattamento descrizione       *
      *                              *---------------------------------*
           go to     stp-int-age-480.
       stp-int-age-480.
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Concatenamento                      *
      *                          *-------------------------------------*
           add       2                    to   w-stp-age-wct-c01      .
           move      w-stp-age-cod-des    to   w-stp-age-ads-asx
                                              (w-stp-age-wct-c01 :)   .
       stp-int-age-500.
      *                  *---------------------------------------------*
      *                  * Preparazione stringa di intestazione, alli- *
      *                  * neata al centro                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione iniziale area allineata *
      *                      * al centro                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-age-ads-sub      .
      *                      *-----------------------------------------*
      *                      * Determinazione posizione iniziale per   *
      *                      * l'area allineata al centro              *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-age-wct-c01      .
           inspect   w-stp-age-ads-asx
                                      tallying w-stp-age-wct-c01
                                  for trailing spaces                 .
           divide    2                    into w-stp-age-wct-c01      .
           add       1                    to   w-stp-age-wct-c01      .
      *                      *-----------------------------------------*
      *                      * Spostamento area allineata a sinistra   *
      *                      * in area allineata al centro             *
      *                      *-----------------------------------------*
           move      w-stp-age-ads-asx    to   w-stp-age-ads-sub
                                              (w-stp-age-wct-c01 :)   .
       stp-int-age-550.
      *                  *---------------------------------------------*
      *                  * Stampa                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-age-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-age-pos-dsi    to   p-pos                  .
           move      w-stp-age-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-age-600.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-age-700.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-stp-age-ads-lng    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-age-pos-dsi    to   p-pos                  .
           move      all   "-"            to   w-stp-age-ads-sub      .
           move      w-stp-age-ads-sub    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-int-age-900.
      *              *-------------------------------------------------*
      *              * Post-intestazione-agente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero totale sub-intestazioni   *
      *                  * stampate                                    *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-age-num-int      .
       stp-int-age-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per stampa dei titoli relativi alla fincatura  *
      *    * verticale della pagina                                    *
      *    *-----------------------------------------------------------*
       stp-tfv-vrt-000.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale per lasciare una in-   *
      *              * terlinea di separazione                         *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tfv-vrt-100.
      *              *-------------------------------------------------*
      *              * Linea 1                                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      "  Numero                                          
      -              "          Provvigione      Provvigione            
      -              "      Annotazioni               "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tfv-vrt-200.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tfv-vrt-300.
      *              *-------------------------------------------------*
      *              * Linea 2                                         *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      " conteggio           Riferimenti alla fattura     
      -              "          conteggiata        maturata             
      -              "   sulla maturazione            "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tfv-vrt-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la stampa della linea di sottolineatura o  *
      *    * sopralineatura relativi alla fincatura verticale della    *
      *    * pagina                                                    *
      *    *-----------------------------------------------------------*
       stp-lds-fvp-000.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-lds-fvp-100.
      *              *-------------------------------------------------*
      *              * Sottolineatura/sopralineatura                   *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      "-----------  -------------------------------------
      -              "------  ---------------  ---------------  --------
      -              "--------------------------------"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-lds-fvp-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la stampa dei totali generali              *
      *    *-----------------------------------------------------------*
       stp-tot-gen-000.
      *              *-------------------------------------------------*
      *              * Posizionamento verticale per lasciare una in-   *
      *              * terlinea di separazione                         *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      p-lnr                to   p-lin                  .
           add       2                    to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tot-gen-100.
      *              *-------------------------------------------------*
      *              * Stampa del totale generale                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      52                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      01                   to   p-pos                  .
           move      "Totale provvigioni maturate.......................
      -              ".:"                 to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "<G"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      54                   to   p-pos                  .
           move      w-liv-gen-tot-mat    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tot-gen-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Routine principale                                        *
      *    *-----------------------------------------------------------*
       det-nmp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione dei flags di uscita dalla rou- *
      *              * tine                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-flg-det      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del tipo di conteggio di rife- *
      *              * rimento                                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-tip-ctg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione dell'importo incassato che fa' *
      *              * scattare la nuova maturazione, per il caso di   *
      *              * maturazione a fronte incasso                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-imp-inc      .
      *              *-------------------------------------------------*
      *              * Inizializzazione dell'importo determinato della *
      *              * nuova maturazione                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-imp-mtz      .
      *              *-------------------------------------------------*
      *              * Lettura del conteggio provvigionale, con buffe- *
      *              * rizzazione dei valori associati                 *
      *              *-------------------------------------------------*
           perform   det-nmp-lco-000      thru det-nmp-lco-999        .
      *              *-------------------------------------------------*
      *              * Se esito della lettura negativo : uscita con    *
      *              * status di uscita ad errore indicante che la ma- *
      *              * turazione non e' stata eseguita in quanto non   *
      *              * e' piu' accessibile il record relativo al con-  *
      *              * teggio provvigionale                            *
      *              *-------------------------------------------------*
           if        w-det-nmp-sub-flg    not  = zero
                     move  w-det-nmp-sub-flg
                                          to   w-det-nmp-flg-det
                     go to det-nmp-999.
      *              *-------------------------------------------------*
      *              * Lettura delle maturazioni gia' presenti relati- *
      *              * ve al record del conteggio provvigionale, con   *
      *              * totalizzazione dell'importo provvigioni gia'    *
      *              * maturato e calcolo del residuo da maturare      *
      *              *-------------------------------------------------*
           perform   det-nmp-lmt-000      thru det-nmp-lmt-999        .
      *              *-------------------------------------------------*
      *              * Se esito della lettura negativo : uscita con    *
      *              * status di uscita ad errore indicante che non    *
      *              * esiste alcun residuo da maturare                *
      *              *-------------------------------------------------*
           if        w-det-nmp-sub-flg    not  = zero
                     move  w-det-nmp-sub-flg
                                          to   w-det-nmp-flg-det
                     go to det-nmp-999.
      *              *-------------------------------------------------*
      *              * Test se conteggio provvigionale bloccato o con  *
      *              * data di maturazione minima superiore a quella   *
      *              * di riferimento                                  *
      *              *-------------------------------------------------*
           perform   det-nmp-tsb-000      thru det-nmp-tsb-999        .
      *              *-------------------------------------------------*
      *              * Se esito del test negativo : uscita con status  *
      *              * di uscita ad errore indicante la causa di non   *
      *              * maturabilita'                                   *
      *              *-------------------------------------------------*
           if        w-det-nmp-sub-flg    not  = zero
                     move  w-det-nmp-sub-flg
                                          to   w-det-nmp-flg-det
                     go to det-nmp-999.
      *              *-------------------------------------------------*
      *              * Esecuzione dell'effettiva determinazione della  *
      *              * nuova maturazione                               *
      *              *-------------------------------------------------*
           perform   det-nmp-det-000      thru det-nmp-det-999        .
      *              *-------------------------------------------------*
      *              * Uscita con gli status ritornati dalla routine   *
      *              *-------------------------------------------------*
           move      w-det-nmp-sub-flg    to   w-det-nmp-flg-det      .
       det-nmp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Lettura del record relativo al conteggio provvigionale    *
      *    *-----------------------------------------------------------*
       det-nmp-lco-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita da subroutine   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-sub-flg      .
      *              *-------------------------------------------------*
      *              * Se record relativo al conteggio da non leggere  *
      *              * in quanto gia' presente in area record : si o-  *
      *              * mette la lettura                                *
      *              *-------------------------------------------------*
           if        w-det-nmp-let-ctg    =    "N"
                     go to det-nmp-lco-300.
       det-nmp-lco-100.
      *              *-------------------------------------------------*
      *              * Lettura del record relativo al conteggio        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMCTG    "         to   f-key                  .
           move      w-det-nmp-num-ctg    to   rf-gpc-num-ctg         .
           move      "pgm/age/fls/ioc/obj/iofgpc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpc                 .
      *              *-------------------------------------------------*
      *              * Se record relativo al conteggio non esistente   *
      *              * si va' al trattamento dell'errore in lettura,   *
      *              * altrimenti al trattamento per record in area    *
      *              * record                                          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-lco-200
           else      go to det-nmp-lco-300.
       det-nmp-lco-200.
      *              *-------------------------------------------------*
      *              * Trattamento dell'errore in lettura              *
      *              *-------------------------------------------------*
       det-nmp-lco-210.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : record non esistente     *
      *                  *---------------------------------------------*
           move      51                   to   w-det-nmp-sub-flg      .
       det-nmp-lco-220.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-nmp-lco-999.
       det-nmp-lco-300.
      *              *-------------------------------------------------*
      *              * Trattamento del record                          *
      *              *-------------------------------------------------*
       det-nmp-lco-310.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori dal record           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero protocollo movimento di fatture  *
      *                      * clienti                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-prt-fcl       to   w-det-nmp-gpc-pfc      .
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   w-det-nmp-gpc-tdc      .
      *                      *-----------------------------------------*
      *                      * Data documento cui si riferisce il con- *
      *                      * teggio                                  *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   w-det-nmp-gpc-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento cui si riferisce il    *
      *                      * conteggio                               *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   w-det-nmp-gpc-ndo      .
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   w-det-nmp-gpc-age      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   w-det-nmp-gpc-cli      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente           *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-cli       to   w-det-nmp-gpc-dcl      .
      *                      *-----------------------------------------*
      *                      * Imponibile provvigionale                *
      *                      *-----------------------------------------*
           move      rf-gpc-ibl-pvg       to   w-det-nmp-gpc-ibl      .
      *                      *-----------------------------------------*
      *                      * % di provvigione                        *
      *                      *-----------------------------------------*
           move      rf-gpc-per-pvg       to   w-det-nmp-gpc-per      .
      *                      *-----------------------------------------*
      *                      * Ammontare provvigione conteggiata       *
      *                      *-----------------------------------------*
           move      rf-gpc-amm-pvg       to   w-det-nmp-gpc-pro      .
      *                      *-----------------------------------------*
      *                      * Importo totale del documento cui si ri- *
      *                      * ferisce il conteggio                    *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-doc       to   w-det-nmp-gpc-tdo      .
      *                      *-----------------------------------------*
      *                      * Importo totale di eventuali acconti gia'*
      *                      * fatturati assorbiti nel documento cui   *
      *                      * si riferisce il conteggio               *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-agf       to   w-det-nmp-gpc-taf      .
      *                      *-----------------------------------------*
      *                      * Annotazioni sul conteggio provvigionale *
      *                      *-----------------------------------------*
           move      rf-gpc-not-ctg       to   w-det-nmp-gpc-not      .
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione prevista per la     *
      *                      * provvigione                             *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-mat       to   w-det-nmp-gpc-tdm      .
      *                      *-----------------------------------------*
      *                      * Segnale di provvigione con maturazione  *
      *                      * bloccata                                *
      *                      *-----------------------------------------*
           move      rf-gpc-mat-blo       to   w-det-nmp-gpc-blo      .
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima per la prov- *
      *                      * vigione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-ddm-min       to   w-det-nmp-gpc-dmm      .
      *                      *-----------------------------------------*
      *                      * Codice agente subordinato               *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   w-det-nmp-gpc-ags      .
      *                      *-----------------------------------------*
      *                      * Codice cliente per la fatturazione      *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   w-det-nmp-gpc-ccf      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente per la    *
      *                      * fatturazione                            *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-plf       to   w-det-nmp-gpc-dcf      .
       det-nmp-lco-400.
      *                  *---------------------------------------------*
      *                  * Preparazione valori in uscita               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   w-det-nmp-tip-ctg      .
       det-nmp-lco-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-lco-999.
       det-nmp-lco-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Lettura delle maturazioni gia' presenti relative al con-  *
      *    * teggio provvigionale, con totalizzazione totalizzazione   *
      *    * dell'importo provvigioni e calcolo del residuo da matu-   *
      *    * rare                                                      *
      *    *-----------------------------------------------------------*
       det-nmp-lmt-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita da subroutine   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-sub-flg      .
      *              *-------------------------------------------------*
      *              * Numero maturazioni preesistenti : zero          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-gpm-num      .
      *              *-------------------------------------------------*
      *              * Totale incassi preesistenti : zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-gpm-ipr      .
      *              *-------------------------------------------------*
      *              * Totale maturazioni preesistenti : zero          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-gpm-pre      .
       det-nmp-lmt-100.
      *              *-------------------------------------------------*
      *              * Start su maturazioni relative al conteggio      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CTGMTZ    "         to   f-key                  .
           move      w-det-nmp-num-ctg    to   rf-gpm-num-ctg         .
           move      zero                 to   rf-gpm-num-mtz         .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *              *-------------------------------------------------*
      *              * Se errore di Start : a preliminari di uscita    *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-lmt-800.
       det-nmp-lmt-200.
      *              *-------------------------------------------------*
      *              * Next su maturazioni relative al conteggio       *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *              *-------------------------------------------------*
      *              * Se fine file : a preliminari di uscita          *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-lmt-800.
       det-nmp-lmt-400.
      *              *-------------------------------------------------*
      *              * Test max su maturazioni relative al conteggio,  *
      *              * se non superato : a preliminari di uscita       *
      *              *-------------------------------------------------*
           if        rf-gpm-num-ctg       not  = w-det-nmp-num-ctg
                     go to det-nmp-lmt-800.
       det-nmp-lmt-600.
      *              *-------------------------------------------------*
      *              * Incremento numero records di maturazione pre-   *
      *              * esistenti relative al conteggio                 *
      *              *-------------------------------------------------*
           add       1                    to   w-det-nmp-gpm-num      .
      *              *-------------------------------------------------*
      *              * Incremento totale incassi preesistente          *
      *              *-------------------------------------------------*
           if        rf-gpm-tip-mtz       =    02
                     add   rf-gpm-imp-inc to   w-det-nmp-gpm-ipr      .
      *              *-------------------------------------------------*
      *              * Incremento totale maturato preesistente         *
      *              *-------------------------------------------------*
           add       rf-gpm-imp-mtz       to   w-det-nmp-gpm-pre      .
       det-nmp-lmt-700.
      *              *-------------------------------------------------*
      *              * Riciclo su maturazione successiva               *
      *              *-------------------------------------------------*
           go to     det-nmp-lmt-200.
       det-nmp-lmt-800.
      *              *-------------------------------------------------*
      *              * Preliminari di uscita                           *
      *              *-------------------------------------------------*
       det-nmp-lmt-810.
      *                  *---------------------------------------------*
      *                  * Determinazione del totale maturazione resi- *
      *                  * dua da assegnare                            *
      *                  *---------------------------------------------*
           subtract  w-det-nmp-gpm-pre    from w-det-nmp-gpc-pro
                                        giving w-det-nmp-gpm-res      .
      *                  *---------------------------------------------*
      *                  * Assestamento in caso di conteggio provvi-   *
      *                  * gionale negativo                            *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpc-pro    <    zero
                     if    w-det-nmp-gpm-res
                                          >    zero
                           move  zero     to   w-det-nmp-gpm-res      .
      *                  *---------------------------------------------*
      *                  * Assestamento in caso di conteggio provvi-   *
      *                  * gionale positivo                            *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpc-pro    not  < zero
                     if    w-det-nmp-gpm-res
                                          <    zero
                           move  zero     to   w-det-nmp-gpm-res      .
       det-nmp-lmt-820.
      *                  *---------------------------------------------*
      *                  * Set del flag di uscita in caso di residuo   *
      *                  * da maturare a zero                          *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpm-res    =    zero
                     move  11             to   w-det-nmp-sub-flg      .
       det-nmp-lmt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-lmt-999.
       det-nmp-lmt-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Test se conteggio provvigionale bloccato o con data di    *
      *    * maturazione minima superiore a quella di riferimento      *
      *    *-----------------------------------------------------------*
       det-nmp-tsb-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita da subroutine   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-sub-flg      .
       det-nmp-tsb-100.
      *              *-------------------------------------------------*
      *              * Test su conteggio bloccato                      *
      *              *-------------------------------------------------*
       det-nmp-tsb-110.
      *                  *---------------------------------------------*
      *                  * Se il record del conteggio contiene il se-  *
      *                  * gnale di maturazione bloccata : uscita con  *
      *                  * flag ad errore                              *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpc-blo    =    02
                     move  21             to   w-det-nmp-sub-flg
                     go to det-nmp-tsb-999.
       det-nmp-tsb-120.
      *                  *---------------------------------------------*
      *                  * Altrimenti continuazione per altri tests    *
      *                  *---------------------------------------------*
           go to     det-nmp-tsb-200.
       det-nmp-tsb-200.
      *              *-------------------------------------------------*
      *              * Test su data di maturazione minima              *
      *              *-------------------------------------------------*
       det-nmp-tsb-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di conteggio  *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpc-tdc    =    03
                     go to det-nmp-tsb-300
           else      go to det-nmp-tsb-400.
       det-nmp-tsb-300.
      *                  *---------------------------------------------*
      *                  * Se il conteggio si riferisce ad una nota di *
      *                  * accredito                                   *
      *                  *---------------------------------------------*
       det-nmp-tsb-310.
      *                      *-----------------------------------------*
      *                      * Nessun controllo sulla data di matura-  *
      *                      * zione minima, continuazione per altri   *
      *                      * tests                                   *
      *                      *-----------------------------------------*
           go to     det-nmp-tsb-700.
       det-nmp-tsb-400.
      *                  *---------------------------------------------*
      *                  * Se il conteggio si riferisce ad una fattura *
      *                  * o ad una nota di addebito                   *
      *                  *---------------------------------------------*
       det-nmp-tsb-410.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda della data di ma-  *
      *                      * turazione minima espressa nel record    *
      *                      * del conteggio provvigionale             *
      *                      *-----------------------------------------*
           if        w-det-nmp-gpc-dmm    =    zero           or
                     w-det-nmp-gpc-dmm    =    w-det-nmp-gpc-ddo
                     go to det-nmp-tsb-500
           else      go to det-nmp-tsb-600.
       det-nmp-tsb-500.
      *                      *-----------------------------------------*
      *                      * Se data di maturazione minima espressa  *
      *                      * nel record del conteggio provvigionale  *
      *                      * pari a zero o uguale alla data docu-    *
      *                      * mento                                   *
      *                      *-----------------------------------------*
       det-nmp-tsb-510.
      *                          *-------------------------------------*
      *                          * Si confronta la data di maturazione *
      *                          * passata come parametro con la data  *
      *                          * documento, e se il test non e' su-  *
      *                          * perato si esce con flag ad errore   *
      *                          *-------------------------------------*
           if        w-det-nmp-gpc-ddo    >    w-det-nmp-dat-mtz
                     move  22             to   w-det-nmp-sub-flg
                     go to det-nmp-tsb-999.
       det-nmp-tsb-520.
      *                          *-------------------------------------*
      *                          * Altrimenti continuazione per altri  *
      *                          * tests                               *
      *                          *-------------------------------------*
           go to     det-nmp-tsb-700.
       det-nmp-tsb-600.
      *                      *-----------------------------------------*
      *                      * Se data di maturazione minima espressa  *
      *                      * nel record del conteggio provvigionale  *
      *                      * diversa da zero                         *
      *                      *-----------------------------------------*
       det-nmp-tsb-610.
      *                          *-------------------------------------*
      *                          * Si confronta la data di maturazione *
      *                          * passata come parametro con la data  *
      *                          * di maturazione minima espressa nel  *
      *                          * record, e se il test non e' supera- *
      *                          * to si esce con flag ad errore       *
      *                          *-------------------------------------*
           if        w-det-nmp-gpc-dmm    >    w-det-nmp-dat-mtz
                     move  22             to   w-det-nmp-sub-flg
                     go to det-nmp-tsb-999.
       det-nmp-tsb-620.
      *                          *-------------------------------------*
      *                          * Altrimenti continuazione per altri  *
      *                          * tests                               *
      *                          *-------------------------------------*
           go to     det-nmp-tsb-700.
       det-nmp-tsb-700.
      *              *-------------------------------------------------*
      *              * Fine tests                                      *
      *              *-------------------------------------------------*
       det-nmp-tsb-710.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-nmp-tsb-999.
       det-nmp-tsb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Esecuzione dell'effettiva determinazione della nuova ma-  *
      *    * turazione                                                 *
      *    *-----------------------------------------------------------*
       det-nmp-det-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flags di uscita da subroutine  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-sub-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di conteggio      *
      *              *-------------------------------------------------*
           if        w-det-nmp-gpc-tdc    =    03 or
                     w-det-nmp-gpc-tdc    =    11
                     go to det-nmp-det-100
           else      go to det-nmp-det-200.
       det-nmp-det-100.
      *              *-------------------------------------------------*
      *              * Se il conteggio si riferisce ad una nota di ac- *
      *              * credito o ad una fattura negativa               *
      *              *-------------------------------------------------*
       det-nmp-det-110.
      *                  *---------------------------------------------*
      *                  * Preparazione del flag di uscita indicante   *
      *                  * una maturazione di tipo immediato           *
      *                  *---------------------------------------------*
           move      01                   to   w-det-nmp-sub-flg      .
       det-nmp-det-120.
      *                  *---------------------------------------------*
      *                  * Preparazione dell'importo della nuova matu- *
      *                  * razione pari all'importo residuo da matura- *
      *                  * re                                          *
      *                  *---------------------------------------------*
           move      w-det-nmp-gpm-res    to   w-det-nmp-imp-mtz      .
       det-nmp-det-130.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-nmp-det-999.
       det-nmp-det-200.
      *              *-------------------------------------------------*
      *              * Se il conteggio si riferisce ad una fattura o   *
      *              * ad una nota di addebito                         *
      *              *-------------------------------------------------*
       det-nmp-det-210.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di maturazio- *
      *                  * ne previsto per il conteggio                *
      *                  *---------------------------------------------*
           if        w-det-nmp-gpc-tdm    =    01
                     go to det-nmp-det-300
           else if   w-det-nmp-gpc-tdm    =    02
                     go to det-nmp-det-400
           else      go to det-nmp-det-300.
       det-nmp-det-300.
      *                  *---------------------------------------------*
      *                  * Se per il conteggio e' prevista una matura- *
      *                  * zione di tipo immediato                     *
      *                  *---------------------------------------------*
       det-nmp-det-310.
      *                      *-----------------------------------------*
      *                      * Preparazione del flag di uscita indi-   *
      *                      * cante una maturazione di tipo immediato *
      *                      *-----------------------------------------*
           move      01                   to   w-det-nmp-sub-flg      .
       det-nmp-det-320.
      *                      *-----------------------------------------*
      *                      * Preparazione dell'importo della nuova   *
      *                      * maturazione pari all'importo residuo da *
      *                      * maturare                                *
      *                      *-----------------------------------------*
           move      w-det-nmp-gpm-res    to   w-det-nmp-imp-mtz      .
       det-nmp-det-330.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-nmp-det-999.
       det-nmp-det-400.
      *                  *---------------------------------------------*
      *                  * Se per il conteggio e' prevista una matura- *
      *                  * zione di tipo a fronte incasso da cliente   *
      *                  *---------------------------------------------*
       det-nmp-det-410.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del segno algebri- *
      *                      * co del totale del conteggio             *
      *                      *-----------------------------------------*
           if        w-det-nmp-gpc-pro    <    zero
                     go to det-nmp-det-500
           else      go to det-nmp-det-600.
       det-nmp-det-500.
      *                      *-----------------------------------------*
      *                      * Se il totale provvigione del conteggio  *
      *                      * provvigionale e' negativo               *
      *                      *-----------------------------------------*
       det-nmp-det-510.
      *                          *-------------------------------------*
      *                          * Preparazione del flag di uscita     *
      *                          * indicante una maturazione di tipo   *
      *                          * immediato                           *
      *                          *-------------------------------------*
           move      01                   to   w-det-nmp-sub-flg      .
       det-nmp-det-520.
      *                          *-------------------------------------*
      *                          * Preparazione dell'importo della     *
      *                          * nuova maturazione pari all'importo  *
      *                          * residuo da maturare                 *
      *                          *-------------------------------------*
           move      w-det-nmp-gpm-res    to   w-det-nmp-imp-mtz      .
       det-nmp-det-530.
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-nmp-det-999.
       det-nmp-det-600.
      *                      *-----------------------------------------*
      *                      * Se il totale provvigione del conteggio  *
      *                      * provvigionale e' positivo               *
      *                      *-----------------------------------------*
       det-nmp-det-610.
      *                          *-------------------------------------*
      *                          * Determinazione del totale documento *
      *                          * presunto, pari al totale documento  *
      *                          * reale, aumentato degli eventuali    *
      *                          * acconti gia' fatturati              *
      *                          *-------------------------------------*
           move      w-det-nmp-gpc-tdo    to   w-det-nmp-det-tdp      .
           add       w-det-nmp-gpc-taf    to   w-det-nmp-det-tdp      .
       det-nmp-det-620.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dell'importo   *
      *                          * del totale documento presunto e del *
      *                          * numero protocollo movimento di fat- *
      *                          * ture clienti                        *
      *                          *-------------------------------------*
           if        w-det-nmp-det-tdp    not > zero or
                     w-det-nmp-gpc-pfc    =    zero
                     go to det-nmp-det-700
           else      go to det-nmp-det-800.
       det-nmp-det-700.
      *                          *-------------------------------------*
      *                          * Se totale documento presunto non    *
      *                          * superiore a zero o se numero pro-   *
      *                          * tocollo movimento di fatture cli-   *
      *                          * enti a zero                         *
      *                          *-------------------------------------*
       det-nmp-det-710.
      *                              *---------------------------------*
      *                              * Preparazione del flag di uscita *
      *                              * indicante una maturazione di    *
      *                              * tipo immediato                  *
      *                              *---------------------------------*
           move      01                   to   w-det-nmp-sub-flg      .
       det-nmp-det-720.
      *                              *---------------------------------*
      *                              * Preparazione dell'importo della *
      *                              * nuova maturazione pari all'im-  *
      *                              * porto residuo da maturare       *
      *                              *---------------------------------*
           move      w-det-nmp-gpm-res    to   w-det-nmp-imp-mtz      .
       det-nmp-det-730.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-nmp-det-999.
       det-nmp-det-800.
      *                          *-------------------------------------*
      *                          * Se totale documento presunto maggio-*
      *                          * re di zero ed inoltre numero proto- *
      *                          * collo movimento di fatture clienti  *
      *                          * diverso da zero                     *
      *                          *-------------------------------------*
       det-nmp-det-810.
      *                              *---------------------------------*
      *                              * Determinazione del totale in-   *
      *                              * cassato reale a fronte del do-  *
      *                              * cumento indicato dal protocollo *
      *                              * movimento di fatture clienti,   *
      *                              * per analisi del portafoglio     *
      *                              * crediti                         *
      *                              *---------------------------------*
           move      w-det-nmp-dat-mtz    to   w-det-nmp-iap-ddr      .
           move      w-det-nmp-gpc-pfc    to   w-det-nmp-iap-pfc      .
           move      w-det-nmp-gpc-tdo    to   w-det-nmp-iap-tft      .
           perform   det-nmp-iap-000      thru det-nmp-iap-999        .
       det-nmp-det-820.
      *                              *---------------------------------*
      *                              * Determinazione dell'incasso to- *
      *                              * tale presunto, pari all'incasso *
      *                              * determinato dall'analisi porta- *
      *                              * foglio crediti, aumentato di e- *
      *                              * ventuali acconti gia' fatturati *
      *                              *---------------------------------*
           move      w-det-nmp-iap-inc    to   w-det-nmp-det-tip      .
           add       w-det-nmp-gpc-taf    to   w-det-nmp-det-tip      .
       det-nmp-det-830.
      *                              *---------------------------------*
      *                              * Determinazione del totale della *
      *                              * provvigione maturata in base    *
      *                              * alla seguente proporzione :     *
      *                              *                                 *
      *                              * tdp : tpc = tip : tpm           *
      *                              *                                 *
      *                              * dove                            *
      *                              *                                 *
      *                              * tdp : Totale documento presunto *
      *                              * tpc : Totale provvigione con-   *
      *                              *       teggiata                  *
      *                              * tip : Totale incassato presunto *
      *                              * tpc : Totale provvigione ma-    *
      *                              *       turata                    *
      *                              *---------------------------------*
           move      w-det-nmp-det-tip    to   w-det-nmp-det-w18      .
           multiply  w-det-nmp-gpc-pro    by   w-det-nmp-det-w18      .
           divide    w-det-nmp-det-tdp    into w-det-nmp-det-w18
                                                         rounded      .
           move      w-det-nmp-det-w18    to   w-det-nmp-det-tpm      .
       det-nmp-det-840.
      *                              *---------------------------------*
      *                              * Se il totale della provvigione  *
      *                              * maturata proporzionalmente agli *
      *                              * incassi supera l'importo totale *
      *                              * della provvigione conteggiata,  *
      *                              * la si riduce ad un importo pari *
      *                              * a quello conteggiato            *
      *                              *---------------------------------*
           if        w-det-nmp-det-tpm    >    w-det-nmp-gpc-pro
                     move  w-det-nmp-gpc-pro
                                          to   w-det-nmp-det-tpm      .
       det-nmp-det-850.
      *                              *---------------------------------*
      *                              * Determinazione della nuova ma-  *
      *                              * turazione come differenza tra   *
      *                              * l'importo totale maturato pro-  *
      *                              * porzionalmente agli incassi, e  *
      *                              * le maturazioni preesistenti     *
      *                              *---------------------------------*
           move      w-det-nmp-det-tpm    to   w-det-nmp-det-npm      .
           subtract  w-det-nmp-gpm-pre    from w-det-nmp-det-npm      .
       det-nmp-det-860.
      *                              *---------------------------------*
      *                              * Se la nuova maturazione non e'  *
      *                              * superiore a zero si esce con    *
      *                              * flag ad errore indicante la     *
      *                              * non maturazione                 *
      *                              *---------------------------------*
           if        w-det-nmp-det-npm    not  > zero
                     move  12             to   w-det-nmp-sub-flg
                     go to det-nmp-det-999.
       det-nmp-det-870.
      *                              *---------------------------------*
      *                              * Se il nuovo residuo risulta in- *
      *                              * feriore ad un minimo di 0,20    *
      *                              * (tenendo conto del fatto che la *
      *                              * valuta base considera le ultime *
      *                              * due cifre dell'importo come i   *
      *                              * decimali dopo la virgola), il   *
      *                              * che puo' essere dovuto anche ad *
      *                              * arrotondamenti nel calcolo      *
      *                              * dell'ammontare reale incassato, *
      *                              * si ingloba questo piccolo resi- *
      *                              * duo nella nuova maturazione     *
      *                              *                                 *
      *                              * Istruzione rivisitata 31/05/05  *
      *                              *---------------------------------*
           move      w-det-nmp-gpm-res    to   w-det-nmp-det-w11      .
           subtract  w-det-nmp-det-npm    from w-det-nmp-det-w11      .
           if        w-det-nmp-det-w11    <    20
                     add   w-det-nmp-det-w11
                                          to   w-det-nmp-det-npm      .
       det-nmp-det-880.
      *                              *---------------------------------*
      *                              * Preparazione dei valori prima   *
      *                              * dell'uscita                     *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Flag di uscita ad indicare  *
      *                                  * una maturazione a fronte    *
      *                                  * incasso                     *
      *                                  *-----------------------------*
           move      02                   to   w-det-nmp-sub-flg      .
      *                                  *-----------------------------*
      *                                  * Nuovo importo incassato,    *
      *                                  * calcolato come differenza   *
      *                                  * tra l'incasso totale pre-   *
      *                                  * sunto e l'incassato pre-    *
      *                                  * esistente.                  *
      *                                  * Se pero' la maturazione e'  *
      *                                  * totale la differenza si e-  *
      *                                  * segue tra il totale docu-   *
      *                                  * mento stesso e l'incassato  *
      *                                  * preesistente.               *
      *                                  *-----------------------------*
           if        w-det-nmp-gpm-res    =    w-det-nmp-det-npm
                     move  w-det-nmp-iap-tft
                                          to   w-det-nmp-imp-inc
           else      move  w-det-nmp-det-tip
                                          to   w-det-nmp-imp-inc      .
           subtract  w-det-nmp-gpm-ipr    from w-det-nmp-imp-inc      .
      *                                  *-----------------------------*
      *                                  * Nuova provvigione maturata  *
      *                                  *-----------------------------*
           move      w-det-nmp-det-npm    to   w-det-nmp-imp-mtz      .
       det-nmp-det-890.
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     det-nmp-det-999.
       det-nmp-det-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Determinazione del totale incassato reale a fronte del    *
      *    * documento indicato dal protocollo movimento di fatture    *
      *    * clienti, per analisi del portafoglio crediti              *
      *    *-----------------------------------------------------------*
       det-nmp-iap-000.
      *              *-------------------------------------------------*
      *              * Incasso totale determinato : a zero             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-iap-inc      .
      *              *-------------------------------------------------*
      *              * Incasso parziale presunto su differenza tra il  *
      *              * totale fattura e le scadenze emesse : a zero    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-iap-ipd      .
      *              *-------------------------------------------------*
      *              * Incasso parziale reale : a zero                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-iap-irl      .
       det-nmp-iap-100.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale delle scadenze emesse in   *
      *              * origine a fronte del movimento di fatture cli-  *
      *              * enti                                            *
      *              *-------------------------------------------------*
       det-nmp-iap-105.
      *                  *---------------------------------------------*
      *                  * Numero scadenze caricate a zero             *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-nmp-iap-box      .
      *                  *---------------------------------------------*
      *                  * Importo totale scadenze caricate a zero     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-nmp-iap-bot      .
       det-nmp-iap-110.
      *                  *---------------------------------------------*
      *                  * Start su archivio scadenze emesse a fronte  *
      *                  * del movimento di fatture clienti            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "PFCNRS    "         to   f-key                  .
           move      w-det-nmp-iap-pfc    to   rf-sdb-prt-fcl         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se errore di Start : a fine caricamento     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-200.
       det-nmp-iap-115.
      *                  *---------------------------------------------*
      *                  * Next su archivio scadenze emesse a fronte   *
      *                  * del movimento di fatture clienti            *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : a fine caricamento           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-200.
       det-nmp-iap-120.
      *                  *---------------------------------------------*
      *                  * Test max su archivio scadenze emesse a      *
      *                  * fronte del movimento di fatture clienti,    *
      *                  * se non superato : a fine caricamento        *
      *                  *---------------------------------------------*
           if        rf-sdb-prt-fcl       not  = w-det-nmp-iap-pfc
                     go to det-nmp-iap-200.
       det-nmp-iap-125.
      *                  *---------------------------------------------*
      *                  * Se importo scadenza a zero o negativo : si  *
      *                  * ignora la scadenza                          *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-115.
      *                  *---------------------------------------------*
      *                  * Se dal tipo documento di riferimento si de- *
      *                  * duce che la scadenza corrisponde ad una no- *
      *                  * ta di accredito o ad un anticipo : si igno- *
      *                  * ra la scadenza                              *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-115.
      *                  *---------------------------------------------*
      *                  * Se il protocollo fattura a cui si riferisce *
      *                  * e' a zero : si ignora la scadenza           *
      *                  *---------------------------------------------*
           if        rf-sdb-prt-fcl       not  > zero
                     go to det-nmp-iap-115.
       det-nmp-iap-130.
      *                  *---------------------------------------------*
      *                  * Incremento il numero di scadenze emesse in  *
      *                  * origine a fronte del movimento di fatture   *
      *                  * clienti                                     *
      *                  *---------------------------------------------*
           add       1                    to   w-det-nmp-iap-box      .
      *                  *---------------------------------------------*
      *                  * Accumulo il totale importi scadenze emesse  *
      *                  * in origine a fronte del movimento di fattu- *
      *                  * re clienti                                  *
      *                  *---------------------------------------------*
           add       rf-sdb-imp-sdb       to   w-det-nmp-iap-bot      .
       det-nmp-iap-135.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione della scadenza              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero scadenza originale               *
      *                      *-----------------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-bon
                                              (w-det-nmp-iap-box)     .
      *                      *-----------------------------------------*
      *                      * Numero riscossione, o pagamento, o com- *
      *                      * pensazione, cui la scadenza e' stata    *
      *                      * sottoposta                              *
      *                      *-----------------------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-bop
                                              (w-det-nmp-iap-box)     .
      *                      *-----------------------------------------*
      *                      * Importo scadenza originale, in valuta   *
      *                      * base                                    *
      *                      *-----------------------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-boi
                                              (w-det-nmp-iap-box)     .
      *                      *-----------------------------------------*
      *                      * Importo scadenza riscosso, in valuta    *
      *                      * base, a zero                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-nmp-iap-bor
                                              (w-det-nmp-iap-box)     .
       det-nmp-iap-140.
      *                  *---------------------------------------------*
      *                  * Se raggiunto il massimo numero di scadenze  *
      *                  * bufferizzabili si va' a fine caricamento,   *
      *                  * altrimenti si ricicla a leggere la scaden-  *
      *                  * za successiva                               *
      *                  *---------------------------------------------*
           if        w-det-nmp-iap-box    =    96
                     go to det-nmp-iap-200
           else      go to det-nmp-iap-115.
       det-nmp-iap-200.
      *              *-------------------------------------------------*
      *              * Fine caricamento iniziale delle scadenze emesse *
      *              * in origine a fronte del movimento di fatture    *
      *              * clienti                                         *
      *              *-------------------------------------------------*
       det-nmp-iap-205.
      *                  *---------------------------------------------*
      *                  * Se il totale importi delle scadenze emesse  *
      *                  * in origine e' inferiore al totale della     *
      *                  * fattura cui devono fare riscontro le sca-   *
      *                  * denze, si considera che l'importo mancante  *
      *                  * sia comunque stato riscosso, pertanto lo si *
      *                  * memorizza come incasso presunto             *
      *                  *---------------------------------------------*
           if        w-det-nmp-iap-bot    <    w-det-nmp-iap-tft
                     move      w-det-nmp-iap-tft
                                          to   w-det-nmp-iap-ipd
                     subtract  w-det-nmp-iap-bot
                                          from w-det-nmp-iap-ipd      .
       det-nmp-iap-210.
      *                  *---------------------------------------------*
      *                  * Se numero scadenze caricate pari a zero :   *
      *                  * a sommatoria conclusiva                     *
      *                  *---------------------------------------------*
           if        w-det-nmp-iap-box    =    zero
                     go to det-nmp-iap-800.
       det-nmp-iap-215.
      *                  *---------------------------------------------*
      *                  * Se totale importi scadenze caricate pari a  *
      *                  * zero : a sommatoria conclusiva              *
      *                  *---------------------------------------------*
           if        w-det-nmp-iap-bot    =    zero
                     go to det-nmp-iap-800.
       det-nmp-iap-250.
      *              *-------------------------------------------------*
      *              * Ciclo per la determinazione dell'importo incas- *
      *              * sato a fronte di ogni singola scadenza emessa   *
      *              * in origine caricata nel buffer                  *
      *              *-------------------------------------------------*
       det-nmp-iap-260.
      *                  *---------------------------------------------*
      *                  * Indice per la scansione del buffer scadenze *
      *                  * emesse in origine a fronte del movimento di *
      *                  * fatture clienti : a zero                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-nmp-iap-boy      .
       det-nmp-iap-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice per la scansione del buf- *
      *                  * fer scadenze emesse in origine a fronte del *
      *                  * movimento di fatture clienti                *
      *                  *---------------------------------------------*
           add       1                    to   w-det-nmp-iap-boy      .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo numero di scadenze ca-  *
      *                  * ricate : a sommatoria conclusiva            *
      *                  *---------------------------------------------*
           if        w-det-nmp-iap-boy    >    w-det-nmp-iap-box
                     go to det-nmp-iap-800.
       det-nmp-iap-350.
      *                  *---------------------------------------------*
      *                  * Determinazione del totale riscosso a fronte *
      *                  * della scadenza emessa in origine in esame   *
      *                  *---------------------------------------------*
       det-nmp-iap-360.
      *                      *-----------------------------------------*
      *                      * Totale riscosso a fronte della scadenza *
      *                      * emessa in origine : a zero              *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-nmp-iap-sbt      .
      *                      *-----------------------------------------*
      *                      * Numero scadenze nel sub-buffer : 1      *
      *                      *-----------------------------------------*
           move      1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-370.
      *                      *-----------------------------------------*
      *                      * Memorizzazione della prima scadenza nel *
      *                      * sub-buffer                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero scadenza                     *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-bon
                    (w-det-nmp-iap-boy)   to   w-det-nmp-iap-sbn (1)  .
      *                          *-------------------------------------*
      *                          * Numero riscossione, o pagamento, o  *
      *                          * compensazione, cui la scadenza e'   *
      *                          * stata sottoposta                    *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-bop
                    (w-det-nmp-iap-boy)   to   w-det-nmp-iap-sbp (1)  .
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata : *
      *                          * a No                                *
      *                          *-------------------------------------*
           move      spaces               to   w-det-nmp-iap-sbf (1)  .
      *                          *-------------------------------------*
      *                          * Importo scadenza                    *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-boi
                    (w-det-nmp-iap-boy)   to   w-det-nmp-iap-sbi (1)  .
      *                          *-------------------------------------*
      *                          * Coefficiente per la scadenza : a 1  *
      *                          *-------------------------------------*
           move      1                    to   w-det-nmp-iap-sbc (1)  .
       det-nmp-iap-380.
      *                      *-----------------------------------------*
      *                      * Indice per la scansione del sub-buffer  *
      *                      * : a zero                                *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-nmp-iap-sby      .
       det-nmp-iap-390.
      *                      *-----------------------------------------*
      *                      * Se l'importo totale riscosso sinora per *
      *                      * la singola scadenza emessa in origine   *
      *                      * in esame e' superiore, o uguale, al-    *
      *                      * l'importo della scadenza stessa, lo si  *
      *                      * riduce ad un importo pari a quello del- *
      *                      * la scadenza stessa e poi si va' a fine  *
      *                      * determinazione importo riscosso per la  *
      *                      * scadenza emessa in origine in esame     *
      *                      *-----------------------------------------*
           if        w-det-nmp-iap-sbt    not  < w-det-nmp-iap-boi
                                                (w-det-nmp-iap-boy)
                     move  w-det-nmp-iap-boi
                          (w-det-nmp-iap-boy)
                                          to   w-det-nmp-iap-sbt
                     go to det-nmp-iap-750.
      *                      *-----------------------------------------*
      *                      * Incremento indice per la scansione del  *
      *                      * sub-buffer                              *
      *                      *-----------------------------------------*
           add       1                    to   w-det-nmp-iap-sby      .
      *                      *-----------------------------------------*
      *                      * Se oltre il numero di scadenze memoriz- *
      *                      * zate nel sub-buffer : a fine determina- *
      *                      * zione importo riscosso per la scadenza  *
      *                      * emessa in origine in esame              *
      *                      *-----------------------------------------*
           if        w-det-nmp-iap-sby    >    w-det-nmp-iap-sbx
                     go to det-nmp-iap-750.
      *                      *-----------------------------------------*
      *                      * Se la scadenza in esame nel sub-buffer  *
      *                      * e' gia' stata trattata : si ricicla su  *
      *                      * scadenza successiva nel sub-buffer      *
      *                      *-----------------------------------------*
           if        w-det-nmp-iap-sbf
                    (w-det-nmp-iap-sby)   not  = spaces
                     go to det-nmp-iap-390.
      *                      *-----------------------------------------*
      *                      * Rilettura della scadenza in esame del   *
      *                      * sub-buffer                              *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      w-det-nmp-iap-sbn
                    (w-det-nmp-iap-sby)   to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                      *-----------------------------------------*
      *                      * Se record relativo alla scadenza in e-  *
      *                      * same del sub-buffer non trovato : a     *
      *                      * trattamento per scadenza in esame non   *
      *                      * piu' esistente                          *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-400.
      *                      *-----------------------------------------*
      *                      * Se la personalizzazione lo prevede, si  *
      *                      * esegue un test sulla data di scadenza   *
      *                      * in modo che venga esclusa dal buffer se *
      *                      * la data di scadenza non supera il test  *
      *                      *-----------------------------------------*
           if        w-prs-dat-mtz        not  = "X"
                     go to det-nmp-iap-392.
           if        rf-sdb-dts-sdb       >    rr-dat-max
                     go to det-nmp-iap-404.
           if        rf-sdb-dts-sdb       =    zero and
                     rf-sdb-dat-ddr       >    rr-dat-max
                     go to det-nmp-iap-404.
       det-nmp-iap-392.
      *                      *-----------------------------------------*
      *                      * Determinazione dell'ultima operazione   *
      *                      * eseguita sulla scadenza del sub-buffer  *
      *                      * in esame, richiamando la routine con    *
      *                      * l'opzione di non considerare lo stato   *
      *                      * delle distinte di presentazione.        *
      *                      *                                         *
      *                      * Si considerano tutti i movimenti di     *
      *                      * portafoglio, con qualsiasi data di re-  *
      *                      * gistrazione.                            *
      *                      *                                         *
      *                      * Test su eventuale personalizzazione che *
      *                      * prevede un test differenziato per gli   *
      *                      * incassi elettronici                     *
      *                      *-----------------------------------------*
           move      02                   to   w-sts-sdb-tip-det      .
           move      9991231              to   w-sts-sdb-dat-rif      .
      *
           if        w-prs-dat-mtz        not  = "X"
                     go to det-nmp-iap-395.
           if        rf-sdb-tip-sdb       =    02 or
                     rf-sdb-tip-sdb       =    03 or
                     rf-sdb-tip-sdb       =    04 or
                     rf-sdb-tip-sdb       =    05 or
                     rf-sdb-tip-sdb       =    06
                     go to det-nmp-iap-395.
           move      rr-dat-max           to   w-sts-sdb-dat-rif      .
      *
       det-nmp-iap-395.
           perform   det-sts-sdb-000      thru det-sts-sdb-999        .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'ultima opera- *
      *                      * zione eseguita entro la data di riferi- *
      *                      * mento sulla scadenza del sub-buffer in  *
      *                      * esame                                   *
      *                      *-----------------------------------------*
           if        w-sts-sdb-ult-ope    =    000
                     go to det-nmp-iap-440
           else if   w-sts-sdb-ult-ope    =    100
                     go to det-nmp-iap-460
           else if   w-sts-sdb-ult-ope    =    200
                     go to det-nmp-iap-480
           else if   w-sts-sdb-ult-ope    =    300 or
                     w-sts-sdb-ult-ope    =    320 or
                     w-sts-sdb-ult-ope    =    350
                     go to det-nmp-iap-520
           else if   w-sts-sdb-ult-ope    =    400 or
                     w-sts-sdb-ult-ope    =    500 or
                     w-sts-sdb-ult-ope    =    540 or
                     w-sts-sdb-ult-ope    =    560
                     go to det-nmp-iap-600
           else if   w-sts-sdb-ult-ope    =    600
                     go to det-nmp-iap-620
           else if   w-sts-sdb-ult-ope    =    620
                     go to det-nmp-iap-660
           else if   w-sts-sdb-ult-ope    =    700 or
                     w-sts-sdb-ult-ope    =    720 or
                     w-sts-sdb-ult-ope    =    740
                     go to det-nmp-iap-700
           else      go to det-nmp-iap-420.
       det-nmp-iap-400.
      *                      *-----------------------------------------*
      *                      * Se record relativo alla scadenza in e-  *
      *                      * same del sub-buffer non piu' esistente  *
      *                      *-----------------------------------------*
       det-nmp-iap-402.
      *                          *-------------------------------------*
      *                          * Accumulo importo totale della sca-  *
      *                          * denza, moltiplicato per il coeffi-  *
      *                          * ciente della scadenza, sul totale   *
      *                          * riscosso                            *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-404.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-406.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-420.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' di  *
      *                      * tipo non riconosciuto                   *
      *                      *-----------------------------------------*
       det-nmp-iap-422.
      *                          *-------------------------------------*
      *                          * Accumulo importo totale della sca-  *
      *                          * denza, moltiplicato per il coeffi-  *
      *                          * ciente della scadenza, sul totale   *
      *                          * riscosso                            *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-424.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-426.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-440.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 000 : Nessuna operazione                *
      *                      *-----------------------------------------*
       det-nmp-iap-442.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-444.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-460.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 100 : Emissione                         *
      *                      *-----------------------------------------*
       det-nmp-iap-462.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-464.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-480.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 200 : Storno                            *
      *                      *-----------------------------------------*
       det-nmp-iap-482.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se c'e' stata  *
      *                          * riemissione di scadenza oppure no   *
      *                          * a fronte dello storno               *
      *                          *-------------------------------------*
           if        rf-sdb-ens-sto       =    02   or
                     rf-sdb-nns-sto       =    zero
                     go to det-nmp-iap-500.
       det-nmp-iap-484.
      *                          *-------------------------------------*
      *                          * Se c'e' stata riemissione di sca-   *
      *                          * denza a fronte dello storno :       *
      *                          *                                     *
      *                          * si effettua una scansione preventi- *
      *                          * va del file [rsc] per verificare se *
      *                          * a fronte dello storno esistono piu' *
      *                          * scadenze di riferimento             *
      *                          *-------------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-rsc-000  thru det-nmp-iap-rsc-999    .
           if        w-det-nmp-iap-rc1    >    1
                     go to det-nmp-iap-486
           else      go to det-nmp-iap-487.
       det-nmp-iap-486.
      *                          *-------------------------------------*
      *                          * Se piu' scadenze trovate            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se esistono scadenze che si     *
      *                              * si riferiscono a quella in fase *
      *                              * di memorizzazione si memoriz-   *
      *                              * zano in sua vece                *
      *                              *---------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-sto-000  thru det-nmp-iap-sto-999    .
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-487.
      *                          *-------------------------------------*
      *                          * In tutti gli altri casi             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura della scadenza riemessa *
      *                              * a fronte dello storno           *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-sdb-nns-sto       to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se scadenza non esistente : co- *
      *                              * me per 'no riemissione' a fron- *
      *                              * te dello storno                 *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-500.
      *                              *---------------------------------*
      *                              * Se scadenza con importo non po- *
      *                              * sitivo : come per 'no riemis-   *
      *                              * sione' a fronte dello storno    *
      *                              *---------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-500.
      *                              *---------------------------------*
      *                              * Se scadenza di tipo negativo :  *
      *                              * come per 'no riemissione' a     *
      *                              * fronte dello storno             *
      *                              *---------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-500.
       det-nmp-iap-488.
      *                              *---------------------------------*
      *                              * Determinazione della differenza *
      *                              * tra l'importo scadenza in esame *
      *                              * nel sub-buffer e l'importo sca- *
      *                              * denza riemessa                  *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w03      .
           subtract  rf-sdb-imp-sdb       from w-det-nmp-iap-w03      .
       det-nmp-iap-490.
      *                              *---------------------------------*
      *                              * Se la scadenza riemessa e' di   *
      *                              * importo inferiore alla scadenza *
      *                              * in esame nel sub-buffer, si ac- *
      *                              * cumula la differenza, moltipli- *
      *                              * cata per il coefficiente della  *
      *                              * scadenza in esame, sul totale   *
      *                              * riscosso                        *
      *                              *---------------------------------*
           if        w-det-nmp-iap-w03    not  > zero
                     go to det-nmp-iap-492.
      *                              *---------------------------------*
      *                              * NOTA IMPORTANTE NdK - 14/05/97  *
      *                              *                                 *
      *                              * Si saltano le 4 linee che se-   *
      *                              * guono perche' non vanno bene.   *
      *                              * Provvisoriamente viene emesso   *
      *                              * un segnale di avviso.           *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Editing numero scadenza     *
      *                                  *-----------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-nmp-iap-sbn
                    (w-det-nmp-iap-sby)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                                  *-----------------------------*
      *                                  * Composizione messaggio      *
      *                                  *-----------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "Per la scadenza nr."
                                          to   w-all-str-cat (01)     .
           move      p-edt                to   w-all-str-cat (02)     .
           move      "e' impossibile il calcolo della maturazione !"
                                          to   w-all-str-cat (03)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                                  *-----------------------------*
      *                                  * Emissione messaggio         *
      *                                  *-----------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                                  *-----------------------------*
      *                                  * Editing numero scadenza     *
      *                                  *-----------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-sdb-num-sdb       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                                  *-----------------------------*
      *                                  * Composizione messaggio      *
      *                                  *-----------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "- Verificare lo stato della scadenza :"
                                          to   w-all-str-cat (01)     .
           move      p-edt                to   w-all-str-cat (02)     .
           move      "emessa a fronte dello storno !"
                                          to   w-all-str-cat (03)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                                  *-----------------------------*
      *                                  * Emissione messaggio         *
      *                                  *-----------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                                  *-----------------------------*
      *                                  * A istruzione successiva     *
      *                                  *-----------------------------*
           go to det-nmp-iap-492.
      *                                  *-----------------------------*
      *                                  * ISTRUZIONI SALTATE          *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-w03    to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-492.
      *                              *---------------------------------*
      *                              * Incremento del numero di sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-494.
      *                              *---------------------------------*
      *                              * Memorizzazione della scadenza   *
      *                              * riemessa in coda del sub-buffer *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero scadenza, pari al    *
      *                                  * numero scadenza riemessa    *
      *                                  *-----------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Numero riscossione, o paga- *
      *                                  * mento, o compensazione, cui *
      *                                  * la scadenza e' stata sotto- *
      *                                  * posta                       *
      *                                  *-----------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Segnale di scadenza gia'    *
      *                                  * trattata, pari a No         *
      *                                  *-----------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Importo scadenza, pari al-  *
      *                                  * l'importo scadenza riemessa *
      *                                  *-----------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Coefficiente per la scaden- *
      *                                  * za, a seconda se e' stata   *
      *                                  * assorbita o no una parte    *
      *                                  * come riscossione, e comun-  *
      *                                  * que moltiplicato per il     *
      *                                  * coefficiente della scadenza *
      *                                  * nel sub-buffer in esame     *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w03    <    zero
                     move     w-det-nmp-iap-sbi
                             (w-det-nmp-iap-sby)
                                          to   w-det-nmp-iap-w02
                     divide   rf-sdb-imp-sdb
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-496.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-498.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-500.
      *                          *-------------------------------------*
      *                          * Se non c'e' stata riemissione di    *
      *                          * scadenza a fronte dello storno      *
      *                          *-------------------------------------*
       det-nmp-iap-502.
      *                              *---------------------------------*
      *                              * Accumulo importo totale della   *
      *                              * scadenza, moltiplicato per il   *
      *                              * coefficiente della scadenza,    *
      *                              * sul totale riscosso             *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-504.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-506.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-520.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 300 : Riscossione                       *
      *                      * 320 : Pagamento                         *
      *                      * 350 : Compensazione                     *
      *                      *-----------------------------------------*
       det-nmp-iap-522.
      *                          *-------------------------------------*
      *                          * Scansione di tutte le scadenze che  *
      *                          * sono state coinvolte nell'operazio- *
      *                          * ne, con :                           *
      *                          * - determinazione del totale degli   *
      *                          *   importi scadenza, di tipo posi-   *
      *                          *   tivo, di tutte le scadenze che    *
      *                          *   sono state coinvolte nell'opera-  *
      *                          *   zione                             *
      *                          * - determinazione del numero di sca- *
      *                          *   denze riemesse a fronte dell'ope- *
      *                          *   razione                           *
      *                          * - bufferizzazione dei numeri sca-   *
      *                          *   denza delle scadenze riemesse a   *
      *                          *   fronte dell'operazione            *
      *                          *-------------------------------------*
       det-nmp-iap-524.
      *                              *---------------------------------*
      *                              * Totale degli importi delle sca- *
      *                              * denze, di tipo positivo, coin-  *
      *                              * volte nell'operazione : pari i- *
      *                              * nizialmente all'importo della   *
      *                              * scadenza del sub-buffer in esa- *
      *                              * me                              *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w11      .
      *                              *---------------------------------*
      *                              * Numero di scadenze riemesse a   *
      *                              * fronte dell'operazione, ed im-  *
      *                              * messe nel sub-sub-buffer : a    *
      *                              * zero                            *
      *                              *---------------------------------*
           move      zero                 to   w-det-nmp-iap-ubx      .
       det-nmp-iap-526.
      *                              *---------------------------------*
      *                              * Start sulle scadenze coinvolte  *
      *                              * nell'operazione                 *
      *                              *---------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RISSDB    "         to   f-key                  .
           move      w-det-nmp-iap-sbp
                    (w-det-nmp-iap-sby)   to   rf-sdb-num-ris         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se start errata : a fine scan-  *
      *                              * sione scadenze coinvolte nella  *
      *                              * operazione                      *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-538.
       det-nmp-iap-528.
      *                              *---------------------------------*
      *                              * Next sulle scadenze coinvolte   *
      *                              * nell'operazione                 *
      *                              *---------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se fine file : a fine scansione *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-538.
       det-nmp-iap-530.
      *                              *---------------------------------*
      *                              * Test max su scadenze coinvolte  *
      *                              * nell'operazione, se non supera- *
      *                              * to : a fine scansione           *
      *                              *---------------------------------*
           if        rf-sdb-num-ris       not  = w-det-nmp-iap-sbp
                                                (w-det-nmp-iap-sby)
                     go to det-nmp-iap-538.
       det-nmp-iap-532.
      *                              *---------------------------------*
      *                              * Memorizzazione della scadenza   *
      *                              * riemessa a fronte di quella ap- *
      *                              * pena letta, se c'e' stata rie-  *
      *                              * missione                        *
      *                              *---------------------------------*
           if        rf-sdb-nns-ris       =    zero
                     go to det-nmp-iap-534.
      *                              *---------------------------------*
      *                              * Test se esistono scadenze che   *
      *                              * si riferiscono a quella in fase *
      *                              * di memorizzazione               *
      *                              * missione                        *
      *                              *---------------------------------*
           move      rf-sdb-nns-ris       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-rsc-000  thru det-nmp-iap-rsc-999    .
           if        w-det-nmp-iap-rc1    not  > 1
                     go to det-nmp-iap-533.
      *                              *---------------------------------*
      *                              * Se esistono scadenze che si     *
      *                              * si riferiscono a quella in fase *
      *                              * di memorizzazione si memoriz-   *
      *                              * zano in sua vece                *
      *                              *---------------------------------*
           move      rf-sdb-nns-ris       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-rss-000  thru det-nmp-iap-rss-999    .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     det-nmp-iap-534.
       det-nmp-iap-533.
           add       1                    to   w-det-nmp-iap-ubx      .
      *                              *---------------------------------*
      *                              * Scadenza in trattamento         *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbp
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-ubw      .
      *                              *---------------------------------*
      *                              * Test se overflow                *
      *                              *---------------------------------*
           if        w-det-nmp-iap-ubx    >    w-det-nmp-iap-ubm
                     perform   det-nmp-iap-mow-000  
                                          thru det-nmp-iap-mow-999
                     go to det-nmp-iap-538.
      *                              *---------------------------------*
      *                              * Scadenza normalizzata           *
      *                              *---------------------------------*
           move      rf-sdb-nns-ris       to   w-det-nmp-iap-ubn
                                              (w-det-nmp-iap-ubx)     .
           move      zero                 to   w-det-nmp-iap-ubp
                                              (w-det-nmp-iap-ubx)     .
           move      zero                 to   w-det-nmp-iap-ubi
                                              (w-det-nmp-iap-ubx)     .
       det-nmp-iap-534.
      *                              *---------------------------------*
      *                              * Accumulo del totale degli im-   *
      *                              * porti delle scadenze coinvolte  *
      *                              * nell'operazione, purche' l'im-  *
      *                              * porto scadenza sia positivo,    *
      *                              * la scadenza non sia di tipo ne- *
      *                              * gativo, e la scadenza non cor-  *
      *                              * risponda a quella in esame nel  *
      *                              * sub-buffer                      *
      *                              *---------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-536.
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-536.
           if        rf-sdb-num-sdb       =    w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sby)
                     go to det-nmp-iap-536.
           add       rf-sdb-imp-sdb       to   w-det-nmp-iap-w11      .
       det-nmp-iap-536.
      *                              *---------------------------------*
      *                              * Riciclo a leggere la scadenza   *
      *                              * successiva coinvolta nell'ope-  *
      *                              * razione di riscossione, o paga- *
      *                              * mento, o compensazione          *
      *                              *---------------------------------*
           go to     det-nmp-iap-528.
       det-nmp-iap-538.
      *                          *-------------------------------------*
      *                          * Determinazione del coefficiente di  *
      *                          * incidenza che ha l'importo della    *
      *                          * scadenza del sub-buffer in esame    *
      *                          * sul totale degli importi delle sca- *
      *                          * denze positive coinvolte nell'ope-  *
      *                          * razione                             *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w02      .
           divide    w-det-nmp-iap-w11    into w-det-nmp-iap-w02      .
           move      w-det-nmp-iap-w02    to   w-det-nmp-iap-w12      .
       det-nmp-iap-540.
      *                          *-------------------------------------*
      *                          * Scansione di tutte le scadenze che  *
      *                          * sono riemesse a fronte delle sca-   *
      *                          * denze che sono state coinvolte nel- *
      *                          * l'operazione, con:                  *
      *                          * - memorizzazione dell'importo della *
      *                          *   scadenza riemessa, purche' essa   *
      *                          *   sia di importo positivo, e pur-   *
      *                          *   che non sia di tipo negativo      *
      *                          * - memorizzazione del numero riscos- *
      *                          *   sione, o pagamento, o compensa-   *
      *                          *   zione, per ls scadenza riemessa   *
      *                          * - determinazione del totale degli   *
      *                          *   importi scadenza determinato alle *
      *                          *   condizioni sopra espresse         *
      *                          *-------------------------------------*
       det-nmp-iap-542.
      *                              *---------------------------------*
      *                              * Totale degli importi delle sca- *
      *                              * denze, di tipo positivo, rie-   *
      *                              * messe a fronte delle scadenze   *
      *                              * coinvolte nell'operazione : pa- *
      *                              * ri inizialmente a zero          *
      *                              *---------------------------------*
           move      zero                 to   w-det-nmp-iap-w13      .
      *                              *---------------------------------*
      *                              * Indice per la scansione del     *
      *                              * sub-sub-buffer : a zero         *
      *                              *---------------------------------*
           move      zero                 to   w-det-nmp-iap-uby      .
       det-nmp-iap-544.
      *                              *---------------------------------*
      *                              * Incremento indice per la scan-  *
      *                              * sione del sub-sub-buffer        *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-uby      .
      *                              *---------------------------------*
      *                              * Se oltre il numero di scadenze  *
      *                              * memorizzate nel sub-sub-buffer  *
      *                              * : a fine scansione              *
      *                              *---------------------------------*
           if        w-det-nmp-iap-uby    >    w-det-nmp-iap-ubx
                     go to det-nmp-iap-560.
       det-nmp-iap-546.
      *                              *---------------------------------*
      *                              * Lettura della scadenza riemessa *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      w-det-nmp-iap-ubn
                    (w-det-nmp-iap-uby)   to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se scadenza non trovata : la si *
      *                              * ignora, lasciando cosi' l'im-   *
      *                              * porto scadenza a zero ed il nu- *
      *                              * mero riscossione a zero         *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-544.
      *                              *---------------------------------*
      *                              * Se scadenza con importo non po- *
      *                              * sitivo : la si ignora, lascian- *
      *                              * do cosi' l'importo scadenza ed  *
      *                              * il numero riscossione a zero    *
      *                              *---------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-544.
      *                              *---------------------------------*
      *                              * Se scadenza di tipo negativo :  *
      *                              * la si ignora, lasciando cosi'   *
      *                              * l'importo scadenza ed il numero *
      *                              * riscossione a zero              *
      *                              *---------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-544.
       det-nmp-iap-548.
      *                              *---------------------------------*
      *                              * Memorizzazione dell'importo     *
      *                              * della scadenza                  *
      *                              *---------------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-ubi
                                              (w-det-nmp-iap-uby)     .
      *                              *---------------------------------*
      *                              * Memorizzazione del numero della *
      *                              * riscossione, o pagamento, o     *
      *                              * compensazione, cui la scadenza  *
      *                              * e' stata sottoposta             *
      *                              *---------------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-ubp
                                              (w-det-nmp-iap-uby)     .
      *                              *---------------------------------*
      *                              * Totalizzazione dell'importo     *
      *                              * della scadenza                  *
      *                              *---------------------------------*
           add       rf-sdb-imp-sdb       to   w-det-nmp-iap-w13      .
       det-nmp-iap-550.
      *                              *---------------------------------*
      *                              * Riciclo sulla scadenza succes-  *
      *                              * siva del sub-sub-buffer         *
      *                              *---------------------------------*
           go to     det-nmp-iap-544.
       det-nmp-iap-560.
      *                          *-------------------------------------*
      *                          * Determinazione della differenza tra *
      *                          * il totale degli importi delle sca-  *
      *                          * denze positive coinvolte nell'ope-  *
      *                          * razione, ed il totale degli importi *
      *                          * delle scadenze positive riemesse a  *
      *                          * fronte di tutte le scadenze coin-   *
      *                          * volte nell'operazione               *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-w11    to   w-det-nmp-iap-w14      .
           subtract  w-det-nmp-iap-w13    from w-det-nmp-iap-w14      .
       det-nmp-iap-562.
      *                          *-------------------------------------*
      *                          * Se c'e' una differenza essa costi-  *
      *                          * tuisce l'importo positivo assorbito *
      *                          * dall'operazione, e si accumula que- *
      *                          * sta differenza, moltiplicata per il *
      *                          * coefficiente della scadenza in esa- *
      *                          * me del sub-buffer, e poi moltipli-  *
      *                          * cata ancora per il coefficiente di  *
      *                          * incidenza della stessa sul totale   *
      *                          * positivo dell'operazione, sul tota- *
      *                          * le riscosso                         *
      *                          *-------------------------------------*
           if        w-det-nmp-iap-w14    not  > zero
                     go to det-nmp-iap-564.
           move      w-det-nmp-iap-w14    to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-w12    by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-564.
      *                          *-------------------------------------*
      *                          * Memorizzazione nel sub-buffer del-  *
      *                          * le scadenze positive riemesse a     *
      *                          * fronte delle scadenze coinvolte     *
      *                          * nell'operazione                     *
      *                          *-------------------------------------*
       det-nmp-iap-566.
      *                              *---------------------------------*
      *                              * Indice per la scansione del     *
      *                              * sub-sub-buffer : a zero         *
      *                              *---------------------------------*
           move      zero                 to   w-det-nmp-iap-uby      .
       det-nmp-iap-568.
      *                              *---------------------------------*
      *                              * Incremento indice per la scan-  *
      *                              * sione del sub-sub-buffer        *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-uby      .
      *                              *---------------------------------*
      *                              * Se oltre il numero di scadenze  *
      *                              * memorizzate nel sub-sub-buffer  *
      *                              * : a fine memorizzazione         *
      *                              *---------------------------------*
           if        w-det-nmp-iap-uby    >    w-det-nmp-iap-ubx
                     go to det-nmp-iap-576.
      *                              *---------------------------------*
      *                              * Se importo scadenza a zero : la *
      *                              * si ignora e si passa alla sca-  *
      *                              * denza riemessa successiva       *
      *                              *---------------------------------*
           if        w-det-nmp-iap-ubi
                    (w-det-nmp-iap-uby)   =    zero
                     go to det-nmp-iap-568.
       det-nmp-iap-570.
      *                              *---------------------------------*
      *                              * Incremento del numero di sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
      *                              *---------------------------------*
      *                              * Scadenza in trattamento         *
      *                              *---------------------------------*
           move      w-det-nmp-iap-ubn
                    (w-det-nmp-iap-uby)   to   w-det-nmp-iap-ubw      .
      *                              *---------------------------------*
      *                              * Test se overflow                *
      *                              *---------------------------------*
           if        w-det-nmp-iap-sbx    >    w-det-nmp-iap-ubm
                     perform   det-nmp-iap-mow-000
                                          thru det-nmp-iap-mow-999
                     go to det-nmp-iap-578.
      *                              *---------------------------------*
      *                              * Test se underflow               *
      *                              *---------------------------------*
           if        w-det-nmp-iap-sbx    =    zero or
                     w-det-nmp-iap-uby    =    zero
                     go to det-nmp-iap-578.
       det-nmp-iap-572.
      *                              *---------------------------------*
      *                              * Memorizzazione della scadenza   *
      *                              * riemessa in coda del sub-buffer *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero scadenza, pari al    *
      *                                  * numero scadenza riemessa    *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-ubn    
                    (w-det-nmp-iap-uby)   to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Numero riscossione, o paga- *
      *                                  * mento, o compensazione, pa- *
      *                                  * ri a quello della scadenza  *
      *                                  * riemessa                    *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-ubp    
                    (w-det-nmp-iap-uby)   to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Segnale di scadenza gia'    *
      *                                  * trattata, pari a No         *
      *                                  *-----------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Importo scadenza, pari al-  *
      *                                  * l'importo scadenza riemessa *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-ubi
                    (w-det-nmp-iap-uby)   to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Coefficiente per la scaden- *
      *                                  * za, a seconda se e' stata   *
      *                                  * assorbita o no una parte    *
      *                                  * come riscossione, e comun-  *
      *                                  * que moltiplicato per il     *
      *                                  * coefficiente della scadenza *
      *                                  * nel sub-buffer in esame e   *
      *                                  * per l'incidenza della sca-  *
      *                                  * denza del sub-buffer in e-  *
      *                                  * same sul totale positivo    *
      *                                  * dell'operazione             *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           multiply  w-det-nmp-iap-w12    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w14    <    zero
                     move     w-det-nmp-iap-w11
                                          to   w-det-nmp-iap-w02
                     divide   w-det-nmp-iap-w13
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-574.
      *                              *---------------------------------*
      *                              * Ad esame della scadenza riemes- *
      *                              * sa successiva                   *
      *                              *---------------------------------*
           go to     det-nmp-iap-568.
       det-nmp-iap-576.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-578.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-600.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 400 : Inclusione in distinta            *
      *                      * 500 : Presentazione distinta            *
      *                      * 540 : Accettazione  distinta            *
      *                      * 560 : Accredito     distinta            *
      *                      *-----------------------------------------*
       det-nmp-iap-602.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-604.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-620.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 600 : Insoluto                          *
      *                      *-----------------------------------------*
       det-nmp-iap-622.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se c'e' stata  *
      *                          * riemissione di scadenza oppure no   *
      *                          * a fronte dell'insoluto              *
      *                          *-------------------------------------*
           if        rf-sdb-ens-isp       =    02   or
                     rf-sdb-nns-isp       =    zero
                     go to det-nmp-iap-648.
       det-nmp-iap-624.
      *                          *-------------------------------------*
      *                          * Se c'e' stata riemissione di sca-   *
      *                          * denza a fronte dell'insoluto :      *
      *                          *                                     *
      *                          * si effettua una scansione preventi- *
      *                          * va del file [rsc] per verificare se *
      *                          * a fronte dello storno esistono piu' *
      *                          * scadenze di riferimento             *
      *                          *-------------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-rsc-000  thru det-nmp-iap-rsc-999    .
           if        w-det-nmp-iap-rc1    >    1
                     go to det-nmp-iap-625
           else      go to det-nmp-iap-626.
       det-nmp-iap-625.
      *                          *-------------------------------------*
      *                          * Se piu' scadenze trovate            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se esistono scadenze che si     *
      *                              * si riferiscono a quella in fase *
      *                              * di memorizzazione si memoriz-   *
      *                              * zano in sua vece                *
      *                              *---------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-rs1      .
           perform   det-nmp-iap-ins-000  thru det-nmp-iap-ins-999    .
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-626.
      *                          *-------------------------------------*
      *                          * In tutti gli altri casi             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura della scadenza riemessa *
      *                              * a fronte dell'insoluto          *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-sdb-nns-isp       to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se scadenza non esistente : si  *
      *                              * da' la scadenza originale per   *
      *                              * completamente riscossa          *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-640.
      *                              *---------------------------------*
      *                              * Se scadenza con importo non po- *
      *                              * sitivo : si da' la scadenza o-  *
      *                              * riginale per completamente ri-  *
      *                              * scossa                          *
      *                              *---------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-640.
      *                              *---------------------------------*
      *                              * Se scadenza di tipo negativo :  *
      *                              * si da' la scadenza originale    *
      *                              * per completamente riscossa      *
      *                              *---------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-640.
       det-nmp-iap-628.
      *                              *---------------------------------*
      *                              * Determinazione della differenza *
      *                              * tra l'importo scadenza in esame *
      *                              * nel sub-buffer e l'importo sca- *
      *                              * denza riemessa                  *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w03      .
           subtract  rf-sdb-imp-sdb       from w-det-nmp-iap-w03      .
       det-nmp-iap-630.
      *                              *---------------------------------*
      *                              * Se la scadenza riemessa e' di   *
      *                              * importo inferiore alla scadenza *
      *                              * in esame nel sub-buffer, si ac- *
      *                              * cumula la differenza, moltipli- *
      *                              * cata per il coefficiente della  *
      *                              * scadenza in esame, sul totale   *
      *                              * riscosso                        *
      *                              *---------------------------------*
           if        w-det-nmp-iap-w03    not  > zero
                     go to det-nmp-iap-632.
           move      w-det-nmp-iap-w03    to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-632.
      *                              *---------------------------------*
      *                              * Incremento del numero di sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-634.
      *                              *---------------------------------*
      *                              * Memorizzazione della scadenza   *
      *                              * riemessa in coda del sub-buffer *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero scadenza, pari al    *
      *                                  * numero scadenza riemessa    *
      *                                  *-----------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Numero riscossione, o paga- *
      *                                  * mento, o compensazione, cui *
      *                                  * la scadenza e' stata sotto- *
      *                                  * posta                       *
      *                                  *-----------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Segnale di scadenza gia'    *
      *                                  * trattata, pari a No         *
      *                                  *-----------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Importo scadenza, pari al-  *
      *                                  * l'importo scadenza riemessa *
      *                                  *-----------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Coefficiente per la scaden- *
      *                                  * za, a seconda se e' stata   *
      *                                  * assorbita o no una parte    *
      *                                  * come riscossione, e comun-  *
      *                                  * que moltiplicato per il     *
      *                                  * coefficiente della scadenza *
      *                                  * nel sub-buffer in esame     *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w03    <    zero
                     move     w-det-nmp-iap-sbi
                             (w-det-nmp-iap-sby)
                                          to   w-det-nmp-iap-w02
                     divide   rf-sdb-imp-sdb
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-636.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-638.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-640.
      *                          *-------------------------------------*
      *                          * Se la scadenza originale deve esse- *
      *                          * re data per completamente riscossa  *
      *                          *-------------------------------------*
       det-nmp-iap-642.
      *                              *---------------------------------*
      *                              * Si accumula l'importo scadenza  *
      *                              * in esame nel sub-buffer, molti- *
      *                              * plicato per il coefficiente     *
      *                              * della scadenza stessa, sul to-  *
      *                              * tale riscosso                   *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-644.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-646.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-648.
      *                          *-------------------------------------*
      *                          * Se non c'e' stata riemissione di    *
      *                          * scadenza a fronte dell'insoluto     *
      *                          *-------------------------------------*
       det-nmp-iap-650.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-652.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-660.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 620 : Richiamo                          *
      *                      *-----------------------------------------*
       det-nmp-iap-662.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se c'e' stata  *
      *                          * riemissione di scadenza oppure no   *
      *                          * a fronte del richiamo               *
      *                          *-------------------------------------*
           if        rf-sdb-ens-rsp       =    02   or
                     rf-sdb-nns-rsp       =    zero
                     go to det-nmp-iap-500.
       det-nmp-iap-664.
      *                          *-------------------------------------*
      *                          * Se c'e' stata riemissione di sca-   *
      *                          * denza a fronte del richiamo         *
      *                          *-------------------------------------*
       det-nmp-iap-666.
      *                              *---------------------------------*
      *                              * Lettura della scadenza riemessa *
      *                              * a fronte del richiamo           *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-sdb-nns-rsp       to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                              *---------------------------------*
      *                              * Se scadenza non esistente : co- *
      *                              * me per 'no riemissione' a fron- *
      *                              * te del richiamo                 *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-500.
      *                              *---------------------------------*
      *                              * Se scadenza con importo non po- *
      *                              * sitivo : come per 'no riemis-   *
      *                              * sione' a fronte del richiamo    *
      *                              *---------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-500.
      *                              *---------------------------------*
      *                              * Se scadenza di tipo negativo :  *
      *                              * come per 'no riemissione' a     *
      *                              * fronte del richiamo             *
      *                              *---------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-500.
       det-nmp-iap-668.
      *                              *---------------------------------*
      *                              * Determinazione della differenza *
      *                              * tra l'importo scadenza in esame *
      *                              * nel sub-buffer e l'importo sca- *
      *                              * denza riemessa                  *
      *                              *---------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w03      .
           subtract  rf-sdb-imp-sdb       from w-det-nmp-iap-w03      .
       det-nmp-iap-670.
      *                              *---------------------------------*
      *                              * Se la scadenza riemessa e' di   *
      *                              * importo inferiore alla scadenza *
      *                              * in esame nel sub-buffer, si ac- *
      *                              * cumula la differenza, moltipli- *
      *                              * cata per il coefficiente della  *
      *                              * scadenza in esame, sul totale   *
      *                              * riscosso                        *
      *                              *---------------------------------*
           if        w-det-nmp-iap-w03    not  > zero
                     go to det-nmp-iap-672.
           move      w-det-nmp-iap-w03    to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-672.
      *                              *---------------------------------*
      *                              * Incremento del numero di sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-674.
      *                              *---------------------------------*
      *                              * Memorizzazione della scadenza   *
      *                              * riemessa in coda del sub-buffer *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Numero scadenza, pari al    *
      *                                  * numero scadenza riemessa    *
      *                                  *-----------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Numero riscossione, o paga- *
      *                                  * mento, o compensazione, cui *
      *                                  * la scadenza e' stata sotto- *
      *                                  * posta                       *
      *                                  *-----------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Segnale di scadenza gia'    *
      *                                  * trattata, pari a No         *
      *                                  *-----------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Importo scadenza, pari al-  *
      *                                  * l'importo scadenza riemessa *
      *                                  *-----------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                                  *-----------------------------*
      *                                  * Coefficiente per la scaden- *
      *                                  * za, a seconda se e' stata   *
      *                                  * assorbita o no una parte    *
      *                                  * come riscossione, e comun-  *
      *                                  * que moltiplicato per il     *
      *                                  * coefficiente della scadenza *
      *                                  * nel sub-buffer in esame     *
      *                                  *-----------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w03    <    zero
                     move     w-det-nmp-iap-sbi
                             (w-det-nmp-iap-sby)
                                          to   w-det-nmp-iap-w02
                     divide   rf-sdb-imp-sdb
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-676.
      *                              *---------------------------------*
      *                              * Segnale di scadenza gia' trat-  *
      *                              * tata                            *
      *                              *---------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-678.
      *                              *---------------------------------*
      *                              * A riesame completo delle sca-   *
      *                              * denze nel sub-buffer            *
      *                              *---------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-700.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione eseguita sulla   *
      *                      * scadenza del sub-buffer in esame e' :   *
      *                      *                                         *
      *                      * 700 : Accredito al dopo incasso         *
      *                      * 720 : Notizia di buon esito             *
      *                      * 740 : Presunto buon esito               *
      *                      *-----------------------------------------*
       det-nmp-iap-702.
      *                          *-------------------------------------*
      *                          * Si accumula l'importo scadenza in   *
      *                          * esame nel sub-buffer, moltiplicata  *
      *                          * per il coefficiente della scadenza  *
      *                          * stessa, sul totale riscosso         *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbi
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-w01      .
           multiply  w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   by   w-det-nmp-iap-w01      .
           add       w-det-nmp-iap-w01    to   w-det-nmp-iap-sbt      .
       det-nmp-iap-704.
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata   *
      *                          *-------------------------------------*
           move      "#"                  to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sby)     .
       det-nmp-iap-706.
      *                          *-------------------------------------*
      *                          * A riesame completo delle scadenze   *
      *                          * nel sub-buffer                      *
      *                          *-------------------------------------*
           go to     det-nmp-iap-380.
       det-nmp-iap-750.
      *                      *-----------------------------------------*
      *                      * Memorizzazione del totale riscosso a    *
      *                      * fronte della scadenza                   *
      *                      *-----------------------------------------*
           move      w-det-nmp-iap-sbt    to   w-det-nmp-iap-bor
                                              (w-det-nmp-iap-boy)     .
       det-nmp-iap-775.
      *                  *---------------------------------------------*
      *                  * Riciclo alla determinazione del totale ri-  *
      *                  * scosso a fronte della scadenza emessa in    *
      *                  * origine successiva                          *
      *                  *---------------------------------------------*
           go to     det-nmp-iap-300.
       det-nmp-iap-800.
      *              *-------------------------------------------------*
      *              * Sommatoria conclusiva dopo il ciclo per la de-  *
      *              * terminazione dell'importo incassato a fronte    *
      *              * di ogni singola scadenza emessa in origine      *
      *              *-------------------------------------------------*
       det-nmp-iap-810.
      *                  *---------------------------------------------*
      *                  * Determinazione totale incasso reale         *
      *                  *---------------------------------------------*
       det-nmp-iap-811.
      *                      *-----------------------------------------*
      *                      * Indice su scadenze emesse in origine :  *
      *                      * a zero                                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-nmp-iap-boy      .
       det-nmp-iap-812.
      *                      *-----------------------------------------*
      *                      * Incremento indice su scadenze emesse in *
      *                      * origine                                 *
      *                      *-----------------------------------------*
           add       1                    to   w-det-nmp-iap-boy      .
      *                      *-----------------------------------------*
      *                      * Se oltre il numero scadenze emesse in   *
      *                      * origine caricate : a determinazione     *
      *                      * incasso totale                          *
      *                      *-----------------------------------------*
           if        w-det-nmp-iap-boy    >    w-det-nmp-iap-box
                     go to det-nmp-iap-820.
      *                      *-----------------------------------------*
      *                      * Accumulo dell'importo incassato a fron- *
      *                      * te della scadenza totale incasso reale  *
      *                      *-----------------------------------------*
           add       w-det-nmp-iap-bor
                    (w-det-nmp-iap-boy)   to   w-det-nmp-iap-irl      .
      *                      *-----------------------------------------*
      *                      * Riciclo su scadenza emessa in origine   *
      *                      * successiva                              *
      *                      *-----------------------------------------*
           go to     det-nmp-iap-812.
       det-nmp-iap-820.
      *                  *---------------------------------------------*
      *                  * Determinazione del totale incasso completo  *
      *                  * come somma tra :                            *
      *                  *  - Incasso presunto per differenza tra to-  *
      *                  *    tale fattura e totale scadenze emesse    *
      *                  *    in origine                               *
      *                  *  - Incasso reale su scadenze emesse in ori- *
      *                  *    gine                                     *
      *                  *---------------------------------------------*
           move      w-det-nmp-iap-ipd    to   w-det-nmp-iap-inc      .
           add       w-det-nmp-iap-irl    to   w-det-nmp-iap-inc      .
       det-nmp-iap-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-999.
       det-nmp-iap-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Subroutine di message overflow                            *
      *    *-----------------------------------------------------------*
       det-nmp-iap-mow-000.
      *              *-------------------------------------------------*
      *              * Editing numero scadenza                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-det-nmp-iap-ubw    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Composizione messaggio                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Superamento tabella riferimenti per la scadenza "
                                          to   w-all-str-cat (01)     .
           move      p-edt                to   w-all-str-cat (02)     .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *              *-------------------------------------------------*
      *              * Emissione del messaggio di anomalia             *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      w-all-str-alf        to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       det-nmp-iap-mow-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-mow-999.
       det-nmp-iap-mow-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Subroutine di pre-scansione file [rsc] di appoggio per il *
      *    * riferimento di piu' scadenze ad una sola di origine       *
      *    *-----------------------------------------------------------*
       det-nmp-iap-rsc-000.
      *              *-------------------------------------------------*
      *              * Numero di scadenze rilevate : a zero            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nmp-iap-rc1      .
       det-nmp-iap-rsc-100.
      *              *-------------------------------------------------*
      *              * Start sulle scadenze coinvolte nell'operazione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SDBNSC    "         to   f-key                  .
           move      w-det-nmp-iap-rs1    to   rf-rsc-nsc-org         .
           move      zero                 to   rf-rsc-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-rsc-900.
       det-nmp-iap-rsc-200.
      *              *-------------------------------------------------*
      *              * Next sulle scadenze coinvolte nell'operazione   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-rsc-900.
       det-nmp-iap-rsc-400.
      *              *-------------------------------------------------*
      *              * Test max sulle scadenze coinvolte nell'opera-   *
      *              * zione, se non superato : ad uscita              *
      *              *-------------------------------------------------*
           if        rf-rsc-nsc-org       not  = w-det-nmp-iap-rs1
                     go to det-nmp-iap-rsc-900.
       det-nmp-iap-rsc-600.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-nmp-iap-rc1      .
       det-nmp-iap-rsc-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-rsc-200.
       det-nmp-iap-rsc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-rsc-999.
       det-nmp-iap-rsc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Subroutine di bufferizzazione delle scadenze con rife-    *
      *    * rimento a quella in corso di trattamento                  *
      *    *-----------------------------------------------------------*
       det-nmp-iap-rss-000.
      *              *-------------------------------------------------*
      *              * Start sulle scadenze coinvolte nell'operazione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SDBNSC    "         to   f-key                  .
           move      w-det-nmp-iap-rs1    to   rf-rsc-nsc-org         .
           move      zero                 to   rf-rsc-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-rss-900.
       det-nmp-iap-rss-200.
      *              *-------------------------------------------------*
      *              * Next sulle scadenze coinvolte nell'operazione   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-rss-900.
       det-nmp-iap-rss-400.
      *              *-------------------------------------------------*
      *              * Test max sulle scadenze coinvolte nell'opera-   *
      *              * zione, se non superato : ad uscita              *
      *              *-------------------------------------------------*
           if        rf-rsc-nsc-org       not  = w-det-nmp-iap-rs1
                     go to det-nmp-iap-rss-900.
       det-nmp-iap-rss-600.
      *              *-------------------------------------------------*
      *              * Memorizzazione                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-det-nmp-iap-ubx      .
           move      rf-rsc-num-sdb       to   w-det-nmp-iap-ubn
                                              (w-det-nmp-iap-ubx)     .
           move      zero                 to   w-det-nmp-iap-ubp
                                              (w-det-nmp-iap-ubx)     .
           move      zero                 to   w-det-nmp-iap-ubi
                                              (w-det-nmp-iap-ubx)     .
       det-nmp-iap-rss-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-rss-200.
       det-nmp-iap-rss-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-rss-999.
       det-nmp-iap-rss-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Storno di scadenza                                        *
      *    *                                                           *
      *    * Subroutine di bufferizzazione delle scadenze con rife-    *
      *    * rimento a quella in corso di trattamento                  *
      *    *-----------------------------------------------------------*
       det-nmp-iap-sto-000.
      *              *-------------------------------------------------*
      *              * Start sulle scadenze coinvolte nell'operazione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SDBNSC    "         to   f-key                  .
           move      w-det-nmp-iap-rs1    to   rf-rsc-nsc-org         .
           move      zero                 to   rf-rsc-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-sto-900.
       det-nmp-iap-sto-200.
      *              *-------------------------------------------------*
      *              * Next sulle scadenze coinvolte nell'operazione   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-sto-900.
       det-nmp-iap-sto-400.
      *              *-------------------------------------------------*
      *              * Test max sulle scadenze coinvolte nell'opera-   *
      *              * zione, se non superato : ad uscita              *
      *              *-------------------------------------------------*
           if        rf-rsc-nsc-org       not  = w-det-nmp-iap-rs1
                     go to det-nmp-iap-sto-900.
       det-nmp-iap-sto-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione [sdb]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Lettura della scadenza riemessa a fronte dello  *
      *              * storno                                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-rsc-num-sdb       to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se scadenza non esistente : a riciclo       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-sto-200.
      *                  *---------------------------------------------*
      *                  * Se scadenza con importo non positivo : a    *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-sto-200.
      *                  *---------------------------------------------*
      *                  * Se scadenza di tipo negativo : a riciclo    *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-sto-200.
      *                  *---------------------------------------------*
      *                  * Incremento del numero di scadenze nel sub-  *
      *                  * buffer                                      *
      *                  *---------------------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-sto-620.
      *                      *-----------------------------------------*
      *                      * Memorizzazione della scadenza riemessa  *
      *                      * in coda del sub-buffer                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero scadenza, pari al numero     *
      *                          * scadenza riemessa                   *
      *                          *-------------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Numero riscossione, o pagamento, o  *
      *                          * compensazione, cui la scadenza e'   *
      *                          * stata sottoposta                    *
      *                          *-------------------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata,  *
      *                          * pari a No                           *
      *                          *-------------------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Importo scadenza, pari all'importo  *
      *                          * scadenza riemessa                   *
      *                          *-------------------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Coefficiente per la scadenza, a     *
      *                          * seconda se e' stata assorbita o no  *
      *                          * una parte come riscossione, e co-   *
      *                          * munque moltiplicato per il coef-    *
      *                          * ficiente della scadenza nel sub-    *
      *                          * buffer in esame                     *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w03    <    zero
                     move     w-det-nmp-iap-sbi
                             (w-det-nmp-iap-sby)
                                          to   w-det-nmp-iap-w02
                     divide   rf-sdb-imp-sdb
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-sto-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-sto-200.
       det-nmp-iap-sto-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-sto-999.
       det-nmp-iap-sto-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle nuove maturazioni di provvigione     *
      *    *                                                           *
      *    * Insoluto                                                  *
      *    *                                                           *
      *    * Subroutine di bufferizzazione delle scadenze con rife-    *
      *    * rimento a quella in corso di trattamento                  *
      *    *-----------------------------------------------------------*
       det-nmp-iap-ins-000.
      *              *-------------------------------------------------*
      *              * Start sulle scadenze coinvolte nell'operazione  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "SDBNSC    "         to   f-key                  .
           move      w-det-nmp-iap-rs1    to   rf-rsc-nsc-org         .
           move      zero                 to   rf-rsc-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-ins-900.
       det-nmp-iap-ins-200.
      *              *-------------------------------------------------*
      *              * Next sulle scadenze coinvolte nell'operazione   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-ins-900.
       det-nmp-iap-ins-400.
      *              *-------------------------------------------------*
      *              * Test max sulle scadenze coinvolte nell'opera-   *
      *              * zione, se non superato : ad uscita              *
      *              *-------------------------------------------------*
           if        rf-rsc-nsc-org       not  = w-det-nmp-iap-rs1
                     go to det-nmp-iap-ins-900.
       det-nmp-iap-ins-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione [sdb]                           *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Lettura della scadenza riemessa a fronte dello  *
      *              * storno                                          *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSDB    "         to   f-key                  .
           move      rf-rsc-num-sdb       to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se scadenza non esistente : a riciclo       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-nmp-iap-ins-200.
      *                  *---------------------------------------------*
      *                  * Se scadenza con importo non positivo : a    *
      *                  * riciclo                                     *
      *                  *---------------------------------------------*
           if        rf-sdb-imp-sdb       not  > zero
                     go to det-nmp-iap-ins-200.
      *                  *---------------------------------------------*
      *                  * Se scadenza di tipo negativo : a riciclo    *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-ddr       =    11 or
                     rf-sdb-tip-ddr       =    21
                     go to det-nmp-iap-ins-200.
      *                  *---------------------------------------------*
      *                  * Incremento del numero di scadenze nel sub-  *
      *                  * buffer                                      *
      *                  *---------------------------------------------*
           add       1                    to   w-det-nmp-iap-sbx      .
       det-nmp-iap-ins-620.
      *                      *-----------------------------------------*
      *                      * Memorizzazione della scadenza riemessa  *
      *                      * in coda del sub-buffer                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero scadenza, pari al numero     *
      *                          * scadenza riemessa                   *
      *                          *-------------------------------------*
           move      rf-sdb-num-sdb       to   w-det-nmp-iap-sbn
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Numero riscossione, o pagamento, o  *
      *                          * compensazione, cui la scadenza e'   *
      *                          * stata sottoposta                    *
      *                          *-------------------------------------*
           move      rf-sdb-num-ris       to   w-det-nmp-iap-sbp
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Segnale di scadenza gia' trattata,  *
      *                          * pari a No                           *
      *                          *-------------------------------------*
           move      spaces               to   w-det-nmp-iap-sbf
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Importo scadenza, pari all'importo  *
      *                          * scadenza riemessa                   *
      *                          *-------------------------------------*
           move      rf-sdb-imp-sdb       to   w-det-nmp-iap-sbi
                                              (w-det-nmp-iap-sbx)     .
      *                          *-------------------------------------*
      *                          * Coefficiente per la scadenza, a     *
      *                          * seconda se e' stata assorbita o no  *
      *                          * una parte come riscossione, e co-   *
      *                          * munque moltiplicato per il coef-    *
      *                          * ficiente della scadenza nel sub-    *
      *                          * buffer in esame                     *
      *                          *-------------------------------------*
           move      w-det-nmp-iap-sbc
                    (w-det-nmp-iap-sby)   to   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
           if        w-det-nmp-iap-w03    <    zero
                     move     w-det-nmp-iap-sbi
                             (w-det-nmp-iap-sby)
                                          to   w-det-nmp-iap-w02
                     divide   rf-sdb-imp-sdb
                                          into w-det-nmp-iap-w02
           else      move     1           to   w-det-nmp-iap-w02      .
           multiply  w-det-nmp-iap-w02    by   w-det-nmp-iap-sbc
                                              (w-det-nmp-iap-sbx)     .
       det-nmp-iap-ins-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-ins-200.
       det-nmp-iap-ins-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-nmp-iap-ins-999.
       det-nmp-iap-ins-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'assestamento degli importi relativi ad   *
      *    * un conteggio provvigionale                                *
      *    *                                                           *
      *    * Input  : w-ass-ctg-tip-ctg = Tipo di conteggio            *
      *    *                                                           *
      *    *          w-ass-ctg-ibl-pvg = Imponibile provvigionale     *
      *    *                                                           *
      *    *          w-ass-ctg-amm-pvg = Ammontare provvigione        *
      *    *                                                           *
      *    *          w-ass-ctg-imp-doc = Totale documento             *
      *    *                                                           *
      *    *          w-ass-ctg-imp-agf = Totale acconti fatturati     *
      *    *                                                           *
      *    * Output : w-ass-ctg-ibl-pvg = Imponibile provvigionale,    *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-ctg-amm-pvg = Ammontare provvigione,       *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-ctg-imp-doc = Totale documento,            *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-ctg-imp-agf = Totale acconti fatturati,    *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-ctg-inf-som = Segnale se importo conteggio *
      *    *                              influente o no sulle tota-   *
      *    *                              lizzazioni di conteggio      *
      *    *                               - S : Si                    *
      *    *                               - N : No                    *
      *    *-----------------------------------------------------------*
       ass-age-ctg-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di conteggio      *
      *              *-------------------------------------------------*
           if        w-ass-ctg-tip-ctg    =    01 or
                     w-ass-ctg-tip-ctg    =    02
                     go to ass-age-ctg-100
           else if   w-ass-ctg-tip-ctg    =    03 or
                     w-ass-ctg-tip-ctg    =    11
                     go to ass-age-ctg-300
           else      go to ass-age-ctg-500.
       ass-age-ctg-100.
      *              *-------------------------------------------------*
      *              * Se conteggio a fronte fattura o nota di addebi- *
      *              * to                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Influenza degli importi del conteggio sulle *
      *                  * totalizzazioni : Si                         *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ass-ctg-inf-som      .
      *                  *---------------------------------------------*
      *                  * Uscita lasciando gli importi positivi       *
      *                  *---------------------------------------------*
           go to     ass-age-ctg-900.
       ass-age-ctg-300.
      *              *-------------------------------------------------*
      *              * Se conteggio a fronte nota di accredito         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Influenza degli importi del conteggio sulle *
      *                  * totalizzazioni : Si                         *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ass-ctg-inf-som      .
      *                  *---------------------------------------------*
      *                  * Inversione del segno algebrico degli impor- *
      *                  * ti                                          *
      *                  *---------------------------------------------*
           multiply  -1                   by   w-ass-ctg-ibl-pvg      .
           multiply  -1                   by   w-ass-ctg-amm-pvg      .
           multiply  -1                   by   w-ass-ctg-imp-doc      .
           multiply  -1                   by   w-ass-ctg-imp-agf      .
      *                  *---------------------------------------------*
      *                  * Uscita con gli importi negativi             *
      *                  *---------------------------------------------*
           go to     ass-age-ctg-900.
       ass-age-ctg-500.
      *              *-------------------------------------------------*
      *              * Se tipo di conteggio non riconosciuto           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Influenza degli importi del conteggio sulle *
      *                  * totalizzazioni : No                         *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ass-ctg-inf-som      .
      *                  *---------------------------------------------*
      *                  * Uscita lasciando gli importi positivi       *
      *                  *---------------------------------------------*
           go to     ass-age-ctg-900.
       ass-age-ctg-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ass-age-ctg-999.
       ass-age-ctg-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per l'assestamento degli importi relativi ad   *
      *    * una maturazione su di un conteggio provvigionale          *
      *    *                                                           *
      *    * Input  : w-ass-mtz-tip-ctg = Tipo di conteggio            *
      *    *                                                           *
      *    *          w-ass-mtz-tip-mtz = Tipo di maturazione          *
      *    *                                                           *
      *    *          w-ass-mtz-imp-mtz = Importo della maturazione    *
      *    *                                                           *
      *    *          w-ass-mtz-imp-inc = Importo incassato che ha de- *
      *    *                              terminato la maturazione     *
      *    *                                                           *
      *    * Output : w-ass-mtz-imp-mtz = Importo della maturazione,   *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-mtz-imp-inc = Importo incassato che ha de- *
      *    *                              terminato la maturazione,    *
      *    *                              assestato                    *
      *    *                                                           *
      *    *          w-ass-mtz-inf-som = Segnale se importo matura-   *
      *    *                              zione influente o no sulle   *
      *    *                              totalizzazioni di matura-    *
      *    *                              zione                        *
      *    *                               - S : Si                    *
      *    *                               - N : No                    *
      *    *-----------------------------------------------------------*
       ass-age-mtz-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di conteggio      *
      *              *-------------------------------------------------*
           if        w-ass-mtz-tip-ctg    =    01 or
                     w-ass-mtz-tip-ctg    =    02
                     go to ass-age-mtz-100
           else if   w-ass-mtz-tip-ctg    =    03 or
                     w-ass-ctg-tip-ctg    =    11
                     go to ass-age-mtz-300
           else      go to ass-age-mtz-500.
       ass-age-mtz-100.
      *              *-------------------------------------------------*
      *              * Se conteggio a fronte fattura o nota di addebi- *
      *              * to                                              *
      *              *-------------------------------------------------*
       ass-age-mtz-125.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di maturazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-ass-mtz-tip-mtz    =    01 or
                     w-ass-mtz-tip-mtz    =    02
                     go to ass-age-mtz-150
           else if   w-ass-mtz-tip-mtz    =    51
                     go to ass-age-mtz-175
           else      go to ass-age-mtz-200.
       ass-age-mtz-150.
      *                  *---------------------------------------------*
      *                  * Se maturazione immediata o a fronte incasso *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : Si                              *
      *                      *-----------------------------------------*
           move      "S"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Uscita lasciando gli importi positivi   *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-175.
      *                  *---------------------------------------------*
      *                  * Se storno di provvigione                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : No                              *
      *                      *-----------------------------------------*
           move      "N"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Inversione del segno algebrico degli    *
      *                      * importi                                 *
      *                      *-----------------------------------------*
           multiply  -1                   by   w-ass-mtz-imp-mtz      .
           multiply  -1                   by   w-ass-mtz-imp-inc      .
      *                      *-----------------------------------------*
      *                      * Uscita con gli importi negativi         *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-200.
      *                  *---------------------------------------------*
      *                  * Se tipo di maturazione non riconosciuto     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : No                              *
      *                      *-----------------------------------------*
           move      "N"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Uscita lasciando gli importi positivi   *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-300.
      *              *-------------------------------------------------*
      *              * Se conteggio a fronte nota di accredito         *
      *              *-------------------------------------------------*
       ass-age-mtz-325.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo di maturazio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        w-ass-mtz-tip-mtz    =    01 or
                     w-ass-mtz-tip-mtz    =    02
                     go to ass-age-mtz-350
           else if   w-ass-mtz-tip-mtz    =    51
                     go to ass-age-mtz-375
           else      go to ass-age-mtz-400.
       ass-age-mtz-350.
      *                  *---------------------------------------------*
      *                  * Se maturazione immediata o a fronte incasso *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : Si                              *
      *                      *-----------------------------------------*
           move      "S"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Inversione del segno algebrico degli    *
      *                      * importi                                 *
      *                      *-----------------------------------------*
           multiply  -1                   by   w-ass-mtz-imp-mtz      .
           multiply  -1                   by   w-ass-mtz-imp-inc      .
      *                      *-----------------------------------------*
      *                      * Uscita con gli importi negativi         *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-375.
      *                  *---------------------------------------------*
      *                  * Se storno di provvigione                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : No                              *
      *                      *-----------------------------------------*
           move      "N"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Uscita lasciando gli importi positivi   *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-400.
      *                  *---------------------------------------------*
      *                  * Se tipo di maturazione non riconosciuto     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Influenza degli importi della matura-   *
      *                      * zione sulle totalizzazioni di matura-   *
      *                      * zione : No                              *
      *                      *-----------------------------------------*
           move      "N"                  to   w-ass-mtz-inf-som      .
      *                      *-----------------------------------------*
      *                      * Inversione del segno algebrico degli    *
      *                      * importi                                 *
      *                      *-----------------------------------------*
           multiply  -1                   by   w-ass-mtz-imp-mtz      .
           multiply  -1                   by   w-ass-mtz-imp-inc      .
      *                      *-----------------------------------------*
      *                      * Uscita con gli importi negativi         *
      *                      *-----------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-500.
      *              *-------------------------------------------------*
      *              * Se tipo di conteggio non riconosciuto           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Influenza degli importi della maturazione   *
      *                  * sulle totalizzazioni di maturazione : No    *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ass-mtz-inf-som      .
      *                  *---------------------------------------------*
      *                  * Uscita lasciando gli importi positivi       *
      *                  *---------------------------------------------*
           go to     ass-age-mtz-900.
       ass-age-mtz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ass-age-mtz-999.
       ass-age-mtz-999.
           exit.

      *    *===========================================================*
      *    * Determinazione dell'ultima operazione eseguita su di una  *
      *    * scadenza, contenuta in 'rf-sdb', con riferimento ad una   *
      *    * certa data                                                *
      *    *                                                           *
      *    * Input  : w-sts-sdb-tip-det = Tipo determinazione          *
      *    *                                                           *
      *    *          w-sts-sdb-dat-rif = Data riferimento             *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-sts-sdb-ult-ope = Ultima operazione determina- *
      *    *                              ta                           *
      *    *                                                           *
      *    *          w-sts-sdb-dat-ril = Data ultima operazione de-   *
      *    *                              ta                           *
      *    *                                                           *
      *    * Legenda codici ultima operazione:                         *
      *    *                                                           *
      *    * '000' = Nessuna operazione                                *
      *    * '100' = Emissione                                         *
      *    * '200' = Storno                                            *
      *    * '300' = Riscossione                                       *
      *    * '320' = Pagamento                                         *
      *    * '350' = Compensazione                                     *
      *    * '400' = Inclusione in distinta                            *
      *    * '500' = Presentazione distinta                            *
      *    * '540' = Accettazione distinta                             *
      *    * '560' = Accredito distinta                                *
      *    * '600' = Insoluto                                          *
      *    * '620' = Richiamo                                          *
      *    * '700' = Accredito al dopo incasso                         *
      *    * '720' = Notizia di buon esito                             *
      *    * '740' = Presunto buon esito                               *
      *    *-----------------------------------------------------------*
       det-sts-sdb-000.
      *              *-------------------------------------------------*
      *              * Nessuna operazione                              *
      *              *-------------------------------------------------*
           move      000                  to   w-sts-sdb-ult-ope      .
      *              *-------------------------------------------------*
      *              * Data rilevamento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-sts-sdb-dat-ril      .
       det-sts-sdb-050.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad uscita               *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-emi       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad uscita                           *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-emi       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : emissione               *
      *                  *---------------------------------------------*
           move      100                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-emi       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-100.
       det-sts-sdb-100.
      *              *-------------------------------------------------*
      *              * Storno                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-sto       =    zero
                     go to det-sts-sdb-150.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad uscita                           *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-sto       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : storno                  *
      *                  *---------------------------------------------*
           move      200                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-sto       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-150.
      *              *-------------------------------------------------*
      *              * Riscossione, o pagamento, o compensazione       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-ris       =    zero
                     go to det-sts-sdb-200.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione successiva                            *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-ris       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-200.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : riscossione, o pagamen- *
      *                  * to, o compensazione, a seconda della moda-  *
      *                  * lita' di riscossione della scadenza         *
      *                  *---------------------------------------------*
           if        rf-sdb-mod-ris       =    01 or
                     rf-sdb-mod-ris       =    02 or
                     rf-sdb-mod-ris       =    03 or
                     rf-sdb-mod-ris       =    04
                     move  300            to   w-sts-sdb-ult-ope
           else if   rf-sdb-mod-ris       =    21 or
                     rf-sdb-mod-ris       =    22 or
                     rf-sdb-mod-ris       =    23 or
                     rf-sdb-mod-ris       =    24
                     move  320            to   w-sts-sdb-ult-ope
           else if   rf-sdb-mod-ris       =    50
                     move  350            to   w-sts-sdb-ult-ope
           else      move  300            to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-ris       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-200.
      *              *-------------------------------------------------*
      *              * Insoluto                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-isp       =    zero
                     go to det-sts-sdb-250.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione di inclusione in distinta             *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-isp       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : insoluto                *
      *                  *---------------------------------------------*
           move      600                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-isp       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-250.
      *              *-------------------------------------------------*
      *              * Richiamo                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-rsp       =    zero
                     go to det-sts-sdb-300.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione di inclusione in distinta             *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-rsp       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : richiamo                *
      *                  *---------------------------------------------*
           move      620                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-rsp       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-300.
      *              *-------------------------------------------------*
      *              * Accredito al dopo incasso                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-acs       =    zero
                     go to det-sts-sdb-350.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione di inclusione in distinta             *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-acs       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : accredito dopo incasso  *
      *                  *---------------------------------------------*
           move      700                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-acs       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-350.
      *              *-------------------------------------------------*
      *              * Notizia di buon esito                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-nbe       =    zero
                     go to det-sts-sdb-400.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione di inclusione in distinta             *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-nbe       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : notizia di buon esito   *
      *                  *---------------------------------------------*
           move      720                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-nbe       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-400.
      *              *-------------------------------------------------*
      *              * Presunto buon esito                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' a zero : ad esecuzione control-  *
      *                  * lo su operazione successiva                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-pbe       =    zero
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione e' successiva alla data di riferi-  *
      *                  * mento : ad esecuzione controllo su opera-   *
      *                  * zione di inclusione in distinta             *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-pbe       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-500.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : presunto buon esito     *
      *                  *---------------------------------------------*
           move      740                  to   w-sts-sdb-ult-ope      .
           move      rf-sdb-dtr-pbe       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-500.
      *              *-------------------------------------------------*
      *              * Inclusione in distinta                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se il tipo di determinazione indica di non  *
      *                  * considerare le distinte di presentazione :  *
      *                  * ad uscita                                   *
      *                  *---------------------------------------------*
           if        w-sts-sdb-tip-det    =    02
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se numero distinta di presentazione a zero  *
      *                  * : ad uscita                                 *
      *                  *---------------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Lettura distinta di presentazione           *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rf-sdb-num-ddp       to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *                  *---------------------------------------------*
      *                  * Se distinta di presentazione non trovata :  *
      *                  * ad uscita                                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di composizione distinta e' a zero  *
      *                  * : ad uscita                                 *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-com       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di composizione distinta e' succes- *
      *                  * siva alla data di riferimento : ad uscita   *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-com       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : inclusione in distinta  *
      *                  *---------------------------------------------*
           move      400                  to   w-sts-sdb-ult-ope      .
           move      rf-ddp-dtr-com       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-550.
       det-sts-sdb-550.
      *              *-------------------------------------------------*
      *              * Presentazione distinta                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di presentazione distinta e' a zero *
      *                  * : ad uscita                                 *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-pre       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di composizione distinta e' succes- *
      *                  * siva alla data di riferimento : ad uscita   *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-pre       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : presentazione distinta  *
      *                  *---------------------------------------------*
           move      500                  to   w-sts-sdb-ult-ope      .
           move      rf-ddp-dtr-pre       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-600.
       det-sts-sdb-600.
      *              *-------------------------------------------------*
      *              * Accettazione distinta                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di accettazione distinta e' a zero  *
      *                  * : a controllo su operazione successiva      *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-act       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di accettazione distinta e' succes- *
      *                  * siva alla data di riferimento : ad uscita   *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-act       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : accettazione distinta   *
      *                  *---------------------------------------------*
           move      540                  to   w-sts-sdb-ult-ope      .
           move      rf-ddp-dtr-act       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-650.
       det-sts-sdb-650.
      *              *-------------------------------------------------*
      *              * Accredito distinta                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di accredito distinta e' a zero :   *
      *                  * ad uscita                                   *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-acd       =    zero
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Se la data registrazione relativa all'ope-  *
      *                  * razione di accredito distinta e' successi-  *
      *                  * va alla data di riferimento : ad uscita     *
      *                  *---------------------------------------------*
           if        rf-ddp-dtr-acd       >    w-sts-sdb-dat-rif
                     go to det-sts-sdb-900.
      *                  *---------------------------------------------*
      *                  * Ultima operazione : accredito distinta      *
      *                  *---------------------------------------------*
           move      560                  to   w-sts-sdb-ult-ope      .
           move      rf-ddp-dtr-acd       to   w-sts-sdb-dat-ril      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-sdb-900.
       det-sts-sdb-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sts-sdb-999.
       det-sts-sdb-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio anagrafica agenti             *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    w-let-arc-age-exc
                     go to let-arc-age-900.
       let-arc-age-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [age] relativamente all'agente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-age-200
           else      go to let-arc-age-300.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente esistente                  *
      *              *-------------------------------------------------*
       let-arc-age-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-age-via-age       to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-age-loc-age       to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-let-arc-age-mne      .
       let-arc-age-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente non esistente              *
      *              *-------------------------------------------------*
       let-arc-age-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-800.
      *              *-------------------------------------------------*
      *              * Se codice agente a zero                         *
      *              *-------------------------------------------------*
       let-arc-age-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-age-mne      .
       let-arc-age-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-age-900.
       let-arc-age-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente su valore precedente          *
      *                  *---------------------------------------------*
           move      w-let-arc-age-cod    to   w-let-arc-age-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio anagrafica agenti subordina-  *
      *    * ti                                                        *
      *    *-----------------------------------------------------------*
       let-arc-ags-000.
      *              *-------------------------------------------------*
      *              * Test se codice agente a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-ags-cod    =    zero
                     go to let-arc-ags-800.
      *              *-------------------------------------------------*
      *              * Test se codice pari al valore precedente        *
      *              *-------------------------------------------------*
           if        w-let-arc-ags-cod    =    w-let-arc-ags-exc
                     go to let-arc-ags-900.
       let-arc-ags-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [age] relativamente all'agente *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE    "         to   f-key                  .
           move      w-let-arc-ags-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-ags-200
           else      go to let-arc-ags-300.
       let-arc-ags-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente esistente                  *
      *              *-------------------------------------------------*
       let-arc-ags-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      rf-age-nom-age       to   w-let-arc-ags-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-ags-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-age-via-age       to   w-let-arc-ags-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-age-loc-age       to   w-let-arc-ags-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      rf-age-cod-mne       to   w-let-arc-ags-mne      .
       let-arc-ags-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-ags-900.
       let-arc-ags-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica agente non esistente              *
      *              *-------------------------------------------------*
       let-arc-ags-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-ags-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-ags-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-ags-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-ags-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-ags-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-mne      .
       let-arc-ags-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-ags-900.
       let-arc-ags-800.
      *              *-------------------------------------------------*
      *              * Se codice agente a zero                         *
      *              *-------------------------------------------------*
       let-arc-ags-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-flg      .
      *                      *-----------------------------------------*
      *                      * Nome                                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-nom      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-loc      .
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-ags-mne      .
       let-arc-ags-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-arc-ags-900.
       let-arc-ags-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice agente su valore precedente          *
      *                  *---------------------------------------------*
           move      w-let-arc-ags-cod    to   w-let-arc-ags-exc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-ags-999.
       let-arc-ags-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio anagrafica clienti di fattu-  *
      *    * razione                                                   *
      *    *-----------------------------------------------------------*
       let-cli-dcc-000.
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    zero
                     go to let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Test se codice e dipendenza pari ai valori pre- *
      *              * cedenti                                         *
      *              *-------------------------------------------------*
           if        w-let-cli-dcc-cli    =    w-let-cli-dcc-exc and
                     w-let-cli-dcc-dpz    =    w-let-cli-dcc-exd
                     go to let-cli-dcc-900.
       let-cli-dcc-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dcc] relativamente al cliente *
      *              * e alla dipendenza richiesta                     *
      *              *-------------------------------------------------*
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
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-cli-dcc-200
           else      go to let-cli-dcc-300.
       let-cli-dcc-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica cliente-dipendenza esistente      *
      *              *-------------------------------------------------*
       let-cli-dcc-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-cli-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-cli-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-dcc-via-dcc       to   w-let-cli-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-dcc-loc-dcc       to   w-let-cli-dcc-loc      .
       let-cli-dcc-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-cli-dcc-900.
       let-cli-dcc-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica cliente-dipendenza non esistente  *
      *              *-------------------------------------------------*
       let-cli-dcc-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-cli-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-cli-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-cli-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-cli-dcc-loc      .
       let-cli-dcc-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-cli-dcc-900.
       let-cli-dcc-800.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
       let-cli-dcc-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-cli-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-cli-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-cli-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-cli-dcc-loc      .
       let-cli-dcc-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-cli-dcc-900.
       let-cli-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice e dipendenza letti su valori prece-  *
      *                  * denti                                       *
      *                  *---------------------------------------------*
           move      w-let-cli-dcc-cli    to   w-let-cli-dcc-exc      .
           move      w-let-cli-dcc-dpz    to   w-let-cli-dcc-exd      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-cli-dcc-999.
       let-cli-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio anagrafica clienti per la     *
      *    * fatturazione                                              *
      *    *-----------------------------------------------------------*
       let-plf-dcc-000.
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-plf-dcc-cli    =    zero
                     go to let-plf-dcc-800.
      *              *-------------------------------------------------*
      *              * Test se codice e dipendenza pari ai valori pre- *
      *              * cedenti                                         *
      *              *-------------------------------------------------*
           if        w-let-plf-dcc-cli    =    w-let-plf-dcc-exc and
                     w-let-plf-dcc-dpz    =    w-let-plf-dcc-exd
                     go to let-plf-dcc-900.
       let-plf-dcc-100.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dcc] relativamente al cliente *
      *              * e alla dipendenza richiesta                     *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-plf-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-plf-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-plf-dcc-200
           else      go to let-plf-dcc-300.
       let-plf-dcc-200.
      *              *-------------------------------------------------*
      *              * Se anagrafica cliente-dipendenza esistente      *
      *              *-------------------------------------------------*
       let-plf-dcc-225.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-plf-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-plf-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      rf-dcc-via-dcc       to   w-let-plf-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-dcc-loc-dcc       to   w-let-plf-dcc-loc      .
       let-plf-dcc-250.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-plf-dcc-900.
       let-plf-dcc-300.
      *              *-------------------------------------------------*
      *              * Se anagrafica cliente-dipendenza non esistente  *
      *              *-------------------------------------------------*
       let-plf-dcc-325.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-plf-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-plf-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-plf-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-plf-dcc-loc      .
       let-plf-dcc-350.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-plf-dcc-900.
       let-plf-dcc-800.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
       let-plf-dcc-825.
      *                  *---------------------------------------------*
      *                  * Memorizzazione flag e valori                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita                          *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-plf-dcc-flg      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-plf-dcc-rag      .
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-plf-dcc-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-plf-dcc-loc      .
       let-plf-dcc-850.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     let-plf-dcc-900.
       let-plf-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice e dipendenza letti su valori prece-  *
      *                  * denti                                       *
      *                  *---------------------------------------------*
           move      w-let-plf-dcc-cli    to   w-let-plf-dcc-exc      .
           move      w-let-plf-dcc-dpz    to   w-let-plf-dcc-exd      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-plf-dcc-999.
       let-plf-dcc-999.
           exit.

      *    *===========================================================*
      *    * Routine di attribuzione numero maturazione per la gestio- *
      *    * ne provvigioni agenti                                     *
      *    *-----------------------------------------------------------*
       att-num-gpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [numgpm]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-num-gpm-400.
       att-num-gpm-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scrittura record normalizzato               *
      *                  *---------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                  *---------------------------------------------*
      *                  * Unlock record                               *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                  *---------------------------------------------*
      *                  * Ripetizione dell'intera operazione          *
      *                  *---------------------------------------------*
           go to     att-num-gpm-000.
       att-num-gpm-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Salvataggio valore pre-incremento           *
      *                  *---------------------------------------------*
           move      rn-num-gpm-num-gpm   to   w-num-gpm-val-pre      .
      *                  *---------------------------------------------*
      *                  * Preparazione valore post-incremento         *
      *                  *---------------------------------------------*
           move      w-num-gpm-val-pre    to   w-num-gpm-val-pos      .
           add       1                    to   w-num-gpm-val-pos      .
           if        w-num-gpm-val-pos    =    zero
                     move  1              to   w-num-gpm-val-pos      .
      *                  *---------------------------------------------*
      *                  * Preparazione numero in uscita               *
      *                  *---------------------------------------------*
           move      w-num-gpm-val-pos    to   w-num-gpm-num-gpm      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento numero movimento              *
      *                  *---------------------------------------------*
           move      w-num-gpm-num-gpm    to   rn-num-gpm-num-gpm     .
      *                  *---------------------------------------------*
      *                  * Update record                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito dell'opera- *
      *                  * zione di update                             *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to att-num-gpm-600.
       att-num-gpm-500.
      *                  *---------------------------------------------*
      *                  * Se errore in operazione di update           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                      *-----------------------------------------*
      *                      * Ripetizione dell'intera operazione      *
      *                      *-----------------------------------------*
           go to     att-num-gpm-000.
       att-num-gpm-600.
      *                  *---------------------------------------------*
      *                  * Se nessun errore in operazione di update    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     att-num-gpm-999.
       att-num-gpm-999.
           exit.

      *    *===========================================================*
      *    * Routine di ripristino numero conteggio per la gestione    *
      *    * provvigioni agenti                                        *
      *    *-----------------------------------------------------------*
       rip-num-gpm-000.
      *              *-------------------------------------------------*
      *              * Lettura tabella numerazioni [numgpm]            *
      *              *-------------------------------------------------*
           move      "GT"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rip-num-gpm-400.
       rip-num-gpm-200.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Unlock record                               *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rip-num-gpm-999.
       rip-num-gpm-400.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto tra il numero movimento attuale   *
      *                  * contenuto nel record numerazioni ed il va-  *
      *                  * lore post-incremento, e deviazione in fun-  *
      *                  * zione dell'esito del test                   *
      *                  *---------------------------------------------*
           move      rn-num-gpm-num-gpm   to   w-num-gpm-num-gpm      .
           if        w-num-gpm-num-gpm    =    w-num-gpm-val-pos
                     go to rip-num-gpm-600.
       rip-num-gpm-500.
      *                  *---------------------------------------------*
      *                  * Se numero movimento attuale contenuto nel   *
      *                  * record numerazioni diverso dal valore post- *
      *                  * incremento                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock record                           *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-num-gpm-999.
       rip-num-gpm-600.
      *                  *---------------------------------------------*
      *                  * Se numero movimento attuale contenuto nel   *
      *                  * record numerazioni uguale al valore post-   *
      *                  * incremento                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controaggiornamento del valore attuale  *
      *                      * contenuto nel record numerazioni, ri-   *
      *                      * portandolo al valore pre-incremento     *
      *                      *-----------------------------------------*
           move      w-num-gpm-val-pre    to   w-num-gpm-num-gpm      .
           move      w-num-gpm-num-gpm    to   rn-num-gpm-num-gpm     .
      *                      *-----------------------------------------*
      *                      * Update record numerazioni               *
      *                      *-----------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                      *-----------------------------------------*
      *                      * Unlock record numerazioni               *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/age/num/ioc/obj/innumgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-gpm             .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     rip-num-gpm-999.
       rip-num-gpm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

