       Identification Division.
       Program-Id.                                 page4401           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    age                 *
      *                                Settore:    mat                 *
      *                                   Fase:    age440              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 12/05/94    *
      *                       Ultima revisione:    NdK del 10/07/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma page4400 *
      *                                                                *
      *                    Stampa elenco delle provvigioni maturate di *
      *                    un periodo, per agente                      *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

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
      *            *               provvigionale e per maturazione     *
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
      *                *-----------------------------------------------*
      *                * Data maturazione                              *
      *                *-----------------------------------------------*
                   15  srt-k02-dat-mat    pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero maturazione                            *
      *                *-----------------------------------------------*
                   15  srt-k02-num-mat    pic  9(09)                  .
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
      *                * Se il codice del cliente per la fatturazione  *
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
      *                * Numero maturazione                            *
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
                     "age440"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "page4401"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "    STAMPA MATURAZIONI DI UN PERIODO    "       .

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

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data iniziale del periodo                             *
      *        *-------------------------------------------------------*
           05  rr-dat-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data finale del periodo                               *
      *        *-------------------------------------------------------*
           05  rr-dat-max                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di maturazione min                               *
      *        *-------------------------------------------------------*
           05  rr-mtz-min                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di maturazione max                               *
      *        *-------------------------------------------------------*
           05  rr-mtz-max                 pic  9(07)                  .
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
      *        *  - 01 : Ordinamento per nominativo agente             *
      *        *  - 02 : Ordinamento per codice agente                 *
      *        *  - 03 : Ordinamento per mnemonico agente              *
      *        *                                                       *
      *        * Non significativo se selezionato un solo agente       *
      *        *-------------------------------------------------------*
           05  rr-tor-age                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di uso della stampa                              *
      *        *  - 01 : Ad uso interno, con stampa quindi dei riferi- *
      *        *         menti interni                                 *
      *        *  - 02 : Da consegnare agli agenti, quindi senza l'e-  *
      *        *         videnziazione dei riferimenti interni         *
      *        *-------------------------------------------------------*
           05  rr-tip-uso                 pic  9(02)                  .

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
      *    * Work per determinazione maturazioni di provvigione gia'   *
      *    * esistenti                                                 *
      *    *                                                           *
      *    * Parte 1 : Valori in input ed in output                    *
      *    *-----------------------------------------------------------*
       01  w-det-mep-001.
      *        *-------------------------------------------------------*
      *        * Input                                                 *
      *        *-------------------------------------------------------*
           05  w-det-mep-inp.
      *            *---------------------------------------------------*
      *            * Si/No esecuzione lettura del record relativo al   *
      *            * conteggio di riferimento                          *
      *            *                                                   *
      *            * - S : Si, lettura da eseguire                     *
      *            * - N : No, lettura da non eseguire in quanto il    *
      *            *           record e' gia' presente in 'rf-gpc'     *
      *            *---------------------------------------------------*
               10  w-det-mep-let-ctg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero conteggio di riferimento                   *
      *            *---------------------------------------------------*
               10  w-det-mep-num-ctg      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Data di maturazione minima                        *
      *            *---------------------------------------------------*
               10  w-det-mep-dat-min      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data di maturazione massima                       *
      *            *---------------------------------------------------*
               10  w-det-mep-dat-max      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Output                                                *
      *        *-------------------------------------------------------*
           05  w-det-mep-out.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalla routine                      *
      *            *                                                   *
      *            * - 00 : Determinazione eseguita                    *
      *            * - 51 : Determinazione non eseguita in quanto non  *
      *            *        esiste piu' in archivio il record relati-  *
      *            *        vo al conteggio provvigionale              *
      *            *---------------------------------------------------*
               10  w-det-mep-flg-det      pic  9(02)                  .
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
               10  w-det-mep-tip-ctg      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di maturazioni esistenti a fronte del con- *
      *            * teggio bufferizzate, totale                       *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-num      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Numero di maturazioni esistenti a fronte del con- *
      *            * teggio bufferizzate, solo quelle con data prece-  *
      *            * dente alla data minima                            *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-pre      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Numero di maturazioni esistenti a fronte del con- *
      *            * teggio bufferizzate, solo quelle con data succes- *
      *            * siva alla data massima                            *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-suc      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Numero di maturazioni esistenti a fronte del con- *
      *            * teggio bufferizzate, solo quelle con data compre- *
      *            * sa tra quella minima e quella massima             *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-tra      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Numero di maturazioni esistenti a fronte del con- *
      *            * teggio bufferizzate, solo quelle con data compre- *
      *            * sa tra quella minima e quella massima, ma senza   *
      *            * includere gli storni di provvigione               *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-trp      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Importo totale delle maturazioni bufferizzate     *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-tot      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Buffer delle maturazioni esistenti                *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-buf.
      *                *-----------------------------------------------*
      *                * Singole maturazioni esistenti                 *
      *                *-----------------------------------------------*
                   15  w-det-mep-bum-ele  occurs  1000.
      *                    *-------------------------------------------*
      *                    * Tipo maturazione                          *
      *                    *-------------------------------------------*
                       20  w-det-mep-tip-mat
                                          pic  9(02)                  .
      *                    *-------------------------------------------*
      *                    * Data maturazione                          *
      *                    *-------------------------------------------*
                       20  w-det-mep-dat-mat
                                          pic  9(07)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Numero maturazione                        *
      *                    *-------------------------------------------*
                       20  w-det-mep-num-mat
                                          pic  9(09)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Importo maturazione                       *
      *                    *-------------------------------------------*
                       20  w-det-mep-imp-mat
                                          pic s9(11)       comp-3     .
      *            *---------------------------------------------------*
      *            * Indice di comodo, per la scansione delle matura-  *
      *            * zioni bufferizzate                                *
      *            *---------------------------------------------------*
               10  w-det-mep-bum-inx      pic  9(04)                  .

      *    *===========================================================*
      *    * Work per determinazione maturazioni di provvigione gia'   *
      *    * esistenti                                                 *
      *    *                                                           *
      *    * Parte 2 : Area di comodo comune a tutte le subroutines    *
      *    *-----------------------------------------------------------*
       01  w-det-mep-002.
      *        *-------------------------------------------------------*
      *        * Area locale per le singole subroutines                *
      *        *-------------------------------------------------------*
           05  w-det-mep-sub.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalle subroutines                  *
      *            * - 00 : Nessuna anomalia                           *
      *            * - nn : Flag indicatore dell'anomalia              *
      *            *---------------------------------------------------*
               10  w-det-mep-sub-flg      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per determinazione maturazioni di provvigione gia'   *
      *    * esistenti                                                 *
      *    *                                                           *
      *    * Parte 3 : Area relativa al conteggio provvigionale        *
      *    *-----------------------------------------------------------*
       01  w-det-mep-003.
      *        *-------------------------------------------------------*
      *        * Area locale per la bufferizzazione di valori prove-   *
      *        * nienti dal record del conteggio provvigionale con-    *
      *        * tenuto in 'rf-gpc'                                    *
      *        *-------------------------------------------------------*
           05  w-det-mep-gpc.
      *            *---------------------------------------------------*
      *            * Numero protocollo movimento di fatture clienti    *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-pfc      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Tipo di conteggio                                 *
      *            *                                                   *
      *            *  - 01 : Conteggio a fronte fattura                *
      *            *  - 02 : Conteggio a fronte nota di addebito       *
      *            *  - 03 : Conteggio a fronte nota di accredito      *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-tdc      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data documento cui si riferisce il conteggio      *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-ddo      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento cui si riferisce il conteggio    *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-ndo      pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Codice agente                                     *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-age      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del cliente                     *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-dcl      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Imponibile provvigionale                          *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-ibl      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * % di provvigione                                  *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-per      pic  9(02)v9(01)            .
      *            *---------------------------------------------------*
      *            * Ammontare provvigione conteggiata sul documento   *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-pro      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale del documento cui si riferisce il  *
      *            * conteggio                                         *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-tdo      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Importo totale di eventuali acconti gia' fattura- *
      *            * ti assorbiti nel documento cui si riferisce il    *
      *            * conteggio                                         *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-taf      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Annotazioni sul conteggio provvigionale           *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-not      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Tipo di maturazione prevista per la provvigione   *
      *            *                                                   *
      *            * - 01 : Maturazione immediata                      *
      *            * - 02 : Maturazione su incassi a fronte documento  *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-tdm      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Segnale di provvigione con maturazione bloccata   *
      *            *                                                   *
      *            * - 01 : Maturazione della provvigione non bloccata *
      *            * - 02 : Maturazione della provvigione bloccata     *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-blo      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data di maturazione minima per la provvigione     *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-dmm      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice agente subordinato                         *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-ags      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente per la fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-ccf      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza del cliente per la fatturazione *
      *            *---------------------------------------------------*
               10  w-det-mep-gpc-dcf      pic  x(04)                  .

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
      *            *---------------------------------------------------*
      *            * Totale imponibile rapportato all'incassato        *
      *            *---------------------------------------------------*
               10  w-liv-age-ibl-pvg      pic s9(11)                  .
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
               10  w-stp-int-wed-dt9      pic  x(08)                  .
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
      *        *-------------------------------------------------------*
      *        * Valori di comodo                                      *
      *        *-------------------------------------------------------*
           05  w-ass-wrk.
      *            *---------------------------------------------------*
      *            * % di rapporto tra incassato e totale documento    *
      *            *---------------------------------------------------*
               10  w-ass-wrk-per-itd      pic s9(03)v9(02)            .
      *            *---------------------------------------------------*
      *            * Imponibile provvigionale in rapporto all'incassa- *
      *            * to                                                *
      *            *---------------------------------------------------*
               10  w-ass-wrk-ibl-pvg      pic s9(11)                  .

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
           if        w-prs-dat-mtz        not  = "S"
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
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
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
           move      zero                 to   rf-gpc-dat-doc         .
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
           else      go to stp-srt-inp-900.
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
           move      zero                 to   rf-gpc-dat-doc         .
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
           else      go to stp-srt-inp-900.
       stp-srt-inp-100.
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
                     go to stp-srt-inp-900.
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
                     go to stp-srt-inp-900.
       stp-srt-inp-224.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-gpc-dat-doc       >    rr-dat-max
                     go to stp-srt-inp-900.
       stp-srt-inp-226.
      *                      *-----------------------------------------*
      *                      * Test max superato                       *
      *                      *-----------------------------------------*
           go to     stp-srt-inp-250.
       stp-srt-inp-230.
      *                  *---------------------------------------------*
      *                  * Se richiesti tutti gli agenti               *
      *                  *---------------------------------------------*
       stp-srt-inp-232.
      *                      *-----------------------------------------*
      *                      * Test su data documento                  *
      *                      *-----------------------------------------*
           if        rf-gpc-dat-doc       >    rr-dat-max
                     go to stp-srt-inp-900.
       stp-srt-inp-234.
      *                      *-----------------------------------------*
      *                      * Test max superato                       *
      *                      *-----------------------------------------*
           go to     stp-srt-inp-250.
       stp-srt-inp-250.
      *              *-------------------------------------------------*
      *              * Selezione su archivio [gpc], e se non supera-   *
      *              * ta : a lettura record successivo                *
      *              *-------------------------------------------------*
       stp-srt-inp-260.
      *                  *---------------------------------------------*
      *                  * Controllo che il conteggio non sia uno di   *
      *                  * quelli senza provvigione                    *
      *                  *---------------------------------------------*
           if        rf-gpc-per-pvg       =    zero and
                     rf-gpc-amm-pvg       =    zero
                     go to stp-srt-inp-100.
       stp-srt-inp-270.
      *                  *---------------------------------------------*
      *                  * Fine selezione su archivio [gpc]            *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-300.
       stp-srt-inp-300.
      *              *-------------------------------------------------*
      *              * Richiamo della routine per la determinazione    *
      *              * delle maturazioni esistenti a fronte di un con- *
      *              * teggio, e deviazione a seconda dell'esito della *
      *              * determinazione                                  *
      *              *-------------------------------------------------*
       stp-srt-inp-305.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per il richiamo  *
      *                  * della subroutine                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura del record relativo al conteg-  *
      *                      * gio provvigionale : No, in quanto gia'  *
      *                      * presente in 'rf-gpc'                    *
      *                      *-----------------------------------------*
           move      "N"                  to   w-det-mep-let-ctg      .
      *                      *-----------------------------------------*
      *                      * Numero conteggio di riferimento per le  *
      *                      * maturazioni : pari a quello contenuto   *
      *                      * in 'rf-gpc'                             *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   w-det-mep-num-ctg      .
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima : pari a     *
      *                      * quella impostata                        *
      *                      *-----------------------------------------*
           move      rr-dat-min           to   w-det-mep-dat-min      .
      *                      *-----------------------------------------*
      *                      * Data di maturazione massima : pari a    *
      *                      * quella impostata                        *
      *                      *-----------------------------------------*
           move      rr-dat-max           to   w-det-mep-dat-max      .
       stp-srt-inp-310.
      *                  *---------------------------------------------*
      *                  * Richiamo effettivo della subroutine         *
      *                  *---------------------------------------------*
           perform   det-mep-000          thru det-mep-999            .
       stp-srt-inp-315.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda dell'esito della rou-  *
      *                  * tine                                        *
      *                  *---------------------------------------------*
           if        w-det-mep-flg-det    =    zero
                     go to stp-srt-inp-400.
       stp-srt-inp-350.
      *              *-------------------------------------------------*
      *              * Se esito della maturazione diverso da zero, es- *
      *              * so puo' essere pari solo a                      *
      *              * - 51 : Record del conteggio provvigionale non   *
      *              *        piu' esistente in archivio.              *
      *              *        Nota : questo status di uscita non puo'  *
      *              *               mai essere ritornato, in quanto   *
      *              *               non e' stata richiesta la lettu-  *
      *              *               ra del record relativo al conteg- *
      *              *               gio provvigionale                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, si ignora il record relati- *
      *                  * vo al conteggio provvigionale, e si ricicla *
      *                  * a leggere quello successivo                 *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-850.
       stp-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Se determinazione eseguita                      *
      *              *                                                 *
      *              * Si effettua una scansione sulle maturazioni     *
      *              * bufferizzate, e per ciascuna di esse avente     *
      *              * data maturazione compresa tra la data mini-     *
      *              * ma e la data massima si emette un record di     *
      *              * sort                                            *
      *              *-------------------------------------------------*
       stp-srt-inp-410.
      *                  *---------------------------------------------*
      *                  * Se non e' stata bufferizzata alcuna matura- *
      *                  * zione con data compresa tra la minima e la  *
      *                  * massima, escludendo gli storni di provvi-   *
      *                  * gione : a lettura conteggio provvigionale   *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        w-det-mep-bum-trp    =    zero
                     go to stp-srt-inp-850.
       stp-srt-inp-430.
      *                  *---------------------------------------------*
      *                  * Letture records preliminari                 *
      *                  *---------------------------------------------*
       stp-srt-inp-432.
      *                      *-----------------------------------------*
      *                      * Anagrafica agente                       *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
       stp-srt-inp-434.
      *                      *-----------------------------------------*
      *                      * Anagrafica agente subordinato           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   w-let-arc-ags-cod      .
           perform   let-arc-ags-000      thru let-arc-ags-999        .
       stp-srt-inp-436.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente di fatturazione      *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   w-let-cli-dcc-cli      .
           move      rf-gpc-dpz-cli       to   w-let-cli-dcc-dpz      .
           perform   let-cli-dcc-000      thru let-cli-dcc-999        .
       stp-srt-inp-438.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente per la fatturazione  *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   w-let-plf-dcc-cli      .
           move      rf-gpc-dpz-plf       to   w-let-plf-dcc-dpz      .
           perform   let-plf-dcc-000      thru let-plf-dcc-999        .
       stp-srt-inp-450.
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice su maturazioni per  *
      *                  * l'esecuzione del ciclo di scansione sulle   *
      *                  * maturazioni bufferizzate                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-mep-bum-inx      .
       stp-srt-inp-500.
      *                  *---------------------------------------------*
      *                  * Incremento indice su maturazioni            *
      *                  *---------------------------------------------*
           add       1                    to   w-det-mep-bum-inx      .
      *                  *---------------------------------------------*
      *                  * Se oltre il numero di maturazioni bufferiz- *
      *                  * zate : a lettura conteggio provvigionale    *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        w-det-mep-bum-inx    >    w-det-mep-bum-num
                     go to stp-srt-inp-850.
       stp-srt-inp-510.
      *                  *---------------------------------------------*
      *                  * Se si tratta di uno storno di provvigione : *
      *                  * si ignora la maturazione e si ricicla alla  *
      *                  * maturazione successiva nel buffer           *
      *                  *---------------------------------------------*
______*    if        w-det-mep-tip-mat
______*             (w-det-mep-bum-inx)   =    51
______*              go to stp-srt-inp-800.
      *                  *---------------------------------------------*
      *                  * Se la maturazione non ha una data compresa  *
      *                  * tra la minima e la massima : si ignora la   *
      *                  * maturazione e si ricicla alla maturazione   *
      *                  * successiva nel buffer                       *
      *                  *                                             *
      *                  * Test assoggettato a personalizzazione       *
      *                  *---------------------------------------------*
           if        w-prs-dat-mtz        =    "S"
                     go to stp-srt-inp-520.
           if        w-det-mep-dat-mat
                    (w-det-mep-bum-inx)   <    w-det-mep-dat-min or
                     w-det-mep-dat-mat
                    (w-det-mep-bum-inx)   >    w-det-mep-dat-max
                     go to stp-srt-inp-800.
       stp-srt-inp-520.
      *                  *---------------------------------------------*
      *                  * Lettura del record maturazione in esame     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CTGMTZ    "         to   f-key                  .
           move      w-det-mep-num-ctg    to   rf-gpm-num-ctg         .
           move      w-det-mep-num-mat
                    (w-det-mep-bum-inx)   to   rf-gpm-num-mtz         .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *                  *---------------------------------------------*
      *                  * Se maturazione non trovata : si ignora la   *
      *                  * maturazione e si ricicla alla maturazione   *
      *                  * successiva nel buffer                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-800.
       stp-srt-inp-530.
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare record si sort  *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
       stp-srt-inp-550.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa alla subchiave 1 : per agente      *
      *                  *---------------------------------------------*
       stp-srt-inp-552.
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
       stp-srt-inp-554.
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
       stp-srt-inp-556.
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
       stp-srt-inp-558.
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
       stp-srt-inp-560.
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   srt-k01-cod-age        .
       stp-srt-inp-570.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa alla subchiave 2 : per documento   *
      *                  * relativo al conteggio provvigionale         *
      *                  *---------------------------------------------*
       stp-srt-inp-572.
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   srt-k02-dat-doc        .
       stp-srt-inp-574.
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   srt-k02-num-doc        .
       stp-srt-inp-576.
      *                      *-----------------------------------------*
      *                      * Numero conteggio                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   srt-k02-num-ctg        .
       stp-srt-inp-578.
      *                      *-----------------------------------------*
      *                      * Data maturazione                        *
      *                      *-----------------------------------------*
           move      rf-gpm-dat-mtz       to   srt-k02-dat-mat        .
       stp-srt-inp-580.
      *                      *-----------------------------------------*
      *                      * Numero maturazione                      *
      *                      *-----------------------------------------*
           move      rf-gpm-num-mtz       to   srt-k02-num-mat        .
       stp-srt-inp-590.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al conteg-  *
      *                  * gio provvigionale                           *
      *                  *---------------------------------------------*
       stp-srt-inp-592.
      *                      *-----------------------------------------*
      *                      * Numero conteggio                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-ctg       to   srt-gpc-num-ctg        .
       stp-srt-inp-594.
      *                      *-----------------------------------------*
      *                      * Numero protocollo movimento di fatture  *
      *                      * clienti                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-prt-fcl       to   srt-gpc-prt-fcl        .
       stp-srt-inp-596.
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   srt-gpc-tip-ctg        .
       stp-srt-inp-597.
      *                      *-----------------------------------------*
      *                      * Tipo di vendita per l'agente            *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-vpa       to   srt-gpc-tip-vpa        .
       stp-srt-inp-598.
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   srt-gpc-dat-doc        .
       stp-srt-inp-600.
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   srt-gpc-num-doc        .
       stp-srt-inp-602.
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   srt-gpc-cod-age        .
       stp-srt-inp-604.
      *                      *-----------------------------------------*
      *                      * Codice cliente di fatturazione          *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   srt-gpc-cod-cli        .
       stp-srt-inp-606.
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente di fattu- *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-cli       to   srt-gpc-dpz-cli        .
       stp-srt-inp-608.
      *                      *-----------------------------------------*
      *                      * Imponibile provvigionale                *
      *                      *-----------------------------------------*
           move      rf-gpc-ibl-pvg       to   srt-gpc-ibl-pvg        .
       stp-srt-inp-610.
      *                      *-----------------------------------------*
      *                      * % di provvigione                        *
      *                      *-----------------------------------------*
           move      rf-gpc-per-pvg       to   srt-gpc-per-pvg        .
       stp-srt-inp-612.
      *                      *-----------------------------------------*
      *                      * Ammontare provvigione conteggiata sul   *
      *                      * documento                               *
      *                      *-----------------------------------------*
           move      rf-gpc-amm-pvg       to   srt-gpc-amm-pvg        .
       stp-srt-inp-614.
      *                      *-----------------------------------------*
      *                      * Importo totale del documento cui si ri- *
      *                      * ferisce il conteggio                    *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-doc       to   srt-gpc-imp-doc        .
       stp-srt-inp-616.
      *                      *-----------------------------------------*
      *                      * Importo totale di eventuali acconti gia'*
      *                      * fatturati assorbiti nel documento cui   *
      *                      * si riferisce il conteggio               *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-agf       to   srt-gpc-imp-agf        .

       stp-srt-inp-618.
      *                      *-----------------------------------------*
      *                      * Annotazioni sul conteggio provvigionale *
      *                      *-----------------------------------------*
           move      rf-gpc-not-ctg       to   srt-gpc-not-ctg        .
       stp-srt-inp-620.
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione prevista per la     *
      *                      * provvigione conteggiata                 *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-mat       to   srt-gpc-tip-mat        .
       stp-srt-inp-622.
      *                      *-----------------------------------------*
      *                      * Segnale di provvigione con maturazione  *
      *                      * bloccata                                *
      *                      *-----------------------------------------*
           move      rf-gpc-mat-blo       to   srt-gpc-mat-blo        .
       stp-srt-inp-624.
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima per la prov- *
      *                      * vigione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-ddm-min       to   srt-gpc-ddm-min        .
       stp-srt-inp-626.
      *                      *-----------------------------------------*
      *                      * Codice agente subordinato               *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   srt-gpc-cod-ags        .
       stp-srt-inp-628.
      *                      *-----------------------------------------*
      *                      * Codice del cliente per la fatturazione  *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   srt-gpc-cod-plf        .
       stp-srt-inp-630.
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente per la    *
      *                      * fatturazione                            *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-plf       to   srt-gpc-dpz-plf        .
       stp-srt-inp-650.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa all'agente  *
      *                  *---------------------------------------------*
       stp-srt-inp-652.
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-age-mne    to   srt-age-mne-age        .
       stp-srt-inp-654.
      *                      *-----------------------------------------*
      *                      * Nominativo                              *
      *                      *-----------------------------------------*
           move      w-let-arc-age-nom    to   srt-age-nom-age        .
       stp-srt-inp-656.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-arc-age-rag    to   srt-age-rag-age        .
       stp-srt-inp-670.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al clien-   *
      *                  * te di fatturazione                          *
      *                  *---------------------------------------------*
       stp-srt-inp-672.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-rag    to   srt-cli-rag-cli        .
       stp-srt-inp-674.
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-via    to   srt-cli-via-cli        .
       stp-srt-inp-676.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      w-let-cli-dcc-loc    to   srt-cli-loc-cli        .
       stp-srt-inp-690.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa all'agente  *
      *                  * subordinato                                 *
      *                  *---------------------------------------------*
       stp-srt-inp-692.
      *                      *-----------------------------------------*
      *                      * Mnemonico                               *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-mne    to   srt-ags-mne-ags        .
       stp-srt-inp-694.
      *                      *-----------------------------------------*
      *                      * Nominativo                              *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-nom    to   srt-ags-nom-ags        .
       stp-srt-inp-696.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-arc-ags-rag    to   srt-ags-rag-ags        .
       stp-srt-inp-710.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa al clien-   *
      *                  * te per la fatturazione                      *
      *                  *---------------------------------------------*
       stp-srt-inp-712.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-rag    to   srt-plf-rag-plf        .
       stp-srt-inp-714.
      *                      *-----------------------------------------*
      *                      * Via                                     *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-via    to   srt-plf-via-plf        .
       stp-srt-inp-716.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      w-let-plf-dcc-loc    to   srt-plf-loc-plf        .
       stp-srt-inp-730.
      *                  *---------------------------------------------*
      *                  * Composizione dell'area del record di sort   *
      *                  * relativa all'area dati relativa alla matu-  *
      *                  * razione vera e propria                      *
      *                  *---------------------------------------------*
       stp-srt-inp-732.
      *                      *-----------------------------------------*
      *                      * Numero maturazione                      *
      *                      *-----------------------------------------*
           move      rf-gpm-num-mtz       to   srt-gpm-num-mtz        .
       stp-srt-inp-734.
      *                      *-----------------------------------------*
      *                      * Data di riferimento per la maturazione  *
      *                      *-----------------------------------------*
           move      rf-gpm-dat-mtz       to   srt-gpm-dat-mtz        .
       stp-srt-inp-736.
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione effettuata          *
      *                      *-----------------------------------------*
           move      rf-gpm-tip-mtz       to   srt-gpm-tip-mtz        .
       stp-srt-inp-738.
      *                      *-----------------------------------------*
      *                      * Importo incassato, che ha fatto scatta- *
      *                      * re la maturazione                       *
      *                      *-----------------------------------------*
           move      rf-gpm-imp-inc       to   srt-gpm-imp-inc        .
       stp-srt-inp-740.
      *                      *-----------------------------------------*
      *                      * Importo di provvigione maturato         *
      *                      *-----------------------------------------*
           move      rf-gpm-imp-mtz       to   srt-gpm-imp-mtz        .
       stp-srt-inp-760.
      *                      *-----------------------------------------*
      *                      * Note sulla maturazione                  *
      *                      *-----------------------------------------*
           move      rf-gpm-not-mtz       to   srt-gpm-not-mtz        .
       stp-srt-inp-780.
      *                  *---------------------------------------------*
      *                  * Rilascio del record al Sort                 *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-800.
      *                  *---------------------------------------------*
      *                  * Riciclo per esaminare la maturazione suc-   *
      *                  * cessiva presente nel buffer                 *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-500.
       stp-srt-inp-850.
      *              *-------------------------------------------------*
      *              * Riciclo alla lettura del conteggio provvigonale *
      *              * successivo                                      *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-100.
       stp-srt-inp-900.
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
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessuna maturazione da stampare entro le date impo
      -              "state !                       "
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
      *              * Se non sono stati trattati almeno due agenti :  *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           if        w-liv-gen-num-age    not  > 1
                     go to prn-fin-cic-999.
       prn-fin-cic-300.
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
       prn-fin-cic-400.
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
       prn-fin-cic-500.
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
      *                  * Totale imponibile provvigionale rapportato  *
      *                  *---------------------------------------------*
           move      zero                 to   w-liv-age-ibl-pvg      .
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
       prn-fin-lr1-560.
      *                  *---------------------------------------------*
      *                  * Totale imponibile provvigionale rapportato  *
      *                  * all'incassato                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore da stampare              *
      *                      *-----------------------------------------*
           if        w-liv-age-ibl-pvg    =    zero
                     go to prn-fin-lr1-999.
      *                      *-----------------------------------------*
      *                      * Editing e concatenazione                *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
      *
           move      "Imponib. provvig. :"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-liv-age-ibl-pvg    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-all-str-cat (02)     .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      093                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
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
       prn-liv-det-060.
      *              *-------------------------------------------------*
      *              * Determinazione in 'w-ass-wrk-per-itd' della %   *
      *              * di rapporto tra incassato e totale documento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-ass-wrk-per-itd      .
           move      zero                 to   w-ass-wrk-ibl-pvg      .
           if        w-ass-ctg-tip-ctg    =    03 or
                     w-ass-ctg-tip-ctg    =    11
                     move   w-ass-ctg-ibl-pvg
                                          to   w-ass-wrk-ibl-pvg
                     go to prn-liv-det-075.
           if        w-ass-mtz-tip-mtz    not  = 02
                     go to prn-liv-det-080.
           divide    w-ass-ctg-imp-doc    into w-ass-mtz-imp-inc
                                        giving w-ass-wrk-per-itd      .
           multiply  100                  by   w-ass-wrk-per-itd      .
      *              *-------------------------------------------------*
      *              * Applicazione della % determinata all'imponibile *
      *              * provvigionale relativo al conteggio             *
      *              *-------------------------------------------------*
           if        w-ass-ctg-ibl-pvg    =    zero or
                     w-ass-wrk-per-itd    =    zero
                     go to prn-liv-det-080.
           multiply  w-ass-wrk-per-itd    by   w-ass-ctg-ibl-pvg
                                        giving w-ass-wrk-ibl-pvg      .
           divide    100                  into w-ass-wrk-ibl-pvg      .
       prn-liv-det-075.
      *              *-------------------------------------------------*
      *              * Il totale ottenuto va cumulato                  *
      *              *-------------------------------------------------*
           add       w-ass-wrk-ibl-pvg    to   w-liv-age-ibl-pvg      .
       prn-liv-det-080.
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
      *                      * tonde                                   *
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
           if        srt-gpc-tip-vpa      =    02
                     move  "indiretta"    to   w-all-str-cat (06)
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
      *                          * Se non ad uso interno : no stampa   *
      *                          *-------------------------------------*
           if        rr-tip-uso           not  = 01
                     go to prn-liv-det-320.
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
      *                          * Se non ad uso interno : no stampa   *
      *                          *-------------------------------------*
           if        rr-tip-uso           not  = 01
                     go to prn-liv-det-330.
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
      *                      * Segnale di maturazione bloccata         *
      *                      *-----------------------------------------*
       prn-liv-det-331.
      *                          *-------------------------------------*
      *                          * Se non ad uso interno : no stampa   *
      *                          *-------------------------------------*
           if        rr-tip-uso           not  = 01
                     go to prn-liv-det-340.
      *                          *-------------------------------------*
      *                          * Se data di maturazione a zero o pa- *
      *                          * ri alla data documento : no stampa  *
      *                          *-------------------------------------*
      *                          *-------------------------------------*
      *                          * Se data di maturazione a zero : no  *
      *                          * stampa                              *
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
      *                          * Se non ad uso interno : no stampa   *
      *                          *-------------------------------------*
           if        rr-tip-uso           not  = 01
                     go to prn-liv-det-530.
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
      *                      * Totale documento                        *
      *                      *-----------------------------------------*
       prn-liv-det-541.
      *                          *-------------------------------------*
      *                          * Se la maturazione non e' a fronte   *
      *                          * di un incasso : no stampa           *
      *                          *-------------------------------------*
           if        w-ass-mtz-tip-mtz    not  = 02
                     go to prn-liv-det-550.
      *                          *-------------------------------------*
      *                          * Se a zero : no stampa               *
      *                          *-------------------------------------*
           if        w-ass-ctg-imp-doc    =    zero
                     go to prn-liv-det-550.
       prn-liv-det-542.
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
           move      "Totale  documento :"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-ass-ctg-imp-doc    to   p-num                  .
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
       prn-liv-det-550.
      *                      *-----------------------------------------*
      *                      * Imponibile provvigionale                *
      *                      *-----------------------------------------*
       prn-liv-det-551.
      *                          *-------------------------------------*
      *                          * Se la maturazione non e' a fronte   *
      *                          * di un incasso : no stampa           *
      *                          *-------------------------------------*
           if        w-ass-mtz-tip-mtz    not  = 02
                     go to prn-liv-det-580.
      *                          *-------------------------------------*
      *                          * Se a zero : no stampa               *
      *                          *-------------------------------------*
           if        w-ass-ctg-ibl-pvg    =    zero
                     go to prn-liv-det-580.
       prn-liv-det-552.
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
           move      "Imponib. provvig. :"
                                          to   w-all-str-cat (01)     .
      *
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "G"                  to   p-edm                  .
           move      w-ass-ctg-ibl-pvg    to   p-num                  .
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
       prn-liv-det-580.
      *                      *-----------------------------------------*
      *                      * Annotazioni                             *
      *                      *-----------------------------------------*
       prn-liv-det-581.
      *                          *-------------------------------------*
      *                          * Se non ad uso interno : no stampa   *
      *                          *-------------------------------------*
           if        rr-tip-uso           not  = 01
                     go to prn-liv-det-590.
      *                          *-------------------------------------*
      *                          * Se a spaces : no stampa             *
      *                          *-------------------------------------*
           if        srt-gpm-not-mtz      =    spaces
                     go to prn-liv-det-590.
       prn-liv-det-582.
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
      *                  * dell linea di sottolineatura relativa alla  *
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
      *                      * Editing data minima                     *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-dat-min           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt0      .
      *                      *-----------------------------------------*
      *                      * Editing data massima                    *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-dat-max           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-int-wed-dt9      .
      *                      *-----------------------------------------*
      *                      * Preparazione del titolo completo        *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-int-tit-stp      .
           string    "ELENCO DELLE PROVVIGIONI MATURATE DAL "
                                delimited by   size
                     w-stp-int-wed-dt0
                                delimited by   size
                     " AL "
                                delimited by   size
                     w-stp-int-wed-dt9
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
           move      "Stampa ad uso interno"
                                          to   w-stp-int-sbt-stp      .
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
      *                  * Se non e' una stampa ad uso interno : no    *
      *                  * sub-intestazione                            *
      *                  *---------------------------------------------*
           if        rr-tip-uso           not  = 01
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
      *    * Determinazione delle maturazioni di provvigione gia' e-   *
      *    * sistenti                                                  *
      *    *                                                           *
      *    * Routine principale                                        *
      *    *-----------------------------------------------------------*
       det-mep-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione dei flags di uscita dalla rou- *
      *              * tine                                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-flg-det      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del tipo di conteggio di rife- *
      *              * rimento                                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-tip-ctg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero totale di matura-   *
      *              * zioni esistenti a fronte del conteggio          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-num      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero di maturazioni esi- *
      *              * stenti a fronte del conteggio, e con data infe- *
      *              * riore alla data minima                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-pre      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero di maturazioni esi- *
      *              * stenti a fronte del conteggio, e con data supe- *
      *              * riore alla data massima                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-suc      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero di maturazioni esi- *
      *              * stenti a fronte del conteggio, e con data com-  *
      *              * presa tra la data minima e la data massima      *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-tra      .
      *              *-------------------------------------------------*
      *              * Inizializzazione del numero di maturazioni esi- *
      *              * stenti a fronte del conteggio, e con data com-  *
      *              * presa tra la data minima e la data massima, ma  *
      *              * senza includere gli storni di provvigione       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-trp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione importo totale delle matura-   *
      *              * zioni esistenti a fronte del conteggio          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-bum-tot      .
      *              *-------------------------------------------------*
      *              * Lettura del conteggio provvigionale, con buffe- *
      *              * rizzazione dei valori associati                 *
      *              *-------------------------------------------------*
           perform   det-mep-lco-000      thru det-mep-lco-999        .
      *              *-------------------------------------------------*
      *              * Se esito della lettura negativo : uscita con    *
      *              * status di uscita ad errore indicante che la ma- *
      *              * turazione non e' stata eseguita in quanto non   *
      *              * e' piu' accessibile il record relativo al con-  *
      *              * teggio provvigionale                            *
      *              *-------------------------------------------------*
           if        w-det-mep-sub-flg    not  = zero
                     move  w-det-mep-sub-flg
                                          to   w-det-mep-flg-det
                     go to det-mep-999.
      *              *-------------------------------------------------*
      *              * Lettura delle maturazioni gia' presenti relati- *
      *              * ve al record del conteggio provvigionale, con   *
      *              * bufferizzazione, e aggiornamento valori in out- *
      *              * put                                             *
      *              *-------------------------------------------------*
           perform   det-mep-lmt-000      thru det-mep-lmt-999        .
      *              *-------------------------------------------------*
      *              * Uscita con lo status ritornato dalla routine    *
      *              *-------------------------------------------------*
           move      w-det-mep-sub-flg    to   w-det-mep-flg-det      .
       det-mep-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle maturazioni di provvigione gia' e-   *
      *    * sistenti                                                  *
      *    *                                                           *
      *    * Lettura del record relativo al conteggio provvigionale    *
      *    *-----------------------------------------------------------*
       det-mep-lco-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita da subroutine   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-sub-flg      .
      *              *-------------------------------------------------*
      *              * Se record relativo al conteggio da non leggere  *
      *              * in quanto gia' presente in area record : si o-  *
      *              * mette la lettura                                *
      *              *-------------------------------------------------*
           if        w-det-mep-let-ctg    =    "N"
                     go to det-mep-lco-300.
       det-mep-lco-100.
      *              *-------------------------------------------------*
      *              * Lettura del record relativo al conteggio        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMCTG    "         to   f-key                  .
           move      w-det-mep-num-ctg    to   rf-gpc-num-ctg         .
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
                     go to det-mep-lco-200
           else      go to det-mep-lco-300.
       det-mep-lco-200.
      *              *-------------------------------------------------*
      *              * Trattamento dell'errore in lettura              *
      *              *-------------------------------------------------*
       det-mep-lco-210.
      *                  *---------------------------------------------*
      *                  * Flag di uscita a : record non esistente     *
      *                  *---------------------------------------------*
           move      51                   to   w-det-mep-sub-flg      .
       det-mep-lco-220.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-mep-lco-999.
       det-mep-lco-300.
      *              *-------------------------------------------------*
      *              * Trattamento del record                          *
      *              *-------------------------------------------------*
       det-mep-lco-310.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori dal record           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero protocollo movimento di fatture  *
      *                      * clienti                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-prt-fcl       to   w-det-mep-gpc-pfc      .
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   w-det-mep-gpc-tdc      .
      *                      *-----------------------------------------*
      *                      * Data documento cui si riferisce il con- *
      *                      * teggio                                  *
      *                      *-----------------------------------------*
           move      rf-gpc-dat-doc       to   w-det-mep-gpc-ddo      .
      *                      *-----------------------------------------*
      *                      * Numero documento cui si riferisce il    *
      *                      * conteggio                               *
      *                      *-----------------------------------------*
           move      rf-gpc-num-doc       to   w-det-mep-gpc-ndo      .
      *                      *-----------------------------------------*
      *                      * Codice agente                           *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-age       to   w-det-mep-gpc-age      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-cli       to   w-det-mep-gpc-cli      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente           *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-cli       to   w-det-mep-gpc-dcl      .
      *                      *-----------------------------------------*
      *                      * Imponibile provvigionale                *
      *                      *-----------------------------------------*
           move      rf-gpc-ibl-pvg       to   w-det-mep-gpc-ibl      .
      *                      *-----------------------------------------*
      *                      * % di provvigione                        *
      *                      *-----------------------------------------*
           move      rf-gpc-per-pvg       to   w-det-mep-gpc-per      .
      *                      *-----------------------------------------*
      *                      * Ammontare provvigione conteggiata       *
      *                      *-----------------------------------------*
           move      rf-gpc-amm-pvg       to   w-det-mep-gpc-pro      .
      *                      *-----------------------------------------*
      *                      * Importo totale del documento cui si ri- *
      *                      * ferisce il conteggio                    *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-doc       to   w-det-mep-gpc-tdo      .
      *                      *-----------------------------------------*
      *                      * Importo totale di eventuali acconti gia'*
      *                      * fatturati assorbiti nel documento cui   *
      *                      * si riferisce il conteggio               *
      *                      *-----------------------------------------*
           move      rf-gpc-imp-agf       to   w-det-mep-gpc-taf      .
      *                      *-----------------------------------------*
      *                      * Annotazioni sul conteggio provvigionale *
      *                      *-----------------------------------------*
           move      rf-gpc-not-ctg       to   w-det-mep-gpc-not      .
      *                      *-----------------------------------------*
      *                      * Tipo di maturazione prevista per la     *
      *                      * provvigione                             *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-mat       to   w-det-mep-gpc-tdm      .
      *                      *-----------------------------------------*
      *                      * Segnale di provvigione con maturazione  *
      *                      * bloccata                                *
      *                      *-----------------------------------------*
           move      rf-gpc-mat-blo       to   w-det-mep-gpc-blo      .
      *                      *-----------------------------------------*
      *                      * Data di maturazione minima per la prov- *
      *                      * vigione                                 *
      *                      *-----------------------------------------*
           move      rf-gpc-ddm-min       to   w-det-mep-gpc-dmm      .
      *                      *-----------------------------------------*
      *                      * Codice agente subordinato               *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-ags       to   w-det-mep-gpc-ags      .
      *                      *-----------------------------------------*
      *                      * Codice cliente per la fatturazione      *
      *                      *-----------------------------------------*
           move      rf-gpc-cod-plf       to   w-det-mep-gpc-ccf      .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza del cliente per la    *
      *                      * fatturazione                            *
      *                      *-----------------------------------------*
           move      rf-gpc-dpz-plf       to   w-det-mep-gpc-dcf      .
       det-mep-lco-400.
      *                  *---------------------------------------------*
      *                  * Preparazione valori in uscita               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo di conteggio                       *
      *                      *-----------------------------------------*
           move      rf-gpc-tip-ctg       to   w-det-mep-tip-ctg      .
       det-mep-lco-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-mep-lco-999.
       det-mep-lco-999.
           exit.

      *    *===========================================================*
      *    * Determinazione delle maturazioni di provvigione gia' e-   *
      *    * sistenti                                                  *
      *    *                                                           *
      *    * Lettura delle maturazioni gia' presenti relative al re-   *
      *    * cord del conteggio provvigionale, con bufferizzazione, e  *
      *    * aggiornamento valori in output                            *
      *    *-----------------------------------------------------------*
       det-mep-lmt-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita da subroutine   *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mep-sub-flg      .
       det-mep-lmt-100.
      *              *-------------------------------------------------*
      *              * Start su maturazioni relative al conteggio      *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CTGMTZ    "         to   f-key                  .
           move      w-det-mep-num-ctg    to   rf-gpm-num-ctg         .
           move      zero                 to   rf-gpm-num-mtz         .
           move      "pgm/age/fls/ioc/obj/iofgpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gpm                 .
      *              *-------------------------------------------------*
      *              * Se errore di Start : ad uscita                  *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mep-lmt-900.
       det-mep-lmt-200.
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
      *              * Se fine file : ad uscita                        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-mep-lmt-900.
       det-mep-lmt-300.
      *              *-------------------------------------------------*
      *              * Test max su maturazioni relative al conteggio,  *
      *              * se non superato : ad uscita                     *
      *              *-------------------------------------------------*
           if        rf-gpm-num-ctg       not  = w-det-mep-num-ctg
                     go to det-mep-lmt-900.
       det-mep-lmt-400.
      *              *-------------------------------------------------*
      *              * Selezioni sulle maturazioni                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-dat-mtz        not  = "S"
                     go to det-mep-lmt-500.
      *                  *---------------------------------------------*
      *                  * Test su data min                            *
      *                  *---------------------------------------------*
           if        rr-mtz-min           =    zero
                     go to det-mep-lmt-420.
           if        rf-gpm-dat-mtz       <    rr-mtz-min
                     go to det-mep-lmt-200.
       det-mep-lmt-420.
      *                  *---------------------------------------------*
      *                  * Test su data max                            *
      *                  *---------------------------------------------*
           if        rr-mtz-max           =    zero
                     go to det-mep-lmt-500.
           if        rf-gpm-dat-mtz       >    rr-mtz-max
                     go to det-mep-lmt-200.
       det-mep-lmt-500.
      *              *-------------------------------------------------*
      *              * Incremento numero di maturazioni esistenti a    *
      *              * fronte del conteggio bufferizzate, totale       *
      *              *-------------------------------------------------*
           add       1                    to   w-det-mep-bum-num      .
      *              *-------------------------------------------------*
      *              * Incremento differenziato del numero di matura-  *
      *              * zioni esistenti a fronte del conteggio, a se-   *
      *              * conda se                                        *
      *              *  - antecedenti la data minima e massima         *
      *              *  - comprese tra la data minima e massima        *
      *              *  - successive alla data minima e massima        *
      *              *-------------------------------------------------*
           if        rf-gpm-dat-mtz       <    w-det-mep-dat-min
                     add   1              to   w-det-mep-bum-pre
                     go to det-mep-lmt-525.
           if        rf-gpm-dat-mtz       >    w-det-mep-dat-max
                     add   1              to   w-det-mep-bum-suc
                     go to det-mep-lmt-525.
           add       1                    to   w-det-mep-bum-tra      .
           if        rf-gpm-tip-mtz       not  = 51
                     add   1              to   w-det-mep-bum-trp      .
       det-mep-lmt-525.
      *              *-------------------------------------------------*
      *              * Incremento importo totale delle maturazioni     *
      *              * bufferizzate                                    *
      *              *-------------------------------------------------*
           add       rf-gpm-imp-mtz       to   w-det-mep-bum-tot      .
       det-mep-lmt-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione della maturazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo maturazione                            *
      *                  *---------------------------------------------*
           move      rf-gpm-tip-mtz       to   w-det-mep-tip-mat
                                              (w-det-mep-bum-num)     .
      *                  *---------------------------------------------*
      *                  * Data maturazione                            *
      *                  *---------------------------------------------*
           move      rf-gpm-dat-mtz       to   w-det-mep-dat-mat
                                              (w-det-mep-bum-num)     .
      *                  *---------------------------------------------*
      *                  * Numero maturazione                          *
      *                  *---------------------------------------------*
           move      rf-gpm-num-mtz       to   w-det-mep-num-mat
                                              (w-det-mep-bum-num)     .
      *                  *---------------------------------------------*
      *                  * Importo maturazione                         *
      *                  *---------------------------------------------*
           move      rf-gpm-imp-mtz       to   w-det-mep-imp-mat
                                              (w-det-mep-bum-num)     .
       det-mep-lmt-700.
      *              *-------------------------------------------------*
      *              * Riciclo a maturazione successiva, a meno che    *
      *              * non si sia saturata la capacita' del buffer     *
      *              *-------------------------------------------------*
           if        w-det-mep-bum-num    <    1000
                     go to det-mep-lmt-200.
       det-mep-lmt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-mep-lmt-999.
       det-mep-lmt-999.
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
                     w-ass-mtz-tip-ctg    =    11
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
______*    move      "N"                  to   w-ass-mtz-inf-som      .
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

