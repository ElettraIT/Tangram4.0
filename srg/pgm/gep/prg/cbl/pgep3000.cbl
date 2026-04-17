       Identification Division.
       Program-Id.                                 pgep3000           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    mov                 *
      *                                   Fase:    gep300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/01/92    *
      *                       Ultima revisione:    NdK del 10/10/16    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Movimenti per gestione portafoglio          *
      *                                                                *
      *                    Main                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                            FUNZIONI                            *
      *                                                                *
      * La fase gep300, nella sua globalita', esegue le funzioni elen- *
      * cate nella tabella seguente :                                  *
      *                                                                *
      *                                                                *
      *   Codice                                                       *
      *    tipo                                                        *
      * operazione          Descrizione Per il tipo operazione         *
      * ----------  -------------------------------------------------- *
      *                                                                *
      *             Overlay pgep300a                                   *
      *             -------------------------------------------------- *
      *   0101      Emissione Rimessa Diretta                          *
      *   0102      Emissione Incasso Elettronico                      *
      *   0103      Emissione Ri.Ba.                                   *
      *   0104      Emissione C.d.O.                                   *
      *   0105      Emissione M.Av.                                    *
      *   0106      Emissione R.I.D.                                   *
      *   0107      Emissione Bonifico Bancario                        *
      *   0108      Emissione C/C Postale                              *
      *   0109      Emissione Ricevuta Bancaria                        *
      *   0110      Emissione Tratta                                   *
      *   0111      Emissione Paghero' Cambiario                       *
      *                                                                *
      *   0161      Cessione Paghero' Cambiario da Cliente             *
      *                                                                *
      *                                                                *
      *             Overlay pgep300b                                   *
      *             -------------------------------------------------- *
      *   0200      Storno Scadenza                                    *
      *                                                                *
      *                                                                *
      *             Overlay pgep300c                                   *
      *             -------------------------------------------------- *
      *   0301      Riscossione Scadenze per Contanti                  *
      *   0302      Riscossione Scadenze con Assegno                   *
      *   0303      Riscossione Scadenze per Bonifico Bancario         *
      *   0304      Riscossione Scadenze per Bonifico in C/C Postale   *
      *   0305      Riscossione Scadenze per incasso RID ___ DA IMPL.  *
      *                                                                *
      *   0321      Pagamento Scadenze per Contanti                    *
      *   0322      Pagamento Scadenze con Assegno                     *
      *   0323      Pagamento Scadenze per Bonifico Bancario           *
      *   0324      Pagamento Scadenze per Bonifico in C/C Postale     *
      *                                                                *
      *   0350      Compensazione Scadenze                             *
      *                                                                *
      *                                                                *
      *             Overlay pgep300d                                   *
      *             -------------------------------------------------- *
      *   0401      Composizione Distinta Incassi Elettronici          *
      *   0402      Composizione Distinta Effetti                      *
      *   0403      Composizione Distinta Paghero' Cambiari            *
      *   0404      Composizione Distinta Cessioni                     *
      *                                                                *
      *                                                                *
      *             Overlay pgep300e                                   *
      *             -------------------------------------------------- *
      *   0415      Stampa Distinta per Controllo                      *
      *                                                                *
      *                                                                *
      *             Overlay pgep300f                                   *
      *             -------------------------------------------------- *
      *   0501      Presentazione Distinta SBF a Maturazione di Valuta *
      *   0502      Presentazione Distinta SBF con Accredito Immediato *
      *   0503      Presentazione Distinta al Dopo Incasso             *
      *   0504      Presentazione Distinta allo Sconto                 *
      *                                                                *
      *                                                                *
      *             Overlay pgep300g                                   *
      *             -------------------------------------------------- *
      *   0510      Preparazione Archivio di Supporto per Distinte     *
      *                                                                *
      *                                                                *
      *             Overlay pgep300h                                   *
      *             -------------------------------------------------- *
      *   0515      Stampa Distinta per la Presentazione               *
      *                                                                *
      *                                                                *
      *             Overlay pgep300i                                   *
      *             -------------------------------------------------- *
      *   0516      Stampa Effetti a fronte Distinta                   *
      *                                                                *
      *                                                                *
      *             Overlay pgep300j                                   *
      *             -------------------------------------------------- *
      *   0345      Stampa Clienti con Compensazioni da Effettuare     *
      *                                                                *
      *                                                                *
      *             Overlay pgep300l                                   *
      *             -------------------------------------------------- *
      *   0520      Riscontro Tipo Avviso a Debitori per Incassi Elet. *
      *                                                                *
      *                                                                *
      *             Overlay pgep300m                                   *
      *             -------------------------------------------------- *
      *   0540      Accettazione Distinta da parte della Banca         *
      *                                                                *
      *                                                                *
      *             Overlay pgep300n                                   *
      *             -------------------------------------------------- *
      *   0560      Accredito Distinta in C/Corrente Bancario          *
      *                                                                *
      *                                                                *
      *             Overlay pgep300p                                   *
      *             -------------------------------------------------- *
      *   0600      Insoluto su Scadenza presentata in Banca           *
      *                                                                *
      *                                                                *
      *             Overlay pgep300q                                   *
      *             -------------------------------------------------- *
      *   0620      Richiamo di Scadenza presentata in Banca           *
      *                                                                *
      *                                                                *
      *             Overlay pgep300r                                   *
      *             -------------------------------------------------- *
      *   0700      Accredito di Scadenza presentata al Dopo Incasso   *
      *                                                                *
      *                                                                *
      *             Overlay pgep300s                                   *
      *             -------------------------------------------------- *
      *   0720      Notizia di buon esito Scadenze SBF o allo Sconto   *
      *                                                                *
      *                                                                *
      *             Overlay pgep300t                                   *
      *             -------------------------------------------------- *
      *   0740      Presunto buon esito di Scadenze SBF o allo Sconto  *
      *                                                                *
      *                                                                *
      *             Overlay pgep300u                                   *
      *             -------------------------------------------------- *
      *   0741      Presunto buon esito per Gruppi di Scadenze         *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      *                    AGGIORNAMENTI CONTABILI                     *
      *                                                                *
      *             (Vedi: 'abd/not/Varie/Tangram_gep300)              *
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
      *    * Work-area per variabili di i.p.c.                         *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Variabile 'gep300-sub-pgm'                            *
      *        *-------------------------------------------------------*
           05  w-ipc-sub-pgm.
      *            *---------------------------------------------------*
      *            * Valore della variabile                            *
      *            *---------------------------------------------------*
               10  w-ipc-sub-pgm-val.
      *                *-----------------------------------------------*
      *                * Dati relativi all'identificazione             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ide.
      *                    *-------------------------------------------*
      *                    * Sistema applicativo                       *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-sap
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Area gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-arg
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Settore gestionale                        *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-set
                                          pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Fase gestionale                           *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-fas
                                          pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Sigla interna del programma               *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-pro
                                          pic  x(10)                  .
      *                    *-------------------------------------------*
      *                    * Descrizione del programma                 *
      *                    *-------------------------------------------*
                       20  w-ipc-sub-pgm-des
                                          pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indicatore                                    *
      *                *  - Spaces : Nessun significato                *
      *                *  - NA     : Il programma e' eseguito come     *
      *                *             sottoprogramma per la movimenta-  *
      *                *             zione di portafoglio senza ag-    *
      *                *             giornamenti contabili             *
      *                *-----------------------------------------------*
                   15  w-ipc-sub-pgm-ind  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di Link per programmi della fase 'pgep3000'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/pgep3000.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

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
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-tus.
      *            *---------------------------------------------------*
      *            * Da accettazione campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tus-acc-key      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi chiave                      *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-key-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su modalita' di funzionamento      *
      *        *-------------------------------------------------------*
           05  w-cnt-mfu.
      *            *---------------------------------------------------*
      *            * Visualizzazione forzata da segreteria             *
      *            *---------------------------------------------------*
               10  w-cnt-mfu-vis-sgr      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione record file [gep]             *
      *    *-----------------------------------------------------------*
       01  w-fil-gep.
      *        *-------------------------------------------------------*
      *        * Per ridefinizione tipo record 01 per personalizzazio- *
      *        * ni inerenti l'aggiornamento della contabilita' gene-  *
      *        * rale dalla gestione portafoglio                       *
      *        *-------------------------------------------------------*
           05  w-fil-gep-001.
      *            *---------------------------------------------------*
      *            * Si/No aggiornamento, parametro globale            *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-fil-gep-001-agg      pic  x(01)                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zop]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzop"                          .
      *        *-------------------------------------------------------*
      *        * [gep]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfgep"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [obp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfobp"                          .
      *        *-------------------------------------------------------*
      *        * [rsa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsa"                          .
      *        *-------------------------------------------------------*
      *        * [rsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsc"                          .
      *        *-------------------------------------------------------*
      *        * [rsd]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfrsd"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [axc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxc"                          .
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .
      *        *-------------------------------------------------------*
      *        * [fir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffir"                          .
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .
      *        *-------------------------------------------------------*
      *        * [numsdb]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/num/rec/rnnumsdb"                       .
      *        *-------------------------------------------------------*
      *        * [numddp]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/num/rec/rnnumddp"                       .
      *        *-------------------------------------------------------*
      *        * [numrsd]                                              *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/num/rec/rnnumrsd"                       .

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena rig, con buffer *
      *    * dati in grado di ospitare l'area w-rig di ogni overlay    *
      *    *-----------------------------------------------------------*
       01  w-cat-rig.
           05  w-cat-rig-ope              pic  x(02)                  .
           05  w-cat-rig-exs              pic  x(01)                  .
           05  w-cat-rig-num              pic  9(05)                  .
           05  w-cat-rig-cur              pic  9(05)                  .
           05  w-cat-rig-prg              pic  9(05)                  .
           05  w-cat-rig-max              pic  9(05)                  .
           05  w-cat-rig-app              pic  x(01)                  .
           05  w-cat-rig-ins              pic  x(01)                  .
           05  w-cat-rig-new              pic  x(01)                  .
           05  w-cat-rig-lst              pic  x(01)                  .
           05  w-cat-rig-buf.
               10  filler occurs 1024     pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo operazione per ge- *
      *    * stione portafoglio                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzop0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice nostra cassa, banca, o  *
      *    * c/c postale                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice ABI                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmnabi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice CAB                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmncab0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice circuito interbancario  *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acdeaxc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice agente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acl"                   .

      *    *===========================================================*
      *    * Link-area per aggiornamento contabilita' generale, clien- *
      *    * ti, fornitori, iva                                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per lettura Tipo operazione                      *
      *        *-------------------------------------------------------*
           05  w-let-tip-ope.
      *            *---------------------------------------------------*
      *            * Flag di lettura                                   *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice tipo operazione                            *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-cod      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Descrizione per codice tipo operazione            *
      *            *---------------------------------------------------*
               10  w-let-tip-ope-des      pic  x(50)                  .

      *    *===========================================================*
      *    * Work per subroutines di Acc                               *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Work per accettazione primo campo della chiave        *
      *        *-------------------------------------------------------*
           05  w-acc-acc-uno.
      *            *---------------------------------------------------*
      *            * Selettore primo campo da accettare                *
      *            * - D : Data registrazione                          *
      *            * - T : Tipo movimento                              *
      *            *---------------------------------------------------*
               10  w-acc-acc-uno-sel      pic  x(01)                  .

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

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di i.p.c. di tipo glo-  *
      *              * bale 'fat300-sub-pgm'                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   ipc-sub-pgm-000      thru ipc-sub-pgm-999        .
      *              *-------------------------------------------------*
      *              * Preparazione parametri di identificazione       *
      *              *-------------------------------------------------*
           perform   pre-par-ide-000      thru pre-par-ide-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma                *
      *              *-------------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione ciclo per la fase 'gep300'           *
      *              *-------------------------------------------------*
           perform   fas-gep-300-000      thru fas-gep-300-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
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
      *    * Lettura della variabile di i.p.c. 'gep300-sub-pgm'        *
      *    *-----------------------------------------------------------*
       ipc-sub-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni valore della variabile          *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-100.
      *              *-------------------------------------------------*
      *              * Lettura e cancellazione della variabile globale *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "gep300-sub-pgm"     to   s-var                  .
           move      "G"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se variabile non esistente : uscita             *
      *              *-------------------------------------------------*
           if        s-ves                not  = spaces
                     go to ipc-sub-pgm-999.
      *              *-------------------------------------------------*
      *              * Se variabile di formato inaspettato : uscita    *
      *              *-------------------------------------------------*
           if        s-tip                not  = "A" or
                     s-car                not  = 80
                     go to ipc-sub-pgm-999.
       ipc-sub-pgm-200.
      *              *-------------------------------------------------*
      *              * Valore della variabile in area di bufferizza-   *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-300.
      *              *-------------------------------------------------*
      *              * Se indicatore di tipo riconosciuto : uscita     *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     go to ipc-sub-pgm-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore della variabile          *
      *              *-------------------------------------------------*
           move      spaces               to   w-ipc-sub-pgm-val      .
       ipc-sub-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri di identificazione                 *
      *    *-----------------------------------------------------------*
       pre-par-ide-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che esista o meno la va-   *
      *              * riabile di i.p.c. 'gep300-sub-pgm'              *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     go to pre-par-ide-100
           else      go to pre-par-ide-200.
       pre-par-ide-100.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. 'gep300-sub-pgm' esi-    *
      *              * stente : preparazione area di identificazione   *
      *              * programma secondo la variabile di i.p.c.        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sistema applicativo                         *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-sap    to   i-ide-sap              .
      *                  *---------------------------------------------*
      *                  * Area gestionale                             *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-arg    to   i-ide-arg              .
      *                  *---------------------------------------------*
      *                  * Settore gestionale                          *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-set    to   i-ide-set              .
      *                  *---------------------------------------------*
      *                  * Fase gestionale                             *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-fas    to   i-ide-fas              .
      *                  *---------------------------------------------*
      *                  * Sigla interna del programma                 *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-pro    to   i-ide-pro              .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      w-ipc-sub-pgm-des    to   i-ide-des              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-par-ide-999.
       pre-par-ide-200.
      *              *-------------------------------------------------*
      *              * Se variabile di i.p.c. 'gep300-sub-pgm' non e-  *
      *              * sistente : preparazione area di identificazione *
      *              * programma in modo normale                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sistema applicativo                         *
      *                  *---------------------------------------------*
           move      "pgm"                to   i-ide-sap              .
      *                  *---------------------------------------------*
      *                  * Area gestionale                             *
      *                  *---------------------------------------------*
           move      "gep"                to   i-ide-arg              .
      *                  *---------------------------------------------*
      *                  * Settore gestionale                          *
      *                  *---------------------------------------------*
           move      "mov"                to   i-ide-set              .
      *                  *---------------------------------------------*
      *                  * Fase gestionale                             *
      *                  *---------------------------------------------*
           move      "gep300"             to   i-ide-fas              .
      *                  *---------------------------------------------*
      *                  * Sigla interna del programma                 *
      *                  *---------------------------------------------*
           move      "pgep3000"           to   i-ide-pro              .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      " MOVIMENTI PER LA GESTIONE PORTAFOGLIO  "
                                          to   i-ide-des              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pre-par-ide-999.
       pre-par-ide-999.
           exit.

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
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale delle personalizzazioni    *
      *              *-------------------------------------------------*
           perform   loa-prs-gep-000      thru loa-prs-gep-999        .
      *              *-------------------------------------------------*
      *              * Forzatura al valore 'N' della personalizzazione *
      *              * per aggiornamenti contabili in presenza della   *
      *              * variabile di i.p.c. 'gep300-sub-pgm'            *
      *              *-------------------------------------------------*
           if        w-ipc-sub-pgm-ind    =    "NA"
                     move  "N"            to   w-prs-gep-snx-cge      .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale dei valori di defaults ge- *
      *              * nerali                                          *
      *              *-------------------------------------------------*
           perform   loa-def-gen-000      thru loa-def-gen-999        .
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della tabella dei tipi o-  *
      *              * perazione                                       *
      *              *-------------------------------------------------*
           perform   loa-tbl-top-000      thru loa-tbl-top-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di accettazione                     *
      *              *-------------------------------------------------*
           perform   opn-mod-acc-000      thru opn-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Open moduli di aggiornamento                    *
      *              *-------------------------------------------------*
           perform   opn-mod-agg-000      thru opn-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Open files utilizzati dalla fase in tutte le    *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   opn-fls-fas-000      thru opn-fls-fas-999        .
      *              *-------------------------------------------------*
      *              * Determinazione della data di registrazione mi-  *
      *              * nima per i movimenti di contabilita' generale   *
      *              *-------------------------------------------------*
           perform   det-drc-min-000      thru det-drc-min-999        .
       pre-exe-pgm-800.
      *              *-------------------------------------------------*
      *              * Test se programma eseguibile                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di eventuale visualizzazione   *
      *                  *---------------------------------------------*
           if        w-cnt-mfu-vis-sgr    not  = "V"
                     go to pre-exe-pgm-999.
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
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Close moduli di accettazione                    *
      *              *-------------------------------------------------*
           perform   cls-mod-acc-000      thru cls-mod-acc-999        .
      *              *-------------------------------------------------*
      *              * Close moduli di aggiornamento                   *
      *              *-------------------------------------------------*
           perform   cls-mod-agg-000      thru cls-mod-agg-999        .
      *              *-------------------------------------------------*
      *              * Close files utilizzati dalla fase in tutte le   *
      *              * sue funzioni                                    *
      *              *-------------------------------------------------*
           perform   cls-fls-fas-000      thru cls-fls-fas-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione ciclo per la fase 'gep300'                     *
      *    *-----------------------------------------------------------*
       fas-gep-300-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di richiesta emissione di  *
      *              * una nuova scadenza a fronte dell'operazione ap- *
      *              * pena completata                                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-ric-ens          .
      *              *-------------------------------------------------*
      *              * Selettore primo campo da accettare della chia-  *
      *              * ve : data di registrazione                      *
      *              *-------------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di accettazione chiave   *
      *              *-------------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *              *-------------------------------------------------*
      *              * Preparazione valori pre-cablati per accettazio- *
      *              * ne da valori di default generali                *
      *              *-------------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts per impostazione campi  *
      *              * chiave                                          *
      *              *-------------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fas-gep-300-100.
      *              *-------------------------------------------------*
      *              * Assestamento flags di controllo per emissione   *
      *              * nuova scadenza a fronte dell'operazione appena  *
      *              * completata                                      *
      *              *-------------------------------------------------*
           if        w-key-ric-ens        =    spaces
                     move  spaces         to   w-key-flg-ens
                     move  zero           to   w-key-dtr-ens
                     move  zero           to   w-key-tns-ens
                     move  zero           to   w-key-imp-ens
                     move  spaces         to   w-key-fds-ens
                     move  zero           to   w-key-dts-ens
                     move  spaces         to   w-key-frz-ens
                     move  zero           to   w-key-nvs-ens
                     move  zero           to   w-key-tvo-ens
                     move  spaces         to   w-key-f00-ens
           else      move  spaces         to   w-key-ric-ens
                     move  "#"            to   w-key-flg-ens          .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare a Spaces per il ti- *
      *              * po uscita da impostazione campi chiave          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tus-ack          .
      *              *-------------------------------------------------*
      *              * Accettazione campi chiave                       *
      *              *-------------------------------------------------*
           perform   acc-key-reg-000      thru acc-key-reg-999        .
      *              *-------------------------------------------------*
      *              * Se tipo uscita "E" : fine programma             *
      *              *-------------------------------------------------*
           if        w-cnt-tus-acc-key    =    "E"
                     go to fas-gep-300-999.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per esecuzione del tipo *
      *              * operazione                                      *
      *              *-------------------------------------------------*
           perform   ric-spg-top-000      thru ric-spg-top-999        .
       fas-gep-300-200.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo uscita da im-   *
      *              * postazione campi chiave                         *
      *              *-------------------------------------------------*
           if        w-key-tus-ack        =    spaces
                     go to fas-gep-300-300
           else if   w-key-tus-ack        =    "U"
                     go to fas-gep-300-400
           else if   w-key-tus-ack        =    "E"
                     go to fas-gep-300-500
           else      go to fas-gep-300-300.
       fas-gep-300-300.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da impostazione campi chiave al  *
      *              * valore Spaces                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : data di registrazione              *
      *                  *---------------------------------------------*
           move      "D"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-gep-300-100.
       fas-gep-300-400.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'U'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo movimento                     *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-gep-300-100.
       fas-gep-300-500.
      *              *-------------------------------------------------*
      *              * Se tipo uscita da im postazione campi chiave al *
      *              * valore 'E'                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di accettazione      *
      *                  * chiave                                      *
      *                  *---------------------------------------------*
           perform   nor-val-key-000      thru nor-val-key-999        .
      *                  *---------------------------------------------*
      *                  * Preparazione valori pre-cablati per accet-  *
      *                  * tazione da valori di default generali       *
      *                  *---------------------------------------------*
           perform   pre-key-def-000      thru pre-key-def-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione prompts per impostazione    *
      *                  * campi chiave                                *
      *                  *---------------------------------------------*
           perform   pmt-key-reg-000      thru pmt-key-reg-999        .
      *                  *---------------------------------------------*
      *                  * Selettore primo campo da accettare della    *
      *                  * chiave : tipo movimento                     *
      *                  *---------------------------------------------*
           move      "T"                  to   w-acc-acc-uno-sel      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo ad impostazione campi chiave        *
      *                  *---------------------------------------------*
           go to     fas-gep-300-100.
       fas-gep-300-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione valori di accettazione chiave             *
      *    *-----------------------------------------------------------*
       nor-val-key-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Descrizione per codice tipo operazione          *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-ope-des      .
       nor-val-key-999.
           exit.

      *    *===========================================================*
      *    * Preparazione valori pre-cablati per accettazione campi    *
      *    * chiave, assumendo dai valori di default generali          *
      *    *-----------------------------------------------------------*
       pre-key-def-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      w-def-dat-reg        to   w-key-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           move      w-def-tip-ope        to   w-key-tip-ope          .
      *              *-------------------------------------------------*
      *              * Lettura codice tipo operazione                  *
      *              *-------------------------------------------------*
           move      w-key-tip-ope        to   w-let-tip-ope-cod      .
           perform   let-tip-ope-000      thru let-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Descrizione tipo operazione                     *
      *              *-------------------------------------------------*
           move      w-let-tip-ope-des    to   w-key-tip-ope-des      .
       pre-key-def-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per accettazione campi chiave, e  *
      *    * dei valori relativi ai campi pre-cablati per defaults     *
      *    *-----------------------------------------------------------*
       pmt-key-reg-000.
      *              *-------------------------------------------------*
      *              * Erase linee impegnate dalla chiave              *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Prompt per data di registrazione                *
      *              *-------------------------------------------------*
           perform   pmt-dat-reg-000      thru pmt-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *              *-------------------------------------------------*
      *              * Prompt per tipo operazione                      *
      *              *-------------------------------------------------*
           perform   pmt-tip-ope-000      thru pmt-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione                          *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione, descrizione             *
      *              *-------------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *              *-------------------------------------------------*
      *              * Trattini di separazione                         *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per data di registrazione                          *
      *    *-----------------------------------------------------------*
       pmt-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data registrazione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo operazione                                *
      *    *-----------------------------------------------------------*
       pmt-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo operazione    :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campi chiave                                 *
      *    *-----------------------------------------------------------*
       acc-key-reg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tus-acc-key      .
      *              *-------------------------------------------------*
      *              * Se si e' in fase di emissione nuova scadenza a  *
      *              * fronte dell'operazione appena completata, l'ac- *
      *              * cettazione inizia dalla data di registrazione   *
      *              *-------------------------------------------------*
           if        w-key-flg-ens        not  = spaces
                     go to acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Altrimenti, caso normale, l'accettazione ha i-  *
      *              * nizio a seconda del valore del Selettore primo  *
      *              * campo da accettare della chiave                 *
      *              *-------------------------------------------------*
           if        w-acc-acc-uno-sel    =    "T"
                     go to acc-key-reg-200
           else      go to acc-key-reg-100.
       acc-key-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione Data registrazione                 *
      *              *-------------------------------------------------*
           perform   acc-dat-reg-000      thru acc-dat-reg-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
       acc-key-reg-200.
      *              *-------------------------------------------------*
      *              * Accettazione Tipo operazione                    *
      *              *-------------------------------------------------*
           perform   acc-tip-ope-000      thru acc-tip-ope-999        .
           if        w-cnt-tus-acc-key    not  = spaces
                     go to acc-key-reg-999.
           if        v-key                =    "UP  "
                     go to acc-key-reg-100.
       acc-key-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Data registrazione                           *
      *    *-----------------------------------------------------------*
       acc-dat-reg-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-reg-025.
      *                  *---------------------------------------------*
      *                  * Se in fase di emissione nuova scadenza a    *
      *                  * fronte dell'operazione appena completata    *
      *                  * si forza come data di registrazione la data *
      *                  * di registrazione per emissione nuova sca-   *
      *                  * denza, la si visualizza, e si continua      *
      *                  * dopo l'accettazione stessa.                 *
      *                  * Se la data di registrazione per emissione   *
      *                  * nuova scadenza e' a zero si forza la data   *
      *                  * di sistema.                                 *
      *                  *---------------------------------------------*
       acc-dat-reg-030.
      *                      *-----------------------------------------*
      *                      * Se non in fase di emissione nuova sca-  *
      *                      * denza a fronte dell'operazione appena   *
      *                      * completata : ad accettazione            *
      *                      *-----------------------------------------*
           if        w-key-flg-ens        =    spaces
                     go to acc-dat-reg-100.
      *                      *-----------------------------------------*
      *                      * Se la data di registrazione per emis-   *
      *                      * sione nuova scadenza e' diversa da ze-  *
      *                      * ro, la si pone e si omette la lettura   *
      *                      * della data di sistema                   *
      *                      *-----------------------------------------*
           if        w-key-dtr-ens        not  = zero
                     move  w-key-dtr-ens  to   w-key-dat-reg
                     go to acc-dat-reg-040.
       acc-dat-reg-035.
      *                      *-----------------------------------------*
      *                      * Lettura data di sistema da segreteria   *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Data di sistema in data di registrazio- *
      *                      * ne                                      *
      *                      *-----------------------------------------*
           move      s-dat                to   w-key-dat-reg          .
       acc-dat-reg-040.
      *                      *-----------------------------------------*
      *                      * Visualizzazione data di registrazione   *
      *                      *-----------------------------------------*
           perform   vis-dat-reg-000      thru vis-dat-reg-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key di impo-   *
      *                      * stazione                                *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * A post accettazione                     *
      *                      *-----------------------------------------*
           go to     acc-dat-reg-400.
       acc-dat-reg-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se viene eseguita l'accettazione effettiva  *
      *                  * si pone comunque in Off il flag di emissio- *
      *                  * ne nuova scadenza a fronte dell'operazione  *
      *                  * appena completata                           *
      *                  *---------------------------------------------*
           move      spaces               to   w-key-flg-ens          .
      *                  *---------------------------------------------*
      *                  * Accettazione effettiva                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-key-dat-reg          .
       acc-dat-reg-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che la data di registrazione sia di-   *
      *                  * versa da zero                               *
      *                  *---------------------------------------------*
           if        w-key-dat-reg        =    zero
                     go to acc-dat-reg-100.
       acc-dat-reg-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-reg-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-dat-reg-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-dat-reg-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-dat-reg-999.
       acc-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Data registrazione                        *
      *    *-----------------------------------------------------------*
       vis-dat-reg-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-dat-reg        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-reg-999.
           exit.

      *    *===========================================================*
      *    * Accettazione Codice tipo operazione                       *
      *    *-----------------------------------------------------------*
       acc-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se in fase di emissione nuova scadenza a    *
      *                  * fronte dell'operazione appena completata    *
      *                  * si forza come tipo operazione quello cor-   *
      *                  * rispondente al tipo della nuova scadenza    *
      *                  * da emettere, lo si visualizza, e si conti-  *
      *                  * nua dopo l'accettazione stessa              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non in fase di emissione nuova sca-  *
      *                      * denza a fronte dell'operazione appena   *
      *                      * completata : ad accettazione            *
      *                      *-----------------------------------------*
           if        w-key-flg-ens        =    spaces
                     go to acc-tip-ope-100.
      *                      *-----------------------------------------*
      *                      * Determinazione tipo operazione, se non  *
      *                      * possibile : ad accettazione             *
      *                      *-----------------------------------------*
           if        w-key-tns-ens        =    01
                     move  0101           to   w-key-tip-ope
           else if   w-key-tns-ens        =    02
                     move  0102           to   w-key-tip-ope
           else if   w-key-tns-ens        =    03
                     move  0103           to   w-key-tip-ope
           else if   w-key-tns-ens        =    04
                     move  0104           to   w-key-tip-ope
           else if   w-key-tns-ens        =    05
                     move  0105           to   w-key-tip-ope
           else if   w-key-tns-ens        =    06
                     move  0106           to   w-key-tip-ope
           else if   w-key-tns-ens        =    07
                     move  0107           to   w-key-tip-ope
           else if   w-key-tns-ens        =    08
                     move  0108           to   w-key-tip-ope
           else if   w-key-tns-ens        =    09
                     move  0109           to   w-key-tip-ope
           else if   w-key-tns-ens        =    10
                     move  0110           to   w-key-tip-ope
           else if   w-key-tns-ens        =    11
                     move  0111           to   w-key-tip-ope
           else      go to acc-tip-ope-100.
      *                      *-----------------------------------------*
      *                      * Visualizzazione tipo operazione         *
      *                      *-----------------------------------------*
           perform   vis-tip-ope-000      thru vis-tip-ope-999        .
      *                      *-----------------------------------------*
      *                      * Normalizzazione function-key di impo-   *
      *                      * stazione                                *
      *                      *-----------------------------------------*
           move      spaces               to   v-key                  .
      *                      *-----------------------------------------*
      *                      * A post accettazione                     *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-400.
       acc-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se viene eseguita l'accettazione effettiva  *
      *                  * si pone comunque in Off il flag di emissio- *
      *                  * ne nuova scadenza a fronte dell'operazione  *
      *                  * appena completata                           *
      *                  *---------------------------------------------*
           move      spaces               to   w-key-flg-ens          .
      *                  *---------------------------------------------*
      *                  * Accettazione effettiva                      *
      *                  *---------------------------------------------*
           move      "AC"                 to   w-cod-mne-zop-ope      .
           move      w-key-tip-ope        to   w-cod-mne-zop-cod      .
           move      05                   to   w-cod-mne-zop-lin      .
           move      22                   to   w-cod-mne-zop-pos      .
           move      05                   to   w-cod-mne-zop-dln      .
           move      27                   to   w-cod-mne-zop-dps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-mne-zop-cll-000  thru cod-mne-zop-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-mne-zop-foi-000  thru cod-mne-zop-foi-999    .
       acc-tip-ope-110.
           perform   cod-mne-zop-cll-000  thru cod-mne-zop-cll-999    .
           if        w-cod-mne-zop-ope    =    "F+"
                     go to acc-tip-ope-115.
           if        w-cod-mne-zop-ope    =    "AC"
                     go to acc-tip-ope-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tip-ope-115.
           perform   cod-mne-zop-foi-000  thru cod-mne-zop-foi-999    .
           go to     acc-tip-ope-110.
       acc-tip-ope-120.
           move      w-cod-mne-zop-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-key-tip-ope          .
       acc-tip-ope-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura codice tipo operazione              *
      *                  *---------------------------------------------*
           move      w-key-tip-ope        to   w-let-tip-ope-cod      .
           perform   let-tip-ope-000      thru let-tip-ope-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione descrizione tipo operazione  *
      *                  *---------------------------------------------*
           move      w-let-tip-ope-des    to   w-key-tip-ope-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione tipo operazione *
      *                  *---------------------------------------------*
           perform   vis-tip-ope-des-000  thru vis-tip-ope-des-999    .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se tipo operazione e-  *
      *                  * sistente o non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-tip-ope-flg    =    spaces
                     go to acc-tip-ope-450.
       acc-tip-ope-425.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione non esistente            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-tip-ope-100.
       acc-tip-ope-450.
      *                  *---------------------------------------------*
      *                  * Se tipo operazione esistente                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del valore         *
      *                      *-----------------------------------------*
           if        w-key-tip-ope        =    zero
                     go to acc-tip-ope-500
           else      go to acc-tip-ope-550.
       acc-tip-ope-500.
      *                      *-----------------------------------------*
      *                      * Se valore a zero                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non si e' in Up : reimpostazione *
      *                          *-------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-tip-ope-100.
      *                          *-------------------------------------*
      *                          * Se si e' in Up : continuazione      *
      *                          *-------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-550.
      *                      *-----------------------------------------*
      *                      * Se valore diverso da zero               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A continuazione                     *
      *                          *-------------------------------------*
           go to     acc-tip-ope-600.
       acc-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-ope-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                not  = "DO  "
                     go to acc-tip-ope-999.
           perform   cnt-tdo-key-000      thru cnt-tdo-key-999        .
           if        w-cnt-tdo-key-flg    not  = spaces
                     go to acc-tip-ope-100
           else      move  "S"            to   w-cnt-tus-acc-key
                     go to acc-tip-ope-999.
       acc-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Codice tipo operazione                    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      05                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      w-key-tip-ope        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione Descrizione per codice tipo operazione    *
      *    *-----------------------------------------------------------*
       vis-tip-ope-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      w-key-tip-ope-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ope-des-999.
           exit.

      *    *===========================================================*
      *    * Lettura Tipo operazione                                   *
      *    *-----------------------------------------------------------*
       let-tip-ope-000.
      *              *-------------------------------------------------*
      *              * Test se codice tipo operazione a zero           *
      *              *-------------------------------------------------*
           if        w-let-tip-ope-cod    =    zero
                     go to let-tip-ope-600.
       let-tip-ope-100.
      *              *-------------------------------------------------*
      *              * Ricerca codice tipo operazione in tabella       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Indice di comodo a zero                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-inx          .
       let-tip-ope-200.
      *                  *---------------------------------------------*
      *                  * Incremento indice di comodo                 *
      *                  *---------------------------------------------*
           add       1                    to   w-top-ele-inx          .
      *                  *---------------------------------------------*
      *                  * Test se superato il massimo numero di ele-  *
      *                  * menti in tabella tipi operazione; se supe-  *
      *                  * rato si va ad elemento non trovato          *
      *                  *---------------------------------------------*
           if        w-top-ele-inx        >    w-top-ele-num
                     go to let-tip-ope-400.
      *                  *---------------------------------------------*
      *                  * Test se l'elemento in esame e' quello cer-  *
      *                  * cato; se non lo e' si ricicla all'elemento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        w-top-cod-top
                    (w-top-ele-inx)       not  = w-let-tip-ope-cod
                     go to let-tip-ope-200.
       let-tip-ope-300.
      *                  *---------------------------------------------*
      *                  * Se elemento trovato                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      w-top-des-top
                    (w-top-ele-inx)       to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a trovato            *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-400.
      *                  *---------------------------------------------*
      *                  * Se elemento non trovato                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      all   "."            to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a non trovato        *
      *                          *-------------------------------------*
           move      "#"                  to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-600.
      *              *-------------------------------------------------*
      *              * Se codice tipo operazione a zero                *
      *              *-------------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per tipo operazione     *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-des      .
      *                          *-------------------------------------*
      *                          * Flag di uscita a trovato            *
      *                          *-------------------------------------*
           move      spaces               to   w-let-tip-ope-flg      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-tip-ope-999.
       let-tip-ope-999.
           exit.

      *    *===========================================================*
      *    * Controllo su impostazione tasto Do                        *
      *    *-----------------------------------------------------------*
       cnt-tdo-key-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-key-flg      .
       cnt-tdo-key-100.
      *              *-------------------------------------------------*
      *              * Test su data di registrazione, che non manchi   *
      *              *-------------------------------------------------*
           if        w-key-dat-reg        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-200.
      *              *-------------------------------------------------*
      *              * Test su tipo operazione, che non manchi         *
      *              *-------------------------------------------------*
           if        w-key-tip-ope        =    zero
                     move  "#"            to   w-cnt-tdo-key-flg
                     go to cnt-tdo-key-999.
       cnt-tdo-key-999.
           exit.
           
      *    *===========================================================*
      *    * Richiamo sottoprogramma per esecuzione Tipo Operazione    *
      *    *-----------------------------------------------------------*
       ric-spg-top-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0101                    *
      *                  *---------------------------------------------*
           if        w-key-tip-ope        =    0101
                     perform   ric-000-101-000
                                          thru ric-000-101-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0102                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0102
                     perform   ric-000-102-000
                                          thru ric-000-102-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0103                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0103
                     perform   ric-000-103-000
                                          thru ric-000-103-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0104                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0104
                     perform   ric-000-104-000
                                          thru ric-000-104-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0105                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0105
                     perform   ric-000-105-000
                                          thru ric-000-105-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0106                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0106
                     perform   ric-000-106-000
                                          thru ric-000-106-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0107                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0107
                     perform   ric-000-107-000
                                          thru ric-000-107-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0108                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0108
                     perform   ric-000-108-000
                                          thru ric-000-108-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0109                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0109
                     perform   ric-000-109-000
                                          thru ric-000-109-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0110                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0110
                     perform   ric-000-110-000
                                          thru ric-000-110-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0111                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0111
                     perform   ric-000-111-000
                                          thru ric-000-111-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0161                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0161
                     perform   ric-000-161-000
                                          thru ric-000-161-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0200                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0200
                     perform   ric-000-200-000
                                          thru ric-000-200-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0301                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0301
                     perform   ric-000-301-000
                                          thru ric-000-301-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0302                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0302
                     perform   ric-000-302-000
                                          thru ric-000-302-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0303                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0303
                     perform   ric-000-303-000
                                          thru ric-000-303-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0304                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0304
                     perform   ric-000-304-000
                                          thru ric-000-304-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0321                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0321
                     perform   ric-000-321-000
                                          thru ric-000-321-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0322                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0322
                     perform   ric-000-322-000
                                          thru ric-000-322-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0323                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0323
                     perform   ric-000-323-000
                                          thru ric-000-323-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0324                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0324
                     perform   ric-000-324-000
                                          thru ric-000-324-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0345                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0345
                     perform   ric-000-345-000
                                          thru ric-000-345-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0350                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0350
                     perform   ric-000-350-000
                                          thru ric-000-350-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0401                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0401
                     perform   ric-000-401-000
                                          thru ric-000-401-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0402                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0402
                     perform   ric-000-402-000
                                          thru ric-000-402-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0403                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0403
                     perform   ric-000-403-000
                                          thru ric-000-403-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0404                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0404
                     perform   ric-000-404-000
                                          thru ric-000-404-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0415                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0415
                     perform   ric-000-415-000
                                          thru ric-000-415-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0501                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0501
                     perform   ric-000-501-000
                                          thru ric-000-501-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0502                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0502
                     perform   ric-000-502-000
                                          thru ric-000-502-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0503                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0503
                     perform   ric-000-503-000
                                          thru ric-000-503-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0504                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0504
                     perform   ric-000-504-000
                                          thru ric-000-504-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0510                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0510
                     perform   ric-000-510-000
                                          thru ric-000-510-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0515                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0515
                     perform   ric-000-515-000
                                          thru ric-000-515-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0516                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0516
                     perform   ric-000-516-000
                                          thru ric-000-516-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0520                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0520
                     perform   ric-000-520-000
                                          thru ric-000-520-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0540                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0540
                     perform   ric-000-540-000
                                          thru ric-000-540-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0560                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0560
                     perform   ric-000-560-000
                                          thru ric-000-560-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0600                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0600
                     perform   ric-000-600-000
                                          thru ric-000-600-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0620                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0620
                     perform   ric-000-620-000
                                          thru ric-000-620-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0700                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0700
                     perform   ric-000-700-000
                                          thru ric-000-700-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0720                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0720
                     perform   ric-000-720-000
                                          thru ric-000-720-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0740                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0740
                     perform   ric-000-740-000
                                          thru ric-000-740-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  0741                    *
      *                  *---------------------------------------------*
           else if   w-key-tip-ope        =    0741
                     perform   ric-000-741-000
                                          thru ric-000-741-999
      *                  *---------------------------------------------*
      *                  * Se tipo operazione  non riconosciuto        *
      *                  *---------------------------------------------*
           else      go to ric-spg-top-999.
       ric-spg-top-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0101         *
      *    *-----------------------------------------------------------*
       ric-000-101-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-101-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0102         *
      *    *-----------------------------------------------------------*
       ric-000-102-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-102-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0103         *
      *    *-----------------------------------------------------------*
       ric-000-103-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-103-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0104         *
      *    *-----------------------------------------------------------*
       ric-000-104-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-104-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0105         *
      *    *-----------------------------------------------------------*
       ric-000-105-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-105-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0106         *
      *    *-----------------------------------------------------------*
       ric-000-106-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-106-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0107         *
      *    *-----------------------------------------------------------*
       ric-000-107-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-107-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0108         *
      *    *-----------------------------------------------------------*
       ric-000-108-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-108-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0109         *
      *    *-----------------------------------------------------------*
       ric-000-109-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-109-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0110         *
      *    *-----------------------------------------------------------*
       ric-000-110-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-110-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0111         *
      *    *-----------------------------------------------------------*
       ric-000-111-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-111-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0161         *
      *    *-----------------------------------------------------------*
       ric-000-161-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300a"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-161-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0200         *
      *    *-----------------------------------------------------------*
       ric-000-200-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300b"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300b"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-200-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0301         *
      *    *-----------------------------------------------------------*
       ric-000-301-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-301-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0302         *
      *    *-----------------------------------------------------------*
       ric-000-302-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-302-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0303         *
      *    *-----------------------------------------------------------*
       ric-000-303-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-303-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0304         *
      *    *-----------------------------------------------------------*
       ric-000-304-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-304-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0321         *
      *    *-----------------------------------------------------------*
       ric-000-321-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-321-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0322         *
      *    *-----------------------------------------------------------*
       ric-000-322-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-322-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0323         *
      *    *-----------------------------------------------------------*
       ric-000-323-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-323-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0324         *
      *    *-----------------------------------------------------------*
       ric-000-324-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-324-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0345         *
      *    *-----------------------------------------------------------*
       ric-000-345-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300j"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300j"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-345-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0350         *
      *    *-----------------------------------------------------------*
       ric-000-350-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300c"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-350-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0401         *
      *    *-----------------------------------------------------------*
       ric-000-401-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-401-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0402         *
      *    *-----------------------------------------------------------*
       ric-000-402-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-402-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0403         *
      *    *-----------------------------------------------------------*
       ric-000-403-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-403-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0404         *
      *    *-----------------------------------------------------------*
       ric-000-404-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300d"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-404-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0415         *
      *    *-----------------------------------------------------------*
       ric-000-415-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300e"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300e"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-415-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0501         *
      *    *-----------------------------------------------------------*
       ric-000-501-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-501-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0502         *
      *    *-----------------------------------------------------------*
       ric-000-502-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-502-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0503         *
      *    *-----------------------------------------------------------*
       ric-000-503-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-503-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0504         *
      *    *-----------------------------------------------------------*
       ric-000-504-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300f"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-504-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0510         *
      *    *-----------------------------------------------------------*
       ric-000-510-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300g"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300g"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-510-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0515         *
      *    *-----------------------------------------------------------*
       ric-000-515-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300h"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300h"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-515-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0516         *
      *    *-----------------------------------------------------------*
       ric-000-516-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300i"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300i"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-516-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0520         *
      *    *-----------------------------------------------------------*
       ric-000-520-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300l"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300l"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-520-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0540         *
      *    *-----------------------------------------------------------*
       ric-000-540-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300m"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300m"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-540-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0560         *
      *    *-----------------------------------------------------------*
       ric-000-560-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300n"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300n"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-560-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0600         *
      *    *-----------------------------------------------------------*
       ric-000-600-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300p"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300p"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-600-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0620         *
      *    *-----------------------------------------------------------*
       ric-000-620-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300q"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300q"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-620-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0700         *
      *    *-----------------------------------------------------------*
       ric-000-700-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300r"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300r"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-700-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0720         *
      *    *-----------------------------------------------------------*
       ric-000-720-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300s"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-720-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0740         *
      *    *-----------------------------------------------------------*
       ric-000-740-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300t"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300t"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-740-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per tipo operazione  0741         *
      *    *-----------------------------------------------------------*
       ric-000-741-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma di esecuzione           *
      *              *-------------------------------------------------*
           move      spaces               to   w-key-tip-fun          .
           move      "pgm/gep/prg/obj/pgep300u"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using i-ide
                                               w-prs
                                               w-top
                                               w-def
                                               w-key                  .
      *              *-------------------------------------------------*
      *              * Cancellazione sottoprogramma di esecuzione      *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep300u"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       ric-000-741-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale delle personalizzazioni              *
      *    *-----------------------------------------------------------*
       loa-prs-gep-000.
      *              *-------------------------------------------------*
      *              * Caricamento iniziale della personalizzazione    *
      *              * per si/no aggiornamenti contabili per movimenti *
      *              * di portafoglio                                  *
      *              *-------------------------------------------------*
           perform   snx-agg-cge-000      thru snx-agg-cge-999        .
      *              *-------------------------------------------------*
      *              * Modalita' per la riemissione di una nuova sca-  *
      *              * denza a fronte di storno, insoluto, ecc.        *
      *              *-------------------------------------------------*
           perform   mod-per-rie-000      thru mod-per-rie-999        .
      *              *-------------------------------------------------*
      *              * Si/No ammissibilita' di avere la data scadenza  *
      *              * inferiore alla data registrazione e all'eventu- *
      *              * ale data documento                              *
      *              *-------------------------------------------------*
           perform   snx-dts-inf-000      thru snx-dts-inf-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per il tipo di file   *
      *              * da preparare per il supporto magnetico          *
      *              *-------------------------------------------------*
           perform   prs-tip-fsm-000      thru prs-tip-fsm-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per l'emissione dei   *
      *              * riferimenti a CIG e CUP in distinta             *
      *              *-------------------------------------------------*
           perform   prs-cig-cup-000      thru prs-cig-cup-999        .
       loa-prs-gep-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della personalizzazione per si/no    *
      *    * aggiornamenti contabili per movimenti di portafoglio      *
      *    *-----------------------------------------------------------*
       snx-agg-cge-000.
      *              *-------------------------------------------------*
      *              * Open file [gep]                                 *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       snx-agg-cge-100.
      *              *-------------------------------------------------*
      *              * Lettura record personalizzazioni                *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODGEP    "         to   f-key                  .
           move      01                   to   rf-gep-tip-gep         .
           move      "CGE       "         to   rf-gep-cod-gep         .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
      *              *-------------------------------------------------*
      *              * Deviazione secondo l'esito della lettura        *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to snx-agg-cge-200
           else      go to snx-agg-cge-400.
       snx-agg-cge-200.
      *              *-------------------------------------------------*
      *              * Se record personalizzazioni esistente           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dati record in area per ridefinizione delle *
      *                  * personalizzazioni portafoglio per tipo re-  *
      *                  * cord 01                                     *
      *                  *---------------------------------------------*
           move      rf-gep-dat-rec       to   w-fil-gep-001          .
      *                  *---------------------------------------------*
      *                  * A bufferizzazione personalizzazione         *
      *                  *---------------------------------------------*
           go to     snx-agg-cge-600.
       snx-agg-cge-400.
      *              *-------------------------------------------------*
      *              * Se record personalizzazioni non esistente       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura per : Si aggiornamento            *
      *                  *---------------------------------------------*
           move      "S"                  to   w-fil-gep-001-agg      .
      *                  *---------------------------------------------*
      *                  * A bufferizzazione personalizzazione         *
      *                  *---------------------------------------------*
           go to     snx-agg-cge-600.
       snx-agg-cge-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione personalizzazione ed eventuale  *
      *              * normalizzazione del valore                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      w-fil-gep-001-agg    to   w-prs-gep-snx-cge      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           if        w-prs-gep-snx-cge    not  = "N"
                     move  "S"            to   w-prs-gep-snx-cge      .
       snx-agg-cge-900.
      *              *-------------------------------------------------*
      *              * Close file [gep]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
       snx-agg-cge-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della personalizzazione relativa al- *
      *    * le modalita' per la riemissione di una nuova scadenza a   *
      *    * fronte di storno, insoluto, ecc.                          *
      *    *-----------------------------------------------------------*
       mod-per-rie-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep/mov/gep300[mod-rns]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       mod-per-rie-100.
      *              *-------------------------------------------------*
      *              * Memorizzazione dei valori                       *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rie
           else      move  spaces         to   w-prs-rie              .
       mod-per-rie-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione dei valori                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Modalita' di riemissione                    *
      *                  *---------------------------------------------*
           if        w-prs-rie-mod-rie    not  numeric
                     move  zero           to   w-prs-rie-mod-rie      .
           if        w-prs-rie-mod-rie    not  = zero and
                     w-prs-rie-mod-rie    not  = 01   and
                     w-prs-rie-mod-rie    not  = 02   and
                     w-prs-rie-mod-rie    not  = 03   and
                     w-prs-rie-mod-rie    not  = 04   and
                     w-prs-rie-mod-rie    not  = 99
                     move  zero           to   w-prs-rie-mod-rie      .
      *                  *---------------------------------------------*
      *                  * Tipo di scadenza da riemettere              *
      *                  *---------------------------------------------*
           if        w-prs-rie-tip-sca    not  numeric
                     move  zero           to   w-prs-rie-tip-sca      .
           if        w-prs-rie-tip-sca    <    01 or
                     w-prs-rie-tip-sca    >    11
                     move  zero           to   w-prs-rie-tip-sca      .
      *                  *---------------------------------------------*
      *                  * Indicazione sulla data di scadenza          *
      *                  *---------------------------------------------*
           if        w-prs-rie-ind-sds    not  = "AV" and
                     w-prs-rie-ind-sds    not  = "FM" and
                     w-prs-rie-ind-sds    not  = "SD" and
                     w-prs-rie-ind-sds    not  = "+1"
                     move  spaces         to   w-prs-rie-ind-sds      .
      *                  *---------------------------------------------*
      *                  * Tipo di ripercussione su anagrafica commer- *
      *                  * ciale del cliente                           *
      *                  *---------------------------------------------*
           if        w-prs-rie-snx-blc    not  numeric
                     move  zero           to   w-prs-rie-snx-blc      .
           if        w-prs-rie-snx-blc    not  = zero and
                     w-prs-rie-snx-blc    not  = 01   and
                     w-prs-rie-snx-blc    not  = 02   and
                     w-prs-rie-snx-blc    not  = 03
                     move  zero           to   w-prs-rie-snx-blc      .
       mod-per-rie-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione dei valori                      *
      *              *-------------------------------------------------*
       mod-per-rie-310.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della modalita' di     *
      *                  * riemissione                                 *
      *                  *---------------------------------------------*
           if        w-prs-rie-mod-rie    =    zero
                     go to mod-per-rie-400
           else if   w-prs-rie-mod-rie    =    01
                     go to mod-per-rie-410
           else if   w-prs-rie-mod-rie    =    02
                     go to mod-per-rie-420
           else if   w-prs-rie-mod-rie    =    03
                     go to mod-per-rie-430
           else if   w-prs-rie-mod-rie    =    04
                     go to mod-per-rie-440
           else if   w-prs-rie-mod-rie    =    99
                     go to mod-per-rie-490.
       mod-per-rie-400.
      *                  *---------------------------------------------*
      *                  * Se tipo 00                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-prs-rie-ind-sds      .
           go to     mod-per-rie-900.
       mod-per-rie-410.
      *                  *---------------------------------------------*
      *                  * Se tipo 01                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-prs-rie-ind-sds      .
           go to     mod-per-rie-900.
       mod-per-rie-420.
      *                  *---------------------------------------------*
      *                  * Se tipo 02                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-prs-rie-ind-sds      .
           go to     mod-per-rie-900.
       mod-per-rie-430.
      *                  *---------------------------------------------*
      *                  * Se tipo 03                                  *
      *                  *---------------------------------------------*
           if        w-prs-rie-tip-sca    =    zero
                     move  02             to   w-prs-rie-mod-rie
                     go to mod-per-rie-300.
           move      spaces               to   w-prs-rie-ind-sds      .
           go to     mod-per-rie-900.
       mod-per-rie-440.
      *                  *---------------------------------------------*
      *                  * Se tipo 04                                  *
      *                  *---------------------------------------------*
           if        w-prs-rie-ind-sds    =    spaces
                     move  03             to   w-prs-rie-mod-rie
                     go to mod-per-rie-300.
           go to     mod-per-rie-900.
       mod-per-rie-490.
      *                  *---------------------------------------------*
      *                  * Se tipo 99                                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-rie-tip-sca      .
           move      spaces               to   w-prs-rie-ind-sds      .
           go to     mod-per-rie-900.
       mod-per-rie-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     mod-per-rie-999.
       mod-per-rie-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della personalizzazione relativa al- *
      *    * la ammissibilita' di avere la data scadenza inferiore al- *
      *    * la data registrazione e all'eventuale data documento      *
      *    *-----------------------------------------------------------*
       snx-dts-inf-000.
      *              *-------------------------------------------------*
      *              * Lettura della personalizzazione                 *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep/mov/gep300[snx-dsi]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       snx-dts-inf-100.
      *              *-------------------------------------------------*
      *              * Memorizzazione dei valori                       *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-dsi
           else      move  spaces         to   w-prs-dsi              .
       snx-dts-inf-200.
      *              *-------------------------------------------------*
      *              * Regolarizzazione dei valori                     *
      *              *-------------------------------------------------*
       snx-dts-inf-210.
      *                  *---------------------------------------------*
      *                  * Rimesse dirette                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (01)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (01)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (01) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (01)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (01)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (01) .
       snx-dts-inf-220.
      *                  *---------------------------------------------*
      *                  * Incassi elettronici                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (02)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (02)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (02) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (02)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (02)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (02) .
       snx-dts-inf-230.
      *                  *---------------------------------------------*
      *                  * Ri.Ba.                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (03)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (03)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (03) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (03)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (03)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (03) .
       snx-dts-inf-240.
      *                  *---------------------------------------------*
      *                  * C.d.O.                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (04)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (04)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (04) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (04)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (04)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (04) .
       snx-dts-inf-250.
      *                  *---------------------------------------------*
      *                  * M.Av.                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (05)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (05)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (05) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (05)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (05)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (05) .
       snx-dts-inf-260.
      *                  *---------------------------------------------*
      *                  * R.I.D.                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (06)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (06)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (06) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (06)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (06)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (06) .
       snx-dts-inf-270.
      *                  *---------------------------------------------*
      *                  * Bonifici bancari                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (07)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (07)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (07) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (07)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (07)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (07) .
       snx-dts-inf-280.
      *                  *---------------------------------------------*
      *                  * C/Correnti postali                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (08)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (08)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (08) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (08)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (08)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (08) .
       snx-dts-inf-290.
      *                  *---------------------------------------------*
      *                  * Ricevute bancarie                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (09)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (09)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (09) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (09)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (09)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (09) .
       snx-dts-inf-300.
      *                  *---------------------------------------------*
      *                  * Tratte                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (10)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (10)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (10) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (10)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (10)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (10) .
       snx-dts-inf-310.
      *                  *---------------------------------------------*
      *                  * Paghero'                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-reg (11)
                                          not  = "N" and
                     w-prs-dsi-dat-reg (11)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-reg (11) .
      *                      *-----------------------------------------*
      *                      * Inferiore alla data di registrazione    *
      *                      *-----------------------------------------*
           if        w-prs-dsi-dat-doc (11)
                                          not  = "N" and
                     w-prs-dsi-dat-doc (11)
                                          not  = "S"
                     move  "T"            to   w-prs-dsi-dat-doc (11) .
       snx-dts-inf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     snx-dts-inf-999.
       snx-dts-inf-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale dei valori di defaults generali      *
      *    *-----------------------------------------------------------*
       loa-def-gen-000.
      *              *-------------------------------------------------*
      *              * Data di registrazione                           *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-def-dat-reg          .
      *              *-------------------------------------------------*
      *              * Codice tipo operazione di default               *
      *              *-------------------------------------------------*
           move      zero                 to   w-def-tip-ope          .
       loa-def-gen-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per il tipo di file da prepara- *
      *    * re per il supporto magnetico                              *
      *    *-----------------------------------------------------------*
       prs-tip-fsm-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep/mov/gep300g[tip-fsm]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-tip-fsm
           else      move  spaces         to   w-prs-tip-fsm          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-tip-fsm        not   = "U" and
                     w-prs-tip-fsm        not   = "T"
                     move  "M"            to   w-prs-tip-fsm          .
       prs-tip-fsm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per l'emissione dei riferimenti *
      *    * a CIG e CUP in distinta                                   *
      *    *-----------------------------------------------------------*
       prs-cig-cup-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/gep/mov/gep300g[cig-cup]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-cig-cup
           else      move  spaces         to   w-prs-cig-cup          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-cig-cup        not   = "S"
                     move  "N"            to   w-prs-cig-cup          .
       prs-cig-cup-999.
           exit.

      *    *===========================================================*
      *    * Caricamento iniziale della tabella dei tipi operazione    *
      *    *-----------------------------------------------------------*
       loa-tbl-top-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale della tabella          *
      *              *-------------------------------------------------*
       loa-tbl-top-010.
      *                  *---------------------------------------------*
      *                  * Massimo numero di elementi in tabella       *
      *                  * ti in tabella                               *
      *                  *---------------------------------------------*
           move      50                   to   w-top-ele-max          .
       loa-tbl-top-015.
      *                  *---------------------------------------------*
      *                  * Normalizzazione numero effettivo di elemen- *
      *                  * ti in tabella                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-num          .
       loa-tbl-top-020.
      *                  *---------------------------------------------*
      *                  * Normalizzazione degli elementi in tabella   *
      *                  *---------------------------------------------*
           move      zero                 to   w-top-ele-inx          .
       loa-tbl-top-022.
           add       1                    to   w-top-ele-inx          .
           if        w-top-ele-inx        >    w-top-ele-max
                     go to loa-tbl-top-030.
           move      zero                 to   w-top-cod-top
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-cod-mne
                                              (w-top-ele-inx)         .
           move      spaces               to   w-top-des-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-cau-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-stc-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-ctp-cge
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f01-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f02-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f03-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f04-top
                                              (w-top-ele-inx)         .
           move      zero                 to   w-top-f05-top
                                              (w-top-ele-inx)         .
           go to     loa-tbl-top-022.
       loa-tbl-top-030.
      *                  *---------------------------------------------*
      *                  * Fine normalizzazione tabella                *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-100.
       loa-tbl-top-100.
      *              *-------------------------------------------------*
      *              * Caricamento della tabella                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura [zop]                              *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *                  *---------------------------------------------*
      *                  * Start su [zop]                              *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODTOP    "         to   f-key                  .
           move      zero                 to   rf-zop-cod-top         .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : fine lettura              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-tbl-top-180.
       loa-tbl-top-120.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale [zop]                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : fine lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to loa-tbl-top-180.
       loa-tbl-top-140.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento in tabella         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero elementi memorizzati  *
      *                      *-----------------------------------------*
           add       1                    to   w-top-ele-num          .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati associati al tipo  *
      *                      * operazione                              *
      *                      *-----------------------------------------*
           move      rf-zop-cod-top       to   w-top-cod-top
                                              (w-top-ele-num)         .
           move      rf-zop-cod-mne       to   w-top-cod-mne
                                              (w-top-ele-num)         .
           move      rf-zop-des-top       to   w-top-des-top
                                              (w-top-ele-num)         .
           move      rf-zop-cau-cge       to   w-top-cau-cge
                                              (w-top-ele-num)         .
           move      rf-zop-stc-cge       to   w-top-stc-cge
                                              (w-top-ele-num)         .
           move      rf-zop-ctp-cge       to   w-top-ctp-cge
                                              (w-top-ele-num)         .
           move      rf-zop-f01-top       to   w-top-f01-top
                                              (w-top-ele-num)         .
           move      rf-zop-f02-top       to   w-top-f02-top
                                              (w-top-ele-num)         .
           move      rf-zop-f03-top       to   w-top-f03-top
                                              (w-top-ele-num)         .
           move      rf-zop-f04-top       to   w-top-f04-top
                                              (w-top-ele-num)         .
           move      rf-zop-f05-top       to   w-top-f05-top
                                              (w-top-ele-num)         .
       loa-tbl-top-160.
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura elemento successivo       *
      *                  *---------------------------------------------*
           go to     loa-tbl-top-120.
       loa-tbl-top-180.
      *                  *---------------------------------------------*
      *                  * Chiusura [zop]                              *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzop"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zop                 .
       loa-tbl-top-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di accettazione                               *
      *    *-----------------------------------------------------------*
       opn-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo operazione per    *
      *              * gestione portafoglio                            *
      *              *-------------------------------------------------*
           perform   cod-mne-zop-opn-000  thru cod-mne-zop-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice nostra cassa,   *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-opn-000  thru cod-des-cbp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice ABI             *
      *              *-------------------------------------------------*
           perform   cod-mne-abi-opn-000  thru cod-mne-abi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice CAB             *
      *              *-------------------------------------------------*
           perform   cod-mne-cab-opn-000  thru cod-mne-cab-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice circuito in-    *
      *              * terbancario                                     *
      *              *-------------------------------------------------*
           perform   cod-des-axc-opn-000  thru cod-des-axc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice agente          *
      *              *-------------------------------------------------*
           perform   cod-mne-age-opn-000  thru cod-mne-age-opn-999    .
       opn-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di accettazione                              *
      *    *-----------------------------------------------------------*
       cls-mod-acc-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo operazione per   *
      *              * gestione portafoglio                            *
      *              *-------------------------------------------------*
           perform   cod-mne-zop-cls-000  thru cod-mne-zop-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice nostra cassa,  *
      *              * banca, o c/c postale                            *
      *              *-------------------------------------------------*
           perform   cod-des-cbp-cls-000  thru cod-des-cbp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice ABI            *
      *              *-------------------------------------------------*
           perform   cod-mne-abi-cls-000  thru cod-mne-abi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice CAB            *
      *              *-------------------------------------------------*
           perform   cod-mne-cab-cls-000  thru cod-mne-cab-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice circuito in-   *
      *              * terbancario                                     *
      *              *-------------------------------------------------*
           perform   cod-des-axc-cls-000  thru cod-des-axc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice agente         *
      *              *-------------------------------------------------*
           perform   cod-mne-age-cls-000  thru cod-mne-age-cls-999    .
       cls-mod-acc-999.
           exit.

      *    *===========================================================*
      *    * Open moduli di aggiornamento                              *
      *    *-----------------------------------------------------------*
       opn-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Open modulo aggiornamento contabilita' genera-  *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-opn-000  thru mdl-agg-cge-opn-999    .
       opn-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Close moduli di aggiornamento                             *
      *    *-----------------------------------------------------------*
       cls-mod-agg-000.
      *              *-------------------------------------------------*
      *              * Close modulo aggiornamento contabilita' genera- *
      *              * le, clienti, fornitori, iva                     *
      *              *-------------------------------------------------*
           perform   mdl-agg-cge-cls-000  thru mdl-agg-cge-cls-999    .
       cls-mod-agg-999.
           exit.

      *    *===========================================================*
      *    * Open files utilizzati dalla fase in tutte le sue funzioni *
      *    *-----------------------------------------------------------*
       opn-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Open sottoprogramma gestione catena righe       *
      *              *-------------------------------------------------*
           move      "OP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * [gep]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
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
      *              * [rsa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsa                 .
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
      *              * [rsd]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *              *-------------------------------------------------*
      *              * [obp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
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
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [axc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axc                 .
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
      *              * Open numerazione [numsdb]                       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-sdb             .
      *              *-------------------------------------------------*
      *              * Open numerazione [numddp]                       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-ddp             .
      *              *-------------------------------------------------*
      *              * Open numerazione [numrsd]                       *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-rsd             .
       opn-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Close files utilizzati dalla fase in tutte le sue fun-    *
      *    * zioni                                                     *
      *    *-----------------------------------------------------------*
       cls-fls-fas-000.
      *              *-------------------------------------------------*
      *              * Close sottoprogramma gestione catena righe con  *
      *              * cancellazione del sottoprogramma stesso         *
      *              *-------------------------------------------------*
           move      "CL"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
           perform   cnc-sub-cat-000      thru cnc-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Close file [gep]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofgep"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gep                 .
      *              *-------------------------------------------------*
      *              * Close file [sdb]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * Close file [obp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofobp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-obp                 .
      *              *-------------------------------------------------*
      *              * [rsa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsa                 .
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
      *              * [rsd]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-rsd                 .
      *              *-------------------------------------------------*
      *              * Close file [ddp]                                *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
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
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [axc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axc                 .
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
      *              * Close numerazione [numsdb]                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-sdb             .
      *              *-------------------------------------------------*
      *              * Close numerazione [numddp]                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-ddp             .
      *              *-------------------------------------------------*
      *              * Close numerazione [numrsd]                      *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/num/ioc/obj/innumrsd"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rn-num-rsd             .
       cls-fls-fas-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           move      "pgm/gep/prg/obj/pgep3002"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione sottoprogramma per gestione catena righe    *
      *    *-----------------------------------------------------------*
       cnc-sub-cat-000.
           move      "pgm/gep/prg/obj/pgep3002"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       cnc-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Determinazione della data di registrazione minima per i   *
      *    * movimenti di contabilita' generale                        *
      *    *-----------------------------------------------------------*
       det-drc-min-000.
      *              *-------------------------------------------------*
      *              * Richiamo sottoprogramma per aggiornamenti con-  *
      *              * tabili per la determinazione della data di re-  *
      *              * gistrazione minima per la contabilita' genera-  *
      *              * le, clienti, fornitori, ma non iva              *
      *              *-------------------------------------------------*
           move      "DN"                 to   l-cge-300-tip-ope      .
           perform   mdl-agg-cge-cll-000  thru mdl-agg-cge-cll-999    .
      *              *-------------------------------------------------*
      *              * Memorizzazione della data di registrazione mi-  *
      *              * nima appena determinata                         *
      *              *-------------------------------------------------*
           move      l-cge-300-dat-reg    to   w-prs-gep-drc-min      .
       det-drc-min-999.
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
      *    * Subroutines per l'accettazione del codice operazione      *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acmnzop0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice nostra cassa, o *
      *    * nostra banca, o nostro c/c postale                        *
      *    *-----------------------------------------------------------*
           copy      "pgm/gep/prg/cpy/acdecbp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice ABI             *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmnabi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice C.A.B.              *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acmncab0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice circuito inter- *
      *    * bancario                                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/abi/prg/cpy/acdeaxc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del codice agente          *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/acmnage0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per modulo aggiornamento contabilita' genera- *
      *    * le, clienti, fornitori, iva                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/pcge300z.pgs"                   .
